%%==============================================================================
%% Copyright 2013-2015 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%%   SPU0 compiler.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2015, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(spu0_compiler).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% API
-export([compile/1, compile/2]).

%% Includes
-include_lib("spu/src/spu0_scan.hrl").
-include_lib("spu/src/spu0_parse.hrl").
-include_lib("spu/src/spu0.hrl").

%% Defines

%% Records
-record(parts, {attributes = []         :: [#attribute_p{}],
                funcs      = dict:new() :: dict:dict(),
                exports    = []         :: [{atom(), integer()}]
               }).

-record(lifting, {no      = 0 :: integer(),
                  defs    = dict:new() :: dict:dict(),
                  bound   = [] :: [integer()],
                  binding = [] :: [integer()],
                  errors  = [] :: [_]
                 }).

-record(lifted, {attributes          :: [#attribute_p{}],
                 funcs  = dict:new() :: dict:dict(),
                 exports             :: [{atom(), integer()}],
                 errors = []         :: [_]
                }).

-record(opts, {dest_name :: string(),
               src_dir = "." :: string(),
               dest_dir = "." :: string()
              }).

-record(var_c, {line :: integer(),
                name :: atom(),
                type :: use | bind,
                no   :: integer()
             }).

-record(anon_c, {line :: integer(),
                 name :: atom()
                }).

-record(clause_c, {line           :: integer(),
                   name  = 'case' :: atom(),
                   args           :: [_],
                   guard          :: [_],
                   body           :: [_],
                   defs           :: dict:dict()
                  }).

-record(func_c, {line         :: integer(),
                 name = 'fun' :: atom(),
                 arity        :: integer(),
                 clauses      :: [#clause_c{}]
                }).

%% Types
-type opt() :: {dest_name, string()} | {src_dir, string()} |
               {dest_dir, string()}.

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: compile(FileName) -> ok | error.
%% @doc
%%   Compiles a .spu0 file.
%% @end
%%--------------------------------------------------------------------
-spec compile(string()) -> ok | {error, _}.
%%--------------------------------------------------------------------
compile(File) -> compile(File, []).

%%--------------------------------------------------------------------
%% Function: compile(FileName, Options) -> ok | error.
%% @doc
%%   Compiles a .spu0 file.
%% @end
%%--------------------------------------------------------------------
-spec compile(atom() | string(), [opt()]) -> ok | error.
%%--------------------------------------------------------------------
compile(Atom, Opts) when is_atom(Atom) -> compile(atom_to_list(Atom), Opts);
compile(File, Opts) ->
    OptsRec =
        case parse_opts(Opts, #opts{}) of
            OptsRec0 = #opts{dest_name = undefined} ->
                Dest = filename:basename(File, filename:extension(File)),
                OptsRec0#opts{dest_name = Dest};
            OptsRec0 ->
                OptsRec0
        end,
    do_compile(File, OptsRec).

%% ===================================================================
%% Internal functions.
%% ===================================================================

do_compile(File, Opts) ->
    chain({ok, File}, Opts,
          [fun read_file/2,
           fun scan/2,
           fun parse/2,
           fun part/2,
           fun lift/2,
           fun match/2
          ]).

chain(Result, _, []) -> Result;
chain({ok, Previous}, Opts, [Fun | T]) -> chain(Fun(Previous, Opts), Opts, T);
chain(Error, _, _) -> {error, Error}.

%% ===================================================================
%% Read file
%% ===================================================================

read_file(File, #opts{src_dir = Dir}) ->
    FileName = case filename:extension(File) of
                   [] -> filename:join(Dir, File ++ ".spu0");
                   ".spu0" -> filename:join(Dir, File)
               end,
    file:read_file(FileName).

%% ===================================================================
%%  Scan
%% ===================================================================

scan(Bin, _) ->
    case spu0_scan:string(binary_to_list(Bin)) of
        {ok, Tokens, _} -> {ok, Tokens};
        Error -> Error
    end.

%% ===================================================================
%% Parse
%% ===================================================================

parse(Tokens, Options) -> parse(Tokens, [], Options).

parse([], Forms, _) -> {ok, lists:reverse(Forms)};
parse(Tokens, Forms, Options) ->
    {FormTokens, Tokens1} = spu0_parse:next_form(Tokens),
    case spu0_parse:parse_form(FormTokens) of
        {ok, Form} -> parse(Tokens1, [Form | Forms], Options);
        Error -> {error, Error}
    end.

%% ===================================================================
%% Part
%% ===================================================================
part(Forms, Options) -> part(Forms, #parts{}, Options).

part([], Parts, _) -> {ok, Parts};
part([H = #attribute_p{name = export, value=FunAritys} | T], Parts, Options) ->
    #parts{attributes = Attrs, exports = Exports} = Parts,
    part(T,
         Parts#parts{attributes = [H | Attrs], exports = FunAritys ++ Exports},
         Options);
part([H  = #attribute_p{} | T], Parts=#parts{attributes=Attributes}, Options) ->
    part(T, Parts#parts{attributes = [H | Attributes]}, Options);
part([H = #func_p{name = Name, arity = Arity} | T], Parts , Options) ->
    #parts{funcs = Funcs} = Parts,
    part(T, Parts#parts{funcs = dict:append({Name, Arity}, H, Funcs)}, Options).

%% ===================================================================
%% Lift
%% ===================================================================
lift(#parts{attributes = Attrs, funcs = Funcs, exports = Exports}, Options) ->
    lift(dict:to_list(Funcs),
         #lifted{attributes = Attrs, exports = Exports},
         Options).

lift([], Lifted = #lifted{errors = []}, _) -> {ok, Lifted};
lift([], #lifted{errors = Errors}, _) -> {errors, Errors};
lift([{FunArity, [#func_p{line = Line1}, #func_p{line = Line2}|_]}|_], _, _) ->
    {error, {FunArity, "at line", Line2, "already defined at", Line1}};
lift([{FunArity, [Func]} | T], Lifted, Options) ->
    #lifted{funcs = Funcs, errors = Errors} = Lifted,
    {Func1, Errors1} = lift_f(Func, Options),
    lift(T,
         Lifted#lifted{funcs = dict:store(FunArity, Func1, Funcs),
                       errors = Errors1 ++ Errors},
         Options).

lift_f(#func_p{line = L, name = N, arity = A, clauses = Cs}, Options) ->
    {Clauses, #lifting{errors = Errors}} = lift_fcs(Cs, [], #lifting{},Options),
    {#func_c{line = L, name = N, arity = A, clauses = Clauses}, Errors}.

lift_fcs([], Acc, Lift, _) -> {lists:reverse(Acc), Lift};
lift_fcs([C | Cs], Acc, Lift, Options) ->
    {C1, #lifting{no = No, errors = Errors}} = lift_c(C, Lift, Options),
    lift_fcs(Cs, [C1 | Acc], #lifting{no = No, errors = Errors}, Options).

lift_cs([], Acc, Lift, _) -> {lists:reverse(Acc), Lift};
lift_cs([C | Cs], Acc, Lift, Options) ->
    {C1, Lift1} = lift_c(C, Lift, Options),
    lift_cs(Cs, [C1 | Acc], Lift1, Options).

lift_c(#clause_p{line = L, name=N, args=A, guard=G, body=B}, Lift, Options) ->
    #lifting{binding = Binding} = Lift,
    {Args, Lift1} = lift_ps(A, [], Lift, Options),
    {Guard, Lift2} = lift_g(G, [], Lift1, Options),
    {Body, Lift3} = lift_es(B, [], Lift2, Options),
    #lifting{defs = Defs, bound = Bound, binding = Binding1} = Lift3,
    {#clause_c{line = L,
               name = N,
               args = Args,
               guard = Guard,
               body = Body,
               defs = Defs},
     Lift3#lifting{bound = (Binding1 -- Binding) ++ Bound, binding = []}}.

lift_ps([], Acc, Lift, _) -> {lists:reverse(Acc), Lift};
lift_ps([H | T], Acc, Lift, Options) ->
    {H1, Lift1} = lift_p(H, Lift, Options),
    lift_ps(T, [H1 | Acc], Lift1, Options).

lift_g([], Acc, Lift, _) -> {lists:reverse(Acc), Lift};
lift_g([H | T], Acc, Lift, Options) ->
    {H1, Lift1} = lift_es(H, [], Lift, Options),
    lift_g(T, [H1 | Acc], Lift1, Options).

lift_p(Atom = #atom{}, Lift, _) -> {Atom, Lift};
lift_p(Integer = #integer{}, Lift, _) -> {Integer, Lift};
lift_p(Float = #float{}, Lift, _) -> {Float, Lift};
lift_p(Char = #char{}, Lift, _) -> {Char, Lift};
lift_p(String = #string{}, Lift, _) -> {String, Lift};
lift_p(Nil = #nil_p{}, Lift, _) -> {Nil, Lift};
lift_p(Var = #var{line = L, name = Name}, Lift, _) ->
    case anon(Var) of
        true -> {#anon_c{line = L, name = Name}, Lift};
        false ->
            #lifting{no = No,
                     defs = Defs,
                     bound = Bound,
                     binding = Binding,
                     errors = Errors} = Lift,
            case dict:find(Name, Defs) of
                {ok, No1} ->
                    case lists:member(No1, Bound) of
                        true ->
                            {#var_c{line = L,
                                    name = Name,
                                    type = use,
                                    no = No1},
                             Lift#lifting{errors =
                                              [{'Var', L, 'already defined'} |
                                               Errors]}};
                        false ->
                            {#var_c{line = L, name = Name, type = use, no=No1},
                             Lift}
                    end;
                error ->
                    {#var_c{line = L, name = Name, type = bind, no = No},
                     #lifting{no = No + 1,
                              defs = dict:store(Name, No, Defs),
                              binding = [No | Binding]
                             }}
            end
    end;
lift_p(Cons = #cons_p{car = Car, cdr = Cdr}, Lift, Options) ->
    {Car1, Lift1} = lift_p(Car, Lift, Options),
    {Cdr1, Lift2} = lift_p(Cdr, Lift1, Options),
    {Cons#cons_p{car = Car1, cdr = Cdr1}, Lift2};
lift_p(Match = #match_p{left = Left, right = Right}, Lift, Options) ->
    {Left1, Lift1} = lift_p(Left, Lift, Options),
    {Right1, Lift2} = lift_p(Right, Lift1, Options),
    {Match#match_p{left = Left1, right = Right1}, Lift2};
lift_p(IndexP = #index_p{index = Index, expr = Expr}, Lift, Options) ->
    {Index1, Lift1} = lift_p(Index, Lift, Options),
    {Expr1, Lift2} = lift_p(Expr, Lift1, Options),
    {IndexP#index_p{index = Index1, expr = Expr1}, Lift2};
lift_p(Map = #map_p{exprs = Exprs, vars = Vars}, Lift, Options) ->
    {Exprs1, Lift1} = lift_ps(Exprs, [], Lift, Options),
    {Vars1, Lift2} = lift_ps(Vars, [], Lift1, Options),
    {Map#map_p{exprs = Exprs1, vars = Vars1}, Lift2};
lift_p(Bin = #bin_p{elements = Elements}, Lift, Options) ->
    {Elements1, Lift1} = lift_ps(Elements, [], Lift, Options),
    {Bin#bin_p{elements = Elements1}, Lift1};
lift_p(Element = #bin_element_p{expr = E, size = S}, Lift, Options) ->
    {Expr, Lift1} = lift_p(E, Lift, Options),
    {Size, Lift2} = lift_bs(S, Lift1, Options),
    {Element#bin_element_p{expr = Expr, size = Size}, Lift2};
lift_p(X, Lift = #lifting{errors = Errors}, _) ->
    {X, Lift#lifting{errors = [{'illegal pattern', element(2, X), X}|Errors]}}.

lift_bs(default, Lift, _) -> {default, Lift};
lift_bs(X, Lift, Options) -> lift_p(X, Lift, Options).

lift_es([], Acc, Lift, _) -> {lists:reverse(Acc), Lift};
lift_es([H | T], Acc, Lift, Options) ->
    {H1, Lift1} = lift_e(H, Lift, Options),
    lift_es(T, [H1 | Acc], Lift1, Options).

lift_e(Atom = #atom{}, Lift, _) -> {Atom, Lift};
lift_e(Integer = #integer{}, Lift, _) -> {Integer, Lift};
lift_e(Float = #float{}, Lift, _) -> {Float, Lift};
lift_e(Char = #char{}, Lift, _) -> {Char, Lift};
lift_e(String = #string{}, Lift, _) -> {String, Lift};
lift_e(Var = #var{line = L, name = Name}, Lift, _) ->
    case anon(Var) of
        true ->
            #lifting{errors = Errors} = Lift,
            {#anon_c{line = L, name = Name},
             Lift#lifting{errors = [{'anon in expr', L} | Errors]}};
        false ->
            #lifting{no = No,
                     defs = Defs,
                     bound = Bound,
                     binding = Binding,
                     errors = Errors} = Lift,
            case dict:find(Name, Defs) of
                {ok, No1} ->
                    case lists:member(No1, Bound) of
                        true ->
                            {#var_c{line = L,
                                    name = Name,
                                    type = use,
                                    no = No1},
                             Lift#lifting{errors =
                                              [{var,Name, L,'already defined'} |
                                               Errors]}};
                        false ->
                            {#var_c{line = L, name = Name, type = use, no=No1},
                             Lift}
                    end;
                error ->
                    #lifting{errors = Errors} = Lift,
                    {#var_c{line = L, name = Name, type = bind, no = No},
                     #lifting{no = No + 1,
                              defs = dict:store(Name, No, Defs),
                              binding = [No | Binding],
                              errors = [{'unbound in expr', Name, L} | Errors]
                             }}
            end
    end;
lift_e(Match = #match_p{left = Left, right = Right}, Lift, Options) ->
    {Right1, Lift1} = lift_e(Right, Lift, Options),
    {Left1, Lift2} = lift_p(Left, Lift1, Options),
    {Match#match_p{left = Left1, right = Right1}, Lift2};
lift_e(IndexP = #index_p{index = Index, expr = Expr}, Lift, Options) ->
    {Index1, Lift1} = lift_e(Index, Lift, Options),
    {Expr1, Lift2} = lift_e(Expr, Lift1, Options),
    {IndexP#index_p{index = Index1, expr = Expr1}, Lift2};
lift_e(Map = #map_p{exprs = Exprs, vars = Vars}, Lift, Options) ->
    {Exprs1, Lift1} = lift_es(Exprs, [], Lift, Options),
    {Vars1, Lift2} = lift_es(Vars, [], Lift1, Options),
    {Map#map_p{exprs = Exprs1, vars = Vars1}, Lift2};
lift_e(Bin = #bin_p{elements = Elements}, Lift, Options) ->
    {Elements1, Lift1} = lift_es(Elements, [], Lift, Options),
    {Bin#bin_p{elements = Elements1}, Lift1};
lift_e(Element = #bin_element_p{expr = E, size = S}, Lift, Options) ->
    {Expr, Lift1} = lift_e(E, Lift, Options),
    {Size, Lift2} = lift_bs(S, Lift1, Options),
    {Element#bin_element_p{expr = Expr, size = Size}, Lift2};
lift_e(Fun = #fun_p{module = M, function = F, arity = A}, Lift, Options) ->
    {Module, Lift1} = lift_or_e(atom, M, Lift, Options),
    {Function, Lift2} = lift_or_e(atom, F, Lift1, Options),
    {Arity, Lift3} = lift_or_e(integer, A, Lift2, Options),
    {Fun#fun_p{module = Module, function = Function, arity = Arity}, Lift3};
lift_e(#func_p{line = L, name = N, arity = A, clauses = Cs}, Lift, Options) ->
    {Clauses, Lift1} = lift_fcs(Cs, [], Lift, Options),
    {#func_c{line = L, name = N, arity = A, clauses = Clauses}, Lift1};
lift_e(Case = #case_p{expr = E, clauses = Cs}, Lift, Options) ->
    {Expr, Lift1} = lift_e(E, Lift, Options),
    {Clauses, Lift2} = lift_cs(Cs, [], Lift1, Options),
    {Case#case_p{expr = Expr, clauses = Clauses}, Lift2};
lift_e(Remote = #remote_p{module = M, function = F}, Lift, Options) ->
    {Module, Lift1} = lift_e(M, Lift, Options),
    {Function, Lift2} = lift_e(F, Lift1, Options),
    {Remote#remote_p{module = Module, function = Function}, Lift2};
lift_e(Call = #call_p{func = F, args = As}, Lift, Options) ->
    {Function, Lift1} = lift_e(F, Lift, Options),
    {Args, Lift2} = lift_es(As, [], Lift1, Options),
    {Call#call_p{func = Function, args = Args}, Lift2};
lift_e(Op = #op_p{left = L, right = R}, Lift, Options) ->
    {Left, Lift1} = lift_e(L, Lift, Options),
    {Right, Lift2} = lift_e(R, Lift1, Options),
    {Op#op_p{left = Left, right = Right}, Lift2};
lift_e(UnOp = #unop_p{right = R}, Lift, Options) ->
    {Right, Lift1} = lift_e(R, Lift, Options),
    {UnOp#unop_p{right = Right}, Lift1};
lift_e(Gen = #gen_p{left = L, right = R}, Lift, Options) ->
    {Left, Lift1} = lift_e(L, Lift, Options),
    {Right, Lift2} = lift_p(R, Lift1, Options),
    {Gen#gen_p{left = Left, right = Right}, Lift2};
lift_e(SeqGen = #seq_gen_p{left = L}, Lift, Options) ->
    {Left, Lift1} = lift_e(L, Lift, Options),
    {SeqGen#seq_gen_p{left = Left}, Lift1};
lift_e(MapC = #map_c_p{map = M, c_exprs = Es}, Lift, Options) ->
    {Exprs, Lift1} = lift_es(Es, [], Lift, Options),
    {Map, Lift2} = lift_e(M, Lift1, Options),
    {MapC#map_c_p{map = Map, c_exprs = Exprs}, Lift2};
lift_e(BinC = #bin_c_p{bin = B, c_exprs = Es}, Lift, Options) ->
    {Exprs, Lift1} = lift_es(Es, [], Lift, Options),
    {Binary, Lift2} = lift_e(B, Lift1, Options),
    {BinC#bin_c_p{bin = Binary, c_exprs = Exprs}, Lift2};
lift_e(Receive = #receive_p{clauses = Cs, after_expr = A, after_body  = B},
       Lift,
       Options) ->
    {Clauses, Lift1} = lift_cs(Cs, [], Lift, Options),
    {Expr, Lift2} = lift_e(A, Lift1, Options),
    {Body, Lift3} = lift_es(B, [], Lift2, Options),
    {Receive#receive_p{clauses = Clauses, after_expr = Expr, after_body = Body},
     Lift3};
lift_e(X, Lift = #lifting{errors = Errors}, _) ->
    {X, Lift#lifting{errors = [{'illegal pattern', element(2, X), X}|Errors]}}.

lift_or_e(atom, Atom, Lift, _) when is_atom(Atom) -> {Atom, Lift};
lift_or_e(integer, Integer, Lift,_) when is_integer(Integer) -> {Integer, Lift};
lift_or_e(_, X, Lift, Options) -> lift_e(X, Lift, Options).

anon(#var{name = Name}) ->
    case atom_to_list(Name) of
        [$_ | _] -> true;
        _ -> false
    end.

%% ===================================================================
%% Match
%% ===================================================================

match(Lifted, _) -> {ok, Lifted}.


%% ===================================================================
%% Common parts
%% ===================================================================


%% format_error(Module, Message, Line) ->
%%     io:format("Error Line ~p:~s~n", [Line, Module:format_error(Message)]).

parse_opts([], Rec) -> Rec;
parse_opts(Opts, Rec) -> lists:foldl(fun parse_opt/2, Rec, Opts).

parse_opt({dest_name, Name}, Opts) -> Opts#opts{dest_name = Name};
parse_opt({src_dir, Dir}, Opts) -> Opts#opts{src_dir = Dir};
parse_opt({dest_dir, Dir}, Opts) -> Opts#opts{dest_dir = Dir}.
