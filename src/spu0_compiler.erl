%%==============================================================================
%% Copyright 2013 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
                funcs      = dict:new() :: dict(),
                exports    = []         :: [{atom(), integer()}]
               }).

-record(lifting, {no      = 0 :: integer(),
                  defs    = dict:new() :: dict(),
                  bound   = [] :: [integer()],
                  binding = [] :: [integer()],
                  errors  = [] :: [_]
                 }).

-record(lifted, {attributes          :: [#attribute_p{}],
                 funcs  = dict:new() :: dict(),
                 exports             :: [{atom(), integer()}],
                 defs                :: dict(),
                 errors = []         :: [_]
                }).

-record(opts, {dest_name :: string(),
               include_paths = [] :: [string()],
               src_dir = "." :: string(),
               dest_dir = "." :: string(),
               include_dir = "" :: string()
              }).

-record(var_c, {line :: integer(),
                name :: atom(),
                type :: use | bind,
                no   :: integer()
             }).

-record(clause_c, {line           :: integer(),
                   name  = 'case' :: atom(),
                   args           :: [_],
                   guard          :: [_],
                   body           :: [_],
                   defs           :: dict()
                  }).

-record(func_c, {line         :: integer(),
                 name = 'fun' :: atom(),
                 arity        :: integer(),
                 clauses      :: [#clause_c{}],
                 defs         :: dict()
                }).


%% Types
-type opt() :: {dest_name, string()} | {include_paths, [string()]} |
               {src_dir, string()} | {dest_dir, string()} |
               {include_dir, string()}.

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
           fun lift/2
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

lift([], Lifted, _) -> Lifted;
lift([{FunArity, [#func_p{line = Line1}, #func_p{line = Line2}|_]}|_], _, _) ->
    {error, {FunArity, "at line", Line2, "already defined at", Line1}};
lift([{FunArity, [Func]} | T], Lifted, Options) ->
    #lifted{funcs = Funcs, defs = Defs, errors = Errors} = Lifted,
    {Func1, Errors1} = lift_f(Func, Options),
    lift(T,
         Lifted#lifted{funcs = dict:store(FunArity, Func1, Funcs),
                       defs = Defs,
                       errors = Errors1 ++ Errors},
         Options).

lift_f(#func_p{line = L, name = N, arity = A, clauses = Cs}, Options) ->
    {Clauses, #lifting{defs = Defs, errors = Errors}} =
        lift_cs(Cs, [], #lifting{}, Options),
    {#func_c{line = L, name = N, arity = A, clauses = Clauses, defs = Defs},
     Errors}.

lift_cs([], Acc, Lift, _) -> {lists:reverse(Acc), Lift};
lift_cs([C | Cs], Acc, Lift, Options) ->
    {C1, Lift1} = lift_c(C, Lift, Options),
    lift_cs(Cs, [C1 | Acc], Lift1, Options).

lift_c(#clause_p{line = L, name=N, args=A, guard=G, body=B}, Lift, Options) ->
    {Args, Lift1} = lift_ps(A, [], Lift, Options),
    {Guard, Lift2} = lift_e(G, Lift1, Options),
    {Body, Lift3} = lift_e(B, Lift2, Options),
    #lifting{bound = Bound, binding = Binding} = Lift3,
    {#clause_c{line = L, name = N, args = Args, guard = Guard, body = Body},
     Lift3#lifting{bound = Binding  ++ Bound, binding = []}}.

lift_ps([], Acc, Lift, _) -> {lists:reverse(Acc), Lift};
lift_ps([H | T], Acc, Lift, Options) ->
    {H1, Lift1} = lift_p(H, Lift, Options),
    lift_ps(T, [H1 | Acc], Lift1, Options).

lift_p(Atom = #atom{}, Lift, _) -> {Atom, Lift};
lift_p(Integer = #integer{}, Lift, _) -> {Integer, Lift};
lift_p(Float = #float{}, Lift, _) -> {Float, Lift};
lift_p(Char = #char{}, Lift, _) -> {Char, Lift};
lift_p(String = #string{}, Lift, _) -> {String, Lift};
lift_p(#var{line = L, name = Name}, Lift, _) ->
     #lifting{no = No,
              defs = Defs,
              bound = Bound,
              binding = Binding,
              errors = Errors} = Lift,
    case dict:find(Name, Defs) of
        {ok, No1} ->
            case lists:member(No1, Bound) of
                true ->
                    {#var_c{line = L, name = Name, type = use, no = No1},
                     Lift#lifting{errors = [{'Var', L, 'already defined'} |
                                            Errors]}};
                false ->
                    {#var_c{line = L, name = Name, type = use, no = No1}, Lift}
            end;
        error ->
            {#var_c{line = L, name = Name, type = bind, no = No},
             #lifting{no = No + 1,
                      defs = dict:store(Name, No, Defs),
                      binding = [No | Binding]
                     }}
    end;
lift_p(X, Lift, _) ->
    {X, Lift}.

lift_e(X, Lift, _) -> {X, Lift}.

%% ===================================================================
%% Common parts
%% ===================================================================


%% format_error(Module, Message, Line) ->
%%     io:format("Error Line ~p:~s~n", [Line, Module:format_error(Message)]).

parse_opts([], Rec) -> Rec;
parse_opts(Opts, Rec) -> lists:foldl(fun parse_opt/2, Rec, Opts).

parse_opt({dest_name, Name}, Opts) -> Opts#opts{dest_name = Name};
parse_opt({src_dir, Dir}, Opts) -> Opts#opts{src_dir = Dir};
parse_opt({dest_dir, Dir}, Opts) -> Opts#opts{dest_dir = Dir};
parse_opt({include_dir, Dir}, Opts) -> Opts#opts{include_dir = Dir};
parse_opt({include_paths, Paths}, Opts = #opts{include_paths = Paths1}) ->
    Opts#opts{include_paths = Paths1 ++ Paths}.
