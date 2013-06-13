%% -*-erlang-*-
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
%%%   SPU0 parser.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------

%% ===================================================================
%% Nonterminals.
%% ===================================================================
Nonterminals
form
attribute attr_val
function function_clauses function_clause
clause_args clause_guard clause_body
expr expr_100 expr_200 expr_400 expr_500
expr_600 expr_700 expr_800 expr_900
expr_max
map map_expr map_exprs map_index
sequence tail
lc_expr lc_exprs
map_comprehension mc_expr mc_exprs
sequence_generator
binary_comprehension
case_expr cr_clause cr_clauses receive_expr
fun_expr fun_clause fun_clauses atom_or_var integer_or_var
function_call argument_list
exprs guard
atomic strings
prefix_op mult_op add_op comp_op
binary bin_elements bin_element bit_expr
opt_bit_size_expr bit_size_expr opt_bit_type_list bit_type_list bit_type.

%% ===================================================================
%% Terminals.
%% ===================================================================
Terminals
char integer float atom string var

'(' ')' ',' '->' '~>' '<~' '{' '}' '[' ']' '|' '||' ';' ':'
'after' 'case' 'catch' 'end' 'fun' 'of' 'receive' 'when'
'bnot' 'not'
'*' '/' 'div' 'rem' 'band' 'and'
'+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor'
'==' '/=' '>=' '>' '=>'
'<<' '>>'
'!' '='
% helper
dot.

%% ===================================================================
%% Expected shit/reduce conflicts.
%% ===================================================================
Expect 0.

%% ===================================================================
%% Rootsymbol.
%% ===================================================================
Rootsymbol form.

%% ===================================================================
%% Rules.
%% ===================================================================

form -> attribute dot : '$1'.
form -> function dot  : '$1'.

attribute -> '-' atom attr_val      : build_attribute('$2', '$3').

attr_val -> expr                    : ['$1'].
attr_val -> expr ',' exprs          : ['$1' | '$3'].
attr_val -> '(' expr ',' exprs ')'  : ['$2' | '$4'].

function -> function_clauses : build_function('$1').

function_clauses -> function_clause                      : ['$1'].
function_clauses -> function_clause ';' function_clauses : ['$1'|'$3'].

function_clause -> atom clause_args clause_guard clause_body :
    {clause, line('$1'), element(3, '$1'), '$2', '$3', '$4'}.


clause_args -> argument_list : element(1, '$1').

clause_guard -> 'when' guard : '$2'.
clause_guard -> '$empty'     : [].

clause_body -> '->' exprs    : '$2'.


expr -> 'catch' expr : {'catch', line('$1'), '$2'}.
expr -> expr_100     : '$1'.

expr_100 -> expr_200 '=' expr_100 : {match, line('$2'), '$1', '$3'}.
expr_100 -> expr_200 '!' expr_100 : mkop('$1', '$2', '$3').
expr_100 -> expr_200              : '$1'.

expr_200 -> expr_400 comp_op expr_400 : mkop('$1', '$2', '$3').
expr_200 -> expr_400                  : '$1'.

expr_400 -> expr_400 add_op expr_500 : mkop('$1', '$2', '$3').
expr_400 -> expr_500                 : '$1'.

expr_500 -> expr_500 mult_op expr_600 : mkop('$1', '$2', '$3').
expr_500 -> expr_600                  : '$1'.

expr_600 -> prefix_op expr_700 : mkop('$1', '$2').
expr_600 -> expr_700           : '$1'.

expr_700 -> function_call : '$1'.
expr_700 -> expr_800      : '$1'.

expr_800 -> expr_900 ':' expr_max : {remote, line('$2'), '$1', '$3'}.
expr_800 -> expr_900              : '$1'.

expr_900 -> expr_max : '$1'.

expr_max -> var                  : '$1'.
expr_max -> atomic               : '$1'.
expr_max -> map                  : '$1'.
expr_max -> sequence             : '$1'.
expr_max -> binary               : '$1'.
expr_max -> map_comprehension    : '$1'.
expr_max -> sequence_generator   : '$1'.
expr_max -> binary_comprehension : '$1'.
expr_max -> '(' expr ')'         : '$2'.
expr_max -> case_expr            : '$1'.
expr_max -> receive_expr         : '$1'.
expr_max -> fun_expr             : '$1'.

map -> '{' '}' :
    {map, line('$1'), undefined, [], []}.
map -> atom '{' '}' :
    {map, line('$1'), '$1', [], []}.
map -> atom '{' var '}' :
    {map, line('$1'), '$1', [], ['$3']}.
map -> '{' map_expr map_exprs :
    {map, line('$1'), undefined, ['$2' | '$3'], []}.
map -> '{' map_expr map_exprs var '}' :
    {map, line('$1'), undefined, ['$2' | '$3'], ['$4']}.
map -> atom '{' map_expr map_exprs :
    {map, line('$1'), '$1', ['$3' | '$4'], []}.
map -> atom '{' map_expr map_exprs var '}' :
    {map, line('$1'), '$1', ['$3' | '$4'], ['$5']}.

map_exprs -> '|'                    : [].
map_exprs -> '}'                    : [].
map_exprs -> ',' map_expr map_exprs : ['$2' | '$3'].

map_expr ->  map_index '~>' expr : {right, line('$1'), '$1', '$3'}.
map_expr ->  expr '<~' map_index : {left, line('$1'), '$1', '$3'}.

map_index -> atom     : '$1'.
map_index -> var      : '$1'.
map_index -> integer  : '$1'.

sequence -> '[' ']'       : {nil, line('$1')}.
sequence -> '[' expr tail : {cons, line('$1'), '$2', '$3'}.

tail -> ']'           : {nil, line('$1')}.
tail -> '|' expr ']'  : '$2'.
tail -> ',' expr tail : {cons,line('$2'),'$2','$3'}.

binary -> '<<' '>>'              : {bin, line('$1'), []}.
binary -> '<<' bin_elements '>>' : {bin, line('$1'), '$2'}.

bin_elements -> bin_element                  : ['$1'].
bin_elements -> bin_element ',' bin_elements : ['$1' | '$3'].

bin_element -> bit_expr opt_bit_size_expr opt_bit_type_list :
        {bin_element, line('$1'), '$1', '$2', '$3'}.

bit_expr -> prefix_op expr_max : mkop('$1', '$2').
bit_expr -> expr_max           : '$1'.

opt_bit_size_expr -> ':' bit_size_expr : '$2'.
opt_bit_size_expr -> '$empty'          : default.

opt_bit_type_list -> '/' bit_type_list : '$2'.
opt_bit_type_list -> '$empty'          : default.

bit_type_list -> bit_type '-' bit_type_list : ['$1' | '$3'].
bit_type_list -> bit_type                   : ['$1'].

bit_type -> atom             : element(3, '$1').
bit_type -> atom ':' integer : { element(3,'$1'), element(3,'$3') }.

bit_size_expr -> expr_max : '$1'.

map_comprehension -> '{' expr '||' mc_exprs '}' : {mc, line('$1'), '$2', '$4'}.
mc_exprs -> mc_expr              : ['$1'].
mc_exprs -> mc_expr ',' mc_exprs : ['$1' | '$3'].

mc_expr -> expr             : '$1'.
mc_expr -> expr '=>' expr   : {generate, line('$2'), '$1', '$3'}.

sequence_generator -> '{' expr '=>' '}' : {seq_gen, line('$1'), '$2'}.

binary_comprehension -> '<<' binary '||' lc_exprs '>>' :
    {bc, line('$1'), '$2', '$4'}.
lc_exprs -> lc_expr              : ['$1'].
lc_exprs -> lc_expr ',' lc_exprs : ['$1' | '$3'].

lc_expr -> expr             : '$1'.
lc_expr -> expr '=>' binary : {generate, line('$2'), '$1', '$3'}.

%% N.B. This is called from expr_700.

function_call -> expr_800 argument_list :
    {call, line('$1'), '$1', element(1, '$2')}.

case_expr -> 'case' expr 'of' cr_clauses 'end' :
    {'case', line('$1'), '$2', '$4'}.

cr_clauses -> cr_clause                : ['$1'].
cr_clauses -> cr_clause ';' cr_clauses : ['$1' | '$3'].

cr_clause -> expr clause_guard clause_body :
    {clause, line('$1'), ['$1'], '$2', '$3'}.

receive_expr -> 'receive' cr_clauses 'end' :
    {'receive', line('$1'), '$2'}.
receive_expr -> 'receive' 'after' expr clause_body 'end' :
    {'receive', line('$1'), [], '$3', '$4'}.
receive_expr -> 'receive' cr_clauses 'after' expr clause_body 'end' :
    {'receive', line('$1'), '$2', '$4', '$5'}.


fun_expr -> 'fun' atom '/' integer :
    {'fun', line('$1'), {function, element(3, '$2'), element(3, '$4')}}.
fun_expr -> 'fun' atom_or_var ':' atom_or_var '/' integer_or_var :
    {'fun', line('$1'), {function, '$2', '$4', '$6'}}.
fun_expr -> 'fun' fun_clauses 'end' :
    build_fun(line('$1'), '$2').

atom_or_var -> atom : '$1'.
atom_or_var -> var  : '$1'.

integer_or_var -> integer : '$1'.
integer_or_var -> var     : '$1'.

fun_clauses -> fun_clause                 : ['$1'].
fun_clauses -> fun_clause ';' 'fun' fun_clauses : ['$1' | '$3'].

fun_clause -> argument_list clause_guard clause_body :
    {Args,Pos} = '$1',
    {clause, Pos, 'fun', Args, '$2', '$3'}.

argument_list -> '(' ')'       : {[], line('$1')}.
argument_list -> '(' exprs ')' : {'$2', line('$1')}.

exprs -> expr           : ['$1'].
exprs -> expr ',' exprs : ['$1' | '$3'].

guard -> exprs           : ['$1'].
guard -> exprs ';' guard : ['$1' | '$3'].

atomic -> char    : '$1'.
atomic -> integer : '$1'.
atomic -> float   : '$1'.
atomic -> atom    : '$1'.
atomic -> strings : '$1'.

strings -> string : '$1'.
strings -> string strings :
    {string, line('$1'), element(3, '$1') ++ element(3, '$2')}.

prefix_op -> '+'    : '$1'.
prefix_op -> '-'    : '$1'.
prefix_op -> 'bnot' : '$1'.
prefix_op -> 'not'  : '$1'.

mult_op -> '/'    : '$1'.
mult_op -> '*'    : '$1'.
mult_op -> 'div'  : '$1'.
mult_op -> 'rem'  : '$1'.
mult_op -> 'band' : '$1'.
mult_op -> 'and'  : '$1'.

add_op -> '+'    : '$1'.
add_op -> '-'    : '$1'.
add_op -> 'bor'  : '$1'.
add_op -> 'bxor' : '$1'.
add_op -> 'bsl'  : '$1'.
add_op -> 'bsr'  : '$1'.
add_op -> 'or'   : '$1'.
add_op -> 'xor'  : '$1'.

comp_op -> '=='  : '$1'.
comp_op -> '/='  : '$1'.
comp_op -> '>='  : '$1'.
comp_op -> '>'   : '$1'.

%% ===================================================================
%% Erlang Code.
%% ===================================================================
Erlang code.

%% API
-export([parse_form/1]).

-export([file/1]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
% -spec 
%%--------------------------------------------------------------------
parse_form(Tokens) -> parse(Tokens).

%%--------------------------------------------------------------------
%% Function: file(FileName) -> .
%% @doc
%%   Parses a .spu0 file.
%% @end
%%--------------------------------------------------------------------
-spec file(string()) -> {ok, _} | {error, _}.
%%--------------------------------------------------------------------
file(File) ->
    case spu0_scan:file(File) of
        {ok, Tokens} -> parse(Tokens);
        Error -> Error
    end.

%%====================================================================
%% Internal functions
%%====================================================================

build_attribute({atom, La, module}, Val) ->
    case Val of
        [{atom, _, Module}] ->
            {attribute, La, module, Module};
        _ ->
            error_bad_decl(La, module)
    end;
build_attribute({atom, La, export}, Val) ->
    case Val of
        [ExpList] -> {attribute, La, export, farity_list(ExpList)};
        _ ->
            error_bad_decl(La, export)
    end;
build_attribute({atom, La, file}, Val) ->
    case Val of
        [{string, _, Name}, {integer, _, Line}] ->
            {attribute, La, file, {Name, Line}};
        _ ->
            error_bad_decl(La, file)
    end;
build_attribute({atom, La, Attr}, Val) ->
    case Val of
        [Expr0] ->
            Expr = attribute_farity(Expr0),
            {attribute, La, Attr, term(Expr)};
        _ ->
            ret_err(La, "bad attribute")
    end.

attribute_farity({cons, L, H, T}) ->
    {cons, L, attribute_farity(H), attribute_farity(T)};
attribute_farity({op, L, '/', Name = {atom, _, _}, Arity = {integer, _, _}}) ->
    {tuple, L, [Name, Arity]};
attribute_farity(Other) ->
    Other.

error_bad_decl(L, S) -> ret_err(L, io_lib:format("bad ~w declaration", [S])).

farity_list({nil, _}) -> [];
farity_list({cons, _, {op, _, '/', {atom, _, A}, {integer, _, I}}, Tail}) ->
    [{A, I} | farity_list(Tail)];
farity_list(Other) ->
    ret_err(line(Other), "bad function arity").

term(Expr) ->
    case catch {ok, normalise(Expr)} of
        {ok, Norm} ->
            Norm;
        _ ->
            ret_err(line(Expr), "bad attribute")
    end.

%% build_function([Clause]) -> {function,Line,Name,Arity,[Clause]}

build_function(Cs) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    {function, line(hd(Cs)), Name, Arity, check_clauses(Cs, Name, Arity)}.

%% build_fun(Line, [Clause]) -> {'fun',Line,{clauses,[Clause]}}.

build_fun(Line, Cs) ->
    Arity = length(element(4, hd(Cs))),
    {'fun', Line, {clauses, check_clauses(Cs, 'fun', Arity)}}.

check_clauses(Cs, Name, Arity) ->
     lists:map(
       fun ({clause, L, N, As, G, B}) when N =:= Name, length(As) =:= Arity ->
               {clause, L, As, G, B};
           ({clause, L, _N, _As, _G, _B}) ->
               ret_err(L, "head mismatch") end, Cs).

ret_err(L, S) ->
    {location, Location} = get_attribute(L, location),
    return_error(Location, S).

%%  Convert between the abstract form of a term and a term.

normalise({char, _, C}) -> C;
normalise({integer, _, I}) -> I;
normalise({float, _, F}) -> F;
normalise({atom, _, A}) -> A;
normalise({string, _, S}) -> S;
normalise({nil, _}) -> [];
normalise({bin, _, Fs}) ->
    {value, B, _} =
        eval_bits:expr_grp(Fs, [],
                           fun(E, _) ->
                                   {value, normalise(E), []}
                           end, [], true),
    B;
normalise({cons, _, Head, Tail}) ->
    [normalise(Head) | normalise(Tail)];
%% Special case for unary +/-.
normalise({op, _, '+', {char, _, I}}) -> I;
normalise({op, _, '+', {integer, _, I}}) -> I;
normalise({op, _, '+', {float, _, F}}) -> F;
normalise({op, _, '-', {char, _, I}}) -> -I; %Weird, but compatible!
normalise({op, _, '-',{integer, _, I}}) -> -I;
normalise({op, _, '-',{float, _, F}}) -> -F;
normalise(X) -> erlang:error({badarg, X}).

abstract(T) when is_integer(T) -> {integer, 0, T};
abstract(T) when is_float(T) -> {float, 0, T};
abstract(T) when is_atom(T) -> {atom, 0, T};
abstract([]) -> {nil, 0};
abstract(B) when is_bitstring(B) ->
    {bin, 0, [abstract_byte(Byte, 0) || Byte <- bitstring_to_list(B)]};
abstract([C | T]) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C]);
abstract([H | T]) ->
    {cons, 0, abstract(H), abstract(T)}.

abstract_string([C | T], String) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C | String]);
abstract_string([], String) ->
    {string, 0, lists:reverse(String)};
abstract_string(T, String) ->
    not_string(String, abstract(T)).

not_string([C | T], Result) ->
    not_string(T, {cons, 0, {integer, 0, C}, Result});
not_string([], Result) ->
    Result.

abstract_byte(Byte, Line) when is_integer(Byte) ->
    {bin_element, Line, {integer, Line, Byte}, default, default};
abstract_byte(Bits, Line) ->
    Sz = bit_size(Bits),
    <<Val:Sz>> = Bits,
    {bin_element, Line, {integer, Line, Val}, {integer, Line, Sz}, default}.

%%% abstract/2 keeps the line number
abstract(T, Line) when is_integer(T) -> {integer, Line, T};
abstract(T, Line) when is_float(T) -> {float, Line, T};
abstract(T, Line) when is_atom(T) -> {atom, Line, T};
abstract([], Line) -> {nil, Line};
abstract(B, Line) when is_bitstring(B) ->
    {bin, Line, [abstract_byte(Byte, Line) || Byte <- bitstring_to_list(B)]};
abstract([C | T], Line) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C], Line);
abstract([H | T], Line) ->
    {cons, Line, abstract(H, Line), abstract(T, Line)}.

abstract_string([C | T], String, Line) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C | String], Line);
abstract_string([], String, Line) ->
    {string, Line, lists:reverse(String)};
abstract_string(T, String, Line) ->
    not_string(String, abstract(T, Line), Line).

not_string([C | T], Result, Line) ->
    not_string(T, {cons, Line, {integer, Line, C}, Result}, Line);
not_string([], Result, _) ->
    Result.

%%  Generate a list of tokens representing the abstract term.

tokens(Abs) -> tokens(Abs, []).

tokens({char, L, C}, More) -> [{char, L, C} | More];
tokens({integer, L, N}, More) -> [{integer, L, N} | More];
tokens({float, L, F}, More) -> [{float, L, F} | More];
tokens({atom, L, A}, More) -> [{atom, L, A} | More];
tokens({var, L, V}, More) -> [{var, L, V} | More];
tokens({string, L, S}, More) -> [{string, L, S} | More];
tokens({nil, L}, More) -> [{'[', L}, {']', L} | More];
tokens({cons, L, Head, Tail}, More) ->
    [{'[', L} | tokens(Head, tokens_tail(Tail, More))].

tokens_tail({cons, L, Head, Tail}, More) ->
    [{',', L} | tokens(Head, tokens_tail(Tail, More))];
tokens_tail({nil, L}, More) ->
    [{']', L}|More];
tokens_tail(Other, More) ->
    L = line(Other),
    [{'|', L} | tokens(Other, [{']', L} | More])].

%% Give the relative precedences of operators.

inop_prec('=') -> {150, 100, 100};
inop_prec('!') -> {150, 100, 100};
inop_prec('==') -> {300, 200, 300};
inop_prec('/=') -> {300, 200, 300};
inop_prec('>=') -> {300, 200, 300};
inop_prec('>') -> {300, 200, 300};
inop_prec('+') -> {400, 400, 500};
inop_prec('-') -> {400, 400, 500};
inop_prec('bor') -> {400, 400, 500};
inop_prec('bxor') -> {400, 400, 500};
inop_prec('bsl') -> {400, 400, 500};
inop_prec('bsr') -> {400, 400, 500};
inop_prec('or') -> {400, 400, 500};
inop_prec('xor') -> {400, 400, 500};
inop_prec('*') -> {500, 500, 600};
inop_prec('/') -> {500, 500, 600};
inop_prec('div') -> {500, 500, 600};
inop_prec('rem') -> {500, 500, 600};
inop_prec('band') -> {500, 500, 600};
inop_prec('and') -> {500, 500, 600};
inop_prec('#') -> {800, 700, 800};
inop_prec(':') -> {900, 800, 900}.

preop_prec('catch') -> {0, 100};
preop_prec('+') -> {600, 700};
preop_prec('-') -> {600, 700};
preop_prec('bnot') -> {600, 700};
preop_prec('not') -> {600, 700};
preop_prec('#') -> {700, 800}.

func_prec() -> {800, 700}.

max_prec() -> 1000.

mkop({Op, Pos}, A) -> {op, Pos, Op, A}.
mkop(L, {Op, Pos}, R) -> {op, Pos, Op, L, R}.

line(Tuple) when is_tuple(Tuple) -> element(2, Tuple).

get_attribute({Line, _}, location) -> {location, Line};
get_attribute(Line, location) -> {location, Line}.

