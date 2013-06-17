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
function_clauses -> function_clause ';' function_clauses : ['$1' | '$3'].

function_clause -> atom clause_args clause_guard clause_body :
    #clause{line = line('$1'),
            name = value('$1'),
            args = '$2',
            guard = '$3',
            body = '$4'}.

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

expr_800 -> expr_900 ':' expr_max :
    #remote{line = line('$2'), module = '$1', function = '$3'}.
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

sequence -> '[' ']'       : #nil{line = line('$1')}.
sequence -> '[' expr tail : #cons{line = line('$1'), car = '$2', cdr = '$3'}.

tail -> ']'           : #nil{line = line('$1')}.
tail -> '|' expr ']'  : '$2'.
tail -> ',' expr tail : #cons{line = line('$2'), car = '$2', cdr = '$3'}.

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

bit_type -> atom             : value('$1').
bit_type -> atom ':' integer : {value('$1'), value('$3')}.

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
    #call{line = line('$1'), func = '$1', args = element(1, '$2')}.

case_expr -> 'case' expr 'of' cr_clauses 'end' :
    #'case'{line = line('$1'), expr = '$2', clauses = '$4'}.

cr_clauses -> cr_clause                : ['$1'].
cr_clauses -> cr_clause ';' cr_clauses : ['$1' | '$3'].

cr_clause -> expr clause_guard clause_body :
    #clause{line = line('$1'), args = ['$1'], guard = '$2', body = '$3'}.

receive_expr -> 'receive' cr_clauses 'end' :
    {'receive', line('$1'), '$2'}.
receive_expr -> 'receive' 'after' expr clause_body 'end' :
    {'receive', line('$1'), [], '$3', '$4'}.
receive_expr -> 'receive' cr_clauses 'after' expr clause_body 'end' :
    {'receive', line('$1'), '$2', '$4', '$5'}.


fun_expr -> 'fun' atom '/' integer :
    {'fun', line('$1'), {function, value('$2'), value('$4')}}.
fun_expr -> 'fun' atom_or_var ':' atom_or_var '/' integer_or_var :
    {'fun', line('$1'), {function, '$2', '$4', '$6'}}.
fun_expr -> 'fun' fun_clauses 'end' :
    build_fun(line('$1'), '$2').

atom_or_var -> atom : '$1'.
atom_or_var -> var  : '$1'.

integer_or_var -> integer : '$1'.
integer_or_var -> var     : '$1'.

fun_clauses -> fun_clause                       : ['$1'].
fun_clauses -> fun_clause ';' 'fun' fun_clauses : ['$1' | '$3'].

fun_clause -> argument_list clause_guard clause_body :
    {Args, Pos} = '$1',
    #clause{line = Pos, name = 'fun', args = Args, guard = '$2', body = '$3'}.

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

strings -> string         : '$1'.
strings -> string strings :
    #string{line = Line, value = String1} = '$1',
    #string{value = String2} = '$2',
    #string{line = Line, value = String1 ++ String2}.


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
-export([parse_form/1, next_form/1]).

-export([file/1]).

%% Includes
-include_lib("spu/src/spu0_scan.hrl").
-include_lib("spu/src/spu0_parse.hrl").

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
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec next_form([tuple()]) -> {tuple(), [tuple()]}.
%%--------------------------------------------------------------------
next_form(Tokens) -> next_form(Tokens, []).

next_form([Dot = {dot, _} | T], Acc) -> {lists:reverse([Dot | Acc]), T};
next_form([H | T], Acc) -> next_form(T, [H | Acc]).

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
        {ok, Tokens} -> fold_tokens(Tokens);
        Error -> Error
    end.

%%====================================================================
%% Internal functions
%%====================================================================

build_attribute(#atom{line = La, name = module}, Val) ->
    case Val of
        [#atom{name = Module}] ->
            #attribute{line = La, name = module, value = Module};
        _ ->
            error_bad_decl(La, module)
    end;
build_attribute(#atom{line = La, name = export}, Val) ->
    case Val of
        [ExpList] ->
            #attribute{line = La, name = export, value = farity_list(ExpList)};
        _ ->
            error_bad_decl(La, export)
    end;
build_attribute(#atom{line = La, name = file}, Val) ->
    case Val of
        [#string{value = Name}, #integer{value = Line}] ->
            #attribute{line = La, name = file, value = {Name, Line}};
        _ ->
            error_bad_decl(La, file)
    end;
build_attribute(#atom{line = La, name = Attr}, Val) ->
    case Val of
        [Expr0] ->
            Expr = attribute_farity(Expr0),
            #attribute{line = La, name = Attr, value = term(Expr)};
        _ ->
            ret_err(La, "bad attribute")
    end.

attribute_farity(#cons{line = L, car = H, cdr = T}) ->
    {cons, L, attribute_farity(H), attribute_farity(T)};
attribute_farity(#op{line = L,
                     op = '/',
                     left = Name = #atom{},
                     right = Arity = #integer{}}) ->
    {tuple, L, [Name, Arity]};
attribute_farity(Other) ->
    Other.

error_bad_decl(L, S) -> ret_err(L, io_lib:format("bad ~w declaration", [S])).

farity_list(#nil{}) -> [];
farity_list(#cons{car = #op{op = '/',
                            left = #atom{name = A},
                            right = #integer{value = I}},
                  cdr = T}) ->
    [{A, I} | farity_list(T)];
farity_list(Other) ->
    ret_err(line(Other), "bad function arity").

term(Expr) ->
    case catch {ok, normalise(Expr)} of
        {ok, Norm} -> Norm;
        _ -> ret_err(line(Expr), "bad attribute")
    end.

%% build_function([Clause]) -> {function,Line,Name,Arity,[Clause]}

build_function(Cs = [C | _]) ->
    Name = element(3, C),
    Arity = length(element(4, C)),
    #func{line = line(C),
          name = Name,
          arity = Arity,
          clauses = check_clauses(Cs, Name, Arity)}.

%% build_fun(Line, [Clause]) -> {'fun',Line,{clauses,[Clause]}}.

build_fun(Line, Cs = [C | _]) ->
    Arity = length(element(4, C)),
    #func{line = Line,
          name = 'fun',
          arity = Arity,
          clauses = check_clauses(Cs, 'fun', Arity)}.

check_clauses(Cs, Name, Arity) ->
    Check = fun (Clause = #clause{name = N, args = As})
                  when N =:= Name, length(As) =:= Arity -> Clause;
                (#clause{line = L}) ->
                    ret_err(L, "head mismatch")
            end,
    [Check(C) || C <- Cs].

ret_err(L, S) ->
    {location, Location} = get_attribute(L, location),
    return_error(Location, S).

%%  Convert between the abstract form of a term and a term.

normalise(#char{value = C}) -> C;
normalise(#integer{value = I}) -> I;
normalise(#float{value = F}) -> F;
normalise(#atom{name = A}) -> A;
normalise(#string{value = S}) -> S;
normalise(#nil{}) -> [];
normalise(#cons{car = Head, cdr = Tail}) -> [normalise(Head) | normalise(Tail)];
normalise({bin, _, Fs}) ->
    EvalFun = fun(E, _) -> {value, normalise(E), []} end,
    {value, Binary, _} = eval_bits:expr_grp(Fs, [], EvalFun, [], true),
    Binary;
%% Special case for unary +/-.
normalise(#op{op = '+', right = #char{value = C}}) -> C;
normalise(#op{op = '+', right = #integer{value = I}}) -> I;
normalise(#op{op = '+', right = #float{value = F}}) -> F;
%%Weird, but compatible!
normalise(#op{op = '-', right = #char{value = C}}) -> -C;
normalise(#op{op = '-', right = #integer{value = I}}) -> -I;
normalise(#op{op = '-', right = #float{value = F}}) -> -F;
normalise(X) -> erlang:error({badarg, X}).

mkop({Op, Pos}, A) -> #unop{line = Pos, op = Op, right = A}.
mkop(L, {Op, Pos}, R) -> #op{line = Pos, op = Op, left = L, right = R}.

line(Tuple) when is_tuple(Tuple) -> element(2, Tuple).

value(Tuple) when is_tuple(Tuple) -> element(3, Tuple).


get_attribute({Line, _}, location) -> {location, Line};
get_attribute(Line, location) -> {location, Line}.

fold_tokens(Tokens) -> fold_tokens(Tokens, []).

fold_tokens([], Abses) -> {ok, lists:reverse(Abses)};
fold_tokens(Tokens, Abses) ->
    {Form, Tokens1} = next_form(Tokens, []),
    {ok, Abs} = parse(Form),
    fold_tokens(Tokens1, [Abs | Abses]).


