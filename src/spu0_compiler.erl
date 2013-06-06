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
-export([file/1]).

%% Records
-record(state,
        {file = "",
         binary = <<>>,
         string = "",
         cont = [],
         line = 1,
         tokens = [],
         abs = []}).

%%====================================================================
%% API
%%====================================================================


%%--------------------------------------------------------------------
%% Function: file(FileName) -> ok.
%% @doc
%%   Compiles SPU0 file.
%% @end
%%--------------------------------------------------------------------
-spec file(atom() | string()) -> ok.
%%--------------------------------------------------------------------
file(File) when is_atom(File) ->
    file(list_to_atom(atom_to_list(File) ++ ".spu0"));
file(File) ->
    {ok, Bin} = file:read_file(File),
    S = scan_parse(scan, #state{file = File, binary = Bin}),
    io:format("~p~n" , [S]).

%% ===================================================================
%% Internal functions.
%% ===================================================================

scan_parse(scan, State = #state{binary = <<>>, cont = [], string = ""}) ->
    lists:reverse(State#state.abs);
scan_parse(scan, State = #state{binary = <<>>, string = "", cont = Cont}) ->
    case ensure_whitespace_only(Cont) of
        true -> scan_parse(scan, State#state{cont = []});
        false -> {error, State}
    end;
scan_parse(scan, State = #state{binary = Bin, string = ""}) ->
    State1 =
        case bin_to_list(Bin) of
            {"", <<>>} -> State#state{binary = <<>>, string = ""};
            {String, Bin1} -> State#state{binary = Bin1, string = String}
        end,
    scan_parse(scan, State1);
scan_parse(scan, State = #state{string = String, cont = Cont, line = Line}) ->
    case spu0_scan:tokens(Cont, String, Line) of
        {done, {ok, Toks, Line1}, Rest} ->
            State1 =
                State#state{tokens = Toks,
                            string = Rest,
                            cont = [],
                            line = Line1},
            scan_parse(parse, State1);
        {more,  Cont1} ->
            scan_parse(scan, State#state{string  = "", cont = Cont1})
    end;
scan_parse(parse, State = #state{tokens = Toks, abs = Abs}) ->
    case spu0_parse:parse_form(Toks) of
        {ok, Abs1} ->
            scan_parse(scan, State#state{abs = [Abs1 | Abs], tokens = []});
        Error ->
            Error
    end.

bin_to_list(Bin) -> bin_to_list(Bin, 1024, []).

bin_to_list(<<>>, _, Acc) -> {lists:reverse([$\n | Acc]), <<>>};
bin_to_list(Bin, 0, Acc) -> {lists:reverse(Acc), Bin};
bin_to_list(<<H, T/binary>>, N, Acc) -> bin_to_list(T, N - 1, [H | Acc]).

ensure_whitespace_only({tokens, _, _, Rest, _, _ , _, _, _}) ->
    ensure_whitespace_only1(Rest).

ensure_whitespace_only1([]) -> true;
ensure_whitespace_only1([$\n | T]) -> ensure_whitespace_only1(T);
ensure_whitespace_only1([$\r | T]) -> ensure_whitespace_only1(T);
ensure_whitespace_only1([$\t | T]) -> ensure_whitespace_only1(T);
ensure_whitespace_only1([$\v | T]) -> ensure_whitespace_only1(T);
ensure_whitespace_only1([$\b | T]) -> ensure_whitespace_only1(T);
ensure_whitespace_only1([$\f | T]) -> ensure_whitespace_only1(T);
ensure_whitespace_only1([$\e | T]) -> ensure_whitespace_only1(T);
ensure_whitespace_only1([$\s | T]) -> ensure_whitespace_only1(T);
ensure_whitespace_only1([$\d | T]) -> ensure_whitespace_only1(T);
ensure_whitespace_only1(_) -> false.

