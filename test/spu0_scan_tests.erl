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
%%%   eunit unit tests for spu0_scan tokenizer.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(spu0_scan_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests.
%% ===================================================================

%%%-------------------------------------------------------------------
% Spu0
%%%-------------------------------------------------------------------
scan_spu0_test_() ->
    [ ?_test(?assertMatch({ok, _}, spu0_scan:file(File))) ||
        File <- files(spu0)].


%% ===================================================================
%% Internal functions.
%% ===================================================================

files(spu0) ->
    Dir = filename:join([code:lib_dir(spu),
                         "test",
                         "spu0"]),
    {ok, Files} = file:list_dir(Dir),
    [filename:join([Dir, File]) ||
        File <- Files, filename:extension(File) == ".spu0"].
