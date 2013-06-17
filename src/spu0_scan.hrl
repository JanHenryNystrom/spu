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

-record(var, {line :: integer(),
              name :: atom()}).

-record(atom, {line :: integer(),
               name :: atom()}).

-record(integer, {line  :: integer(),
                  value :: integer()}).

-record(float, {line  :: integer(),
                value :: integer()}).

-record(char, {line  :: integer(),
               value :: integer()}).

