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

-record(attribute, {line  :: integer(),
                    name  :: atom(),
                    value :: _}).

-record(clause, {line           :: integer(),
                 name  = 'case' :: atom(),
                 args           :: [_],
                 guard          :: [_],
                 body           :: [_]}).

-record(func, {line         :: integer(),
               name = 'fun' :: atom(),
               arity        :: integer(),
               clauses      :: [#clause{}]}).

-record(nil, {line :: integer()}).

-record(cons, {line :: integer(),
               car  :: _,
               cdr  :: #nil{} | #cons{}}).

-record('case', {line    :: integer(),
                 expr    :: _,
                 clauses :: [#clause{}]}).

-record(remote, {line     :: integer(),
                 module   :: atom(),
                 function :: atom()}).

-record(call, {line :: integer(),
               func :: #atom{} | #remote{},
               args :: [_]}).

-record(op, {line  :: integer(),
             op    :: atom(),
             left  :: _,
             right :: _}).

-record(unop, {line  :: integer(),
               op    :: atom(),
               right  :: _}).

-record(index, {line      :: integer(),
                direction :: left | right,
                index     :: _,
                expr      :: _}).

-record(map, {line              :: integer(),
              name  = undefined :: atom(),
              exprs = []        :: [_],
              vars  = []        :: [#var{}]}).

-record('catch', {line :: integer(),
                  expr :: _}).

-record(match, {line  :: integer(),
                left  :: _,
                right :: _}).

-record(bin_element, {line :: integer(),
                      expr :: _,
                      size :: _,
                      type :: _}).

-record(bin, {line           :: integer(),
              elements  = [] :: [#bin_element{}]}).

-record(gen, {line  :: integer(),
              left  :: _,
              right :: _}).

-record(seq_gen, {line  :: integer(),
                  left  :: _}).

-record(map_c, {line         :: integer(),
                map          :: _,
                c_exprs = [] :: [_]}).

-record(bin_c, {line         :: integer(),
                bin          :: _,
                c_exprs = [] :: [_]}).

-record('receive', {line :: integer(),
                    clauses  = [] :: [#clause{}],
                    after_expr :: _,
                    after_body :: _}).
