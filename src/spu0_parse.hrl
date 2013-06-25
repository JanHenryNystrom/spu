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

-record(attribute_p, {line  :: integer(),
                      name  :: atom(),
                      value :: _}).

-record(clause_p, {line           :: integer(),
                   name  = 'case' :: atom(),
                   args           :: [_],
                   guard          :: [_],
                   body           :: [_]}).

-record(fun_p, {line              :: integer(),
                module = '$local' :: atom(),
                function          :: atom() | #var{},
                arity             :: integer() | #var{}}).

-record(func_p, {line         :: integer(),
                 name = 'fun' :: atom(),
                 arity        :: integer(),
                 clauses      :: [#clause_p{}]}).

-record(nil_p, {line :: integer()}).

-record(cons_p, {line :: integer(),
                 car  :: _,
                 cdr  :: #nil_p{} | #cons_p{}}).

-record(case_p, {line    :: integer(),
                 expr    :: _,
                 clauses :: [#clause_p{}]}).

-record(remote_p, {line     :: integer(),
                   module   :: atom(),
                   function :: atom()}).

-record(call_p, {line :: integer(),
                 func :: #atom{} | #remote_p{},
                 args :: [_]}).

-record(op_p, {line  :: integer(),
               op    :: atom(),
               left  :: _,
               right :: _}).

-record(unop_p, {line  :: integer(),
                 op    :: atom(),
                 right  :: _}).

-record(index_p, {line      :: integer(),
                  direction :: left | right,
                  index     :: _,
                  expr      :: _}).

-record(map_p, {line              :: integer(),
                name  = undefined :: atom(),
                exprs = []        :: [_],
                vars  = []        :: [#var{}]}).

-record(catch_p, {line :: integer(),
                  expr :: _}).

-record(match_p, {line  :: integer(),
                  left  :: _,
                  right :: _}).

-record(bin_element_p, {line :: integer(),
                        expr :: _,
                        size :: _,
                        type :: _}).

-record(bin_p, {line           :: integer(),
                elements  = [] :: [#bin_element_p{}]}).

-record(gen_p, {line  :: integer(),
                left  :: _,
                right :: _}).

-record(seq_gen_p, {line  :: integer(),
                    left  :: _}).

-record(map_c_p, {line         :: integer(),
                  map          :: _,
                  c_exprs = [] :: [_]}).

-record(bin_c_p, {line         :: integer(),
                  bin          :: _,
                  c_exprs = [] :: [_]}).

-record(receive_p, {line :: integer(),
                    clauses  = [] :: [#clause_p{}],
                    after_expr :: _,
                    after_body :: _}).
