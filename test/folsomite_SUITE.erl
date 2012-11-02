%%%=============================================================================
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% A copy of the license is included in the file LICENSE.
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @doc folsomite test suite
%%% @end
%%%=============================================================================

-module(folsomite_SUITE).

%% Callbacks
-export([ all/0
        , init_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , end_per_suite/1
        ]).


%% Test cases
-export([add_counter/1]).

%% Includes
-include_lib("common_test/include/ct.hrl").

%% Defines
-define(TABLE, foo_table).

%% Flush every second
-define(FLUSH_INTERVAL, 1).

%%%===================================================================
%%% API
%%%===================================================================
all() -> [ add_counter ].

init_per_suite(Config) ->
    application:load(folsomite),
    application:set_env(folsomite, flush_seconds, ?FLUSH_INTERVAL),
    application:set_env(folsomite,
                        backends,
                        [ {folsomite_test_backend, [?TABLE]}
                        ]),
    application:set_env(folsomite,
                        send_timer_callback,
                        folsomite_timer_dummy),
    ok = application:start(folsom),
    ok = application:start(folsomite),
    Config.

end_per_suite(Config) ->
    application:stop(folsomite),
    application:stop(folsom),
    Config.

init_per_testcase(TestCase, Config) ->
  ?MODULE:TestCase({init, Config}).

end_per_testcase(TestCase, Config) ->
  ?MODULE:TestCase({'end', Config}).

add_counter(doc) ->
    "Adds a counter to folsom then waits until it shows up in "
    "folsomite or a timeout beats it";
add_counter({init, Config}) ->
    Config;
add_counter({'end', Config}) ->
    Config;
add_counter(_Config) ->
    [] = folsom_metrics:get_metrics(),
    folsom_metrics:notify([foo, bar], {inc, 1}, counter),
    true = lists:member([foo, bar], folsom_metrics:get_metrics()),
    %% Yes yes, EVIL
    timer:sleep(2000),
    All  = ets:foldl(fun (E, Acc) -> [E | Acc] end, [], ?TABLE),
    {_Timestamp, Metrics} = hd(lists:reverse(lists:sort(All))),
    {[[foo, bar]], 1} = proplists:lookup([[foo, bar]], Metrics),
    ok.



