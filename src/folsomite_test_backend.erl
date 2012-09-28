%%%-------------------------------------------------------------------
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
%%% @doc Testing backend for folsomite
%%% @end
%%%-------------------------------------------------------------------
-module(folsomite_test_backend).

-behaviour(gen_server).

%% API
-export([ start_link/1
        , send_metrics/3
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(s, {table_name}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Args) ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []),
    Pid.

send_metrics(Pid, Timestamp, Metrics) ->
    ok = gen_server:cast(Pid, {send_metrics, Timestamp, Metrics}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init([Table_name]) ->
    Table_name = ets:new(Table_name, [named_table]),
    {ok, #s{table_name=Table_name}}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_call(_Request, _From, S) ->
    {stop, bad_call, S}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_cast({send_metrics, Timestamp, Metrics},
            #s{table_name=Table_name} = S) ->
    true = ets:insert(Table_name, {Timestamp, Metrics}),
    {noreply, S}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_info(_Info, S) ->
    {stop, S}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
terminate(_Reason, #s{table_name=Table_name}) ->
    ets:delete(Table_name),
    ok.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
