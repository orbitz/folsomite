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
%%% @doc folsomite backend that pushes to graphite
%%% @end
%%%-------------------------------------------------------------------
-module(folsomite_graphite_backend).

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

-record(s, { host :: string()
           , port :: integer()
           , sock :: any()
           }).

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
init([Host, Port]) ->
    {ok, #s{host=Host, port=Port, sock=undefined}, 0}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_call(_Request, _From, S) ->
    {stop, bad_call, S}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_cast({send_metrics, Timestamp, Metrics}, #s{sock=Sock} = S) ->
    ok = gen_tcp:send(Sock, format_msg(Timestamp, Metrics)),
    {noreply, S}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_info(timeout, S) ->
    {ok, Sock} = gen_tcp:connect(S#s.host,
                                 S#s.port,
                                 [binary, {active, false}],
                                 5000),
    {noreply, S#s{sock=Sock}}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
terminate(_Reason, #s{sock=undefined}) ->
    ok;
terminate(_Reason, #s{sock=Sock}) ->
    gen_tcp:close(Sock),
    ok.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
format_msg(Timestamp, Metrics) ->
    [ [ "folsomite.", node_key(), ".", folsomite_utils:space2dot(K)
      , " "
      , folsomite_utils:any2l(V)
      , " "
      , Timestamp
      , "\n"
      ]
      || {K, V} <- Metrics
    ].

node_key() ->
    NodeList = atom_to_list(node()),
    Opts     = [global, {return, list}],
    re:replace(NodeList, "[\@\.]", "_", Opts).
