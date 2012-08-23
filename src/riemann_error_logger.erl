%% Copyright (c) 2012 Campanja AB
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% A copy of the license is included in the file LICENSE.
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% @doc gen_event module that is an error_logger
%% that takes crash reports from error_logger/sasl
%% and forwards them to riemann.

-module(riemann_error_logger).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

%% API
-export([register_with_logger/0]).

-record(state, {node_prefix :: string()}).


register_with_logger() ->
    error_logger:add_report_handler(?MODULE).

init(_) ->
    State = #state{node_prefix = node_prefix()},
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({error, _GL, {_Pid, _FMT, Data}}, State) ->
    send_stats(State, Data),
    {ok, State};
handle_event({error_report, _GL, {_Pid, _Type, Data}}, State) ->
    send_stats(State, Data),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal functions
node_prefix() ->
    NodeList = atom_to_list(node()),
    [A, _] = string:tokens(NodeList, "@"),
    A.

send_stats(State, Data)->
    {FmtData, _Used} = lager_trunc_io:safe(Data,250),
    Hostname = net_adm:localhost(),
    Prefix = State#state.node_prefix ++ " ",
    zeta:cv({Hostname, Prefix ++ "erlang crash"}, 1, critical,
            [{tags,["transient", "erlang"]},{description, FmtData}]).
