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

%%% periodically dump reports of folsom metrics to graphite
-module(folsomite_server).
-behaviour(gen_server).

%% management api
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).


-define(APP, folsomite).
-define(TIMER_MSG, '#flush').

-record(state, {flush_interval :: integer(),
                node_key       :: string(),
                node_prefix    :: string(),
                timer_ref      :: reference()}).


%% management api
start_link() ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_arg, []).

%% gen_server callbacks
init(no_arg) ->
    FlushInterval = get_env(flush_interval),
    Ref = erlang:start_timer(FlushInterval, self(), ?TIMER_MSG),
    State = #state{flush_interval = FlushInterval,
                   node_key = node_key(),
                   node_prefix = node_prefix(),
                   timer_ref = Ref},
    {ok, State}.

handle_call(Call, _, State) ->
    unexpected(call, Call),
    {noreply, State}.

handle_cast(Cast, State) ->
    unexpected(cast, Cast),
    {noreply, State}.

handle_info({timeout, _R, ?TIMER_MSG},
            #state{timer_ref = _R, flush_interval = FlushInterval} = State) ->
    Ref = erlang:start_timer(FlushInterval, self(), ?TIMER_MSG),
    F = fun() -> send_stats(State) end,
    folsom_metrics:histogram_timed_update({?APP, send_stats}, F),
    {noreply, State#state{timer_ref = Ref}};
handle_info({'EXIT', _, _} = Exit, State) ->
    {stop, {exit, Exit}, State};
handle_info(Info, State) ->
    unexpected(info, Info),
    {noreply, State}.


terminate(normal, #state{timer_ref = Ref} = State) ->
    erlang:cancel_timer(Ref),
    Terminate = prepare_event(State, "heartbeat", 1, [terminate]),
    zeta:sv_batch(Terminate).

code_change(_, State, _) -> {ok, State}.


%% internal
get_stats() ->
    Memory = expand0(folsom_vm_metrics:get_memory(), [memory, vm]),
    Stats = expand0(folsom_vm_metrics:get_statistics(), [vm]),
    Metrics = folsom_metrics:get_metrics_info(),
    Memory ++ Stats ++ lists:flatmap(fun expand_metric/1, Metrics).


expand_metric({Name, [{type, Type}]}) ->
    M = case Type of
            histogram ->
                proplists:delete(histogram,
                                 folsom_metrics:get_histogram_statistics(Name));
            Type1 ->
                case lists:member(Type1,
                                  [counter, gauge, meter, meter_reader]) of
                    true -> folsom_metrics:get_metric_value(Name);
                    false -> []
                end
        end,
    expand0(M, [Name]);
expand_metric(_) ->
    [].

expand0(M, NamePrefix) -> lists:flatten(expand(M, NamePrefix)).

expand({K, X}, NamePrefix) ->
    expand(X, [K | NamePrefix]);
expand([_|_] = Xs, NamePrefix) ->
    [expand(X, NamePrefix) || X <- Xs];
expand(X, NamePrefix) ->
    K = string:join(lists:map(fun a2l/1, lists:reverse(NamePrefix)), " "),
    [{K, X}].

send_stats(State) ->
    Metrics = get_stats(),
    Timestamp = num2str(unixtime()),
    Heartbeat = prepare_event(State, "heartbeat", 1, []),
    Events =
        [Heartbeat|[prepare_event(State, K, V, [transient]) ||
            {K, V} <- Metrics]],
    zeta:sv_batch(Events),
    Message = [format1(State#state.node_key, M, Timestamp) || M <- Metrics],
    case folsomite_graphite_client_sup:get_client() of
        {ok, Socket} -> folsomite_graphite_client:send(Socket, Message);
        {error, _} = Error -> Error
    end.

prepare_event(State, K, V, Tags) ->
    Hostname = net_adm:localhost(),
    Prefix = State#state.node_prefix,
    zeta:ev({Hostname, Prefix ++ " " ++ K}, V, ok, [{tags, [folsomite|Tags]}]).

format1(Base, {K, V}, Timestamp) ->
    ["folsomite.", Base, ".", space2dot(K), " ", a2l(V), " ", Timestamp, "\n"].

num2str(NN) -> lists:flatten(io_lib:format("~w",[NN])).
unixtime()  -> {Meg, S, _} = os:timestamp(), Meg*1000000 + S.


node_prefix() ->
    NodeList = atom_to_list(node()),
    [A, _] = string:tokens(NodeList, "@"),
    A.

node_key() ->
    NodeList = atom_to_list(node()),
    Opts = [global, {return, list}],
    re:replace(NodeList, "[\@\.]", "_", Opts).


a2l(X) when is_list(X) -> X;
a2l(X) when is_atom(X) -> atom_to_list(X);
a2l(X) when is_integer(X) -> integer_to_list(X);
a2l(X) when is_float(X) -> float_to_list(X);
a2l(X) when is_tuple(X) -> string:join([a2l(A) || A <- tuple_to_list(X)], " ").

space2dot(X) -> string:join(string:tokens(X, " "), ".").

get_env(Name) ->
    {ok, Value} = application:get_env(?APP, Name),
    Value.

unexpected(Type, Message) ->
    error_logger:info_msg(" unexpected ~p ~p~n", [Type, Message]).
