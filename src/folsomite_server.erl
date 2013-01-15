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
%%
%% Changes
%%   - Removed specific calls to graphite/riemann, now supports backends

%%%-------------------------------------------------------------------
%%% @doc Periodically dump reports from folsom metrics to backends
%%% Backends must have a start_link and send_metrics function.
%%% start_link([any()]) -> {ok, pid()}
%%% send_metrics(pid(), string(), metrics()) -> ok
%%%
%%% Where the string() is a string representation of the timestamp
%%% metrics() looks like:
%%% [{[k1, k2, ..., kn], any()} | ...]
%%%
%%% Where kN can be an atom, string, tuple of atom or string, or list
%%% of tuple or string.
%%% @end
%%%-------------------------------------------------------------------

-module(folsomite_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , code_change/3
        , terminate/2
        ]).


-define(APP, folsomite).
-define(TIMER_MSG, '#flush').
-define(RESTART_MSG, '#restart').

-record(s, { backends       :: [{module(), pid()}]
           , timer_ref      :: reference()
           , send_timer     :: module()
           , restart_ref    :: reference()
           }).


%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_arg, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init(no_arg) ->
    erlang:process_flag(trap_exit, true),
    {ok, undefined, 0}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_call(_Call, _From, S) ->
    {stop, bad_call, S}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_cast(_Cast, S) ->
    {stop, bad_cast, S}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, #s{backends = Backends} = S) ->
    case lists:keyfind(Pid, 2, Backends) of
        {M, Pid} ->
            error_logger:error_msg(
              "folsomite: Backend died: ~p Reason: ~p",
              [M, Reason]),
            Deleted = lists:keydelete(Pid, 2, Backends),
            {noreply, S#s{backends = Deleted}};
        false ->
            error_logger:error_msg(
              "folsomite: Unknown pid died: ~p Reason: ~p",
              [Pid, Reason]),
            {noreply, S}
    end;
handle_info(?RESTART_MSG, S) ->
    Backends = start_backends(S#s.backends),
    {noreply, S#s{backends = Backends}};
handle_info(?TIMER_MSG, S) ->
    send_metrics(S),
    {noreply, S};
handle_info(timeout, undefined) ->
    FlushIntr   = get_env(?APP, flush_seconds) * 1000,
    RestartIntr = get_env(?APP, backend_restart_seconds) * 1000,
    Backends    = start_backends([]),
    Ref         = timer:send_interval(FlushIntr,
                                      self(),
                                      ?TIMER_MSG),
    Restart_ref = timer:send_interval(RestartIntr,
                                      self(),
                                      ?RESTART_MSG),
    Send_timer  = get_env(?APP, send_timer_callback),
    State       = #s{ backends       = Backends
                    , timer_ref      = Ref
                    , send_timer     = Send_timer
                    , restart_ref    = Restart_ref
                    },
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
terminate(shutdown, #s{ timer_ref   = TimerRef
                      , restart_ref = RestartRef
                      }) ->
    timer:cancel_timer(TimerRef),
    timer:cancel_timer(RestartRef),
    ok.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
code_change(_, S, _) -> {ok, S}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
get_stats() ->
    Memory  = flatten(folsom_vm_metrics:get_memory(), [vm, memory]),
    Stats   = flatten(folsom_vm_metrics:get_statistics(), [vm]),
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
    flatten(M, [Name]);
expand_metric(_) ->
    [].

flatten([], _Name) ->
    [];
flatten([{K, V} | Rest], Name) ->
    KV = flatten(V, Name ++ [K]),
    KV ++ flatten(Rest, Name);
flatten({K, V}, Name) ->
    [{Name ++ [K], V}];
flatten(V, Name) ->
    [{Name, V}].

send_metrics(#s{send_timer = Send_timer} = S) ->
    Metrics   = get_stats(),
    Timestamp = num2str(unixtime()),
    F = fun ({M, Pid}) ->
                Send_timer:time(
                  M,
                  fun () -> M:send_metrics(Pid, Timestamp, Metrics) end)
        end,
    lists:foreach(F, S#s.backends).


num2str(NN) -> lists:flatten(io_lib:format("~w",[NN])).
unixtime()  -> {Meg, S, _} = os:timestamp(), Meg*1000000 + S.

get_env(Application, Name) ->
    {ok, Value} = application:get_env(Application, Name),
    Value.

get_backends(Application) ->
    lists:map(fun ({M, Args}) ->
                      {M, Args};
                  (M) ->
                      {M, []}
              end,
              get_env(Application, backends)).

start_backends(Backends) ->
    Module      = fun({M, _}) -> M end,
    AllBackends = get_backends(?APP),
    Missing     = sets:subtract(
                    sets:from_list(lists:map(Module, AllBackends)),
                    sets:from_list(lists:map(Module, Backends))),
    start_missing(Backends, AllBackends, Missing).


start_missing(RunningBackends, AllBackends, Missing) ->
    F = fun({M, Args}, Acc) ->
                case sets:is_element(M, Missing) of
                    true ->
                        start_missing_backend(M, Args, Acc);
                    false ->
                        Acc
                end
        end,
    lists:foldl(F, RunningBackends, AllBackends).

start_missing_backend(M, Args, Acc) ->
    try
        error_logger:info_msg("folsomite: Starting backend ~p", [M]),
        Pid = M:start_link(Args),
        [{M, Pid} | Acc]
    catch
        Type:Err ->
            error_logger:error_msg(
              "folsomite: Failed starting ~p Reason: ~p",
              [M, {Type, Err}]),
            Acc
    end.
