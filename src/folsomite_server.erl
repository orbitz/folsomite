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
%%   - Removed specific calls to graphite/reimann, now supports backends

%%%-------------------------------------------------------------------
%%% @doc Periodically dump reports from folsom metrics to backends
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

-record(s, { flush_interval  :: integer()
           , backends        :: [{module(), pid()}]
           , timer_ref       :: reference()
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
    FlushInterval = get_env(?APP, flush_seconds) * 1000,
    Backends      = [ begin
                          Pid = M:start_link(Args),
                          {M, Pid}
                      end
                      || {M, Args} <- get_backends(?APP)],
    Ref           = erlang:start_timer(FlushInterval,
                                       self(),
                                       ?TIMER_MSG),
    State         = #s{ flush_interval = FlushInterval
                      , backends       = Backends
                      , timer_ref      = Ref
                      },
    {ok, State}.

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
handle_info({timeout, R, ?TIMER_MSG},
            #s{ timer_ref = R
              , flush_interval = FlushInterval} = S) ->
    Ref = erlang:start_timer(FlushInterval, self(), ?TIMER_MSG),
    F   = fun() -> send_metrics(S) end,
    folsom_metrics:histogram_timed_update({?APP, send_metrics}, F),
    {noreply, S#s{timer_ref = Ref}}.


%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
terminate(shutdown, #s{timer_ref = Ref}) ->
    erlang:cancel_timer(Ref),
    ok.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
code_change(_, S, _) -> {ok, S}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
get_stats() ->
    Memory  = expand0(folsom_vm_metrics:get_memory(), [memory, vm]),
    Stats   = expand0(folsom_vm_metrics:get_statistics(), [vm]),
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
    K = string:join(lists:map(fun folsomite_utils:any2l/1, lists:reverse(NamePrefix)), " "),
    [{K, X}].

send_metrics(S) ->
    Metrics   = get_stats(),
    Timestamp = num2str(unixtime()),
    lists:foreach(fun ({M, Pid}) ->
                          M:send_metrics(Pid, Timestamp, Metrics)
                  end,
                  S#s.backends).


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
