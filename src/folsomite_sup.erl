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
%% Changes:
%%   - Removed all starting of backends

-module(folsomite_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%% Management API functions
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

%% Supervisor callbacks
init(no_arg) ->
    Server = {folsomite_server,
              {folsomite_server, start_link, []},
              permanent,
              1000,
              worker,
              [folsomite_server]},
    {ok, {{one_for_all, 5, 3600}, [Server]}}.
