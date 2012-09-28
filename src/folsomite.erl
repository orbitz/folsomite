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
%%   - Changed to ensure_started and folsom is started as well

-module(folsomite).
-export([start/0]).

%% api
start() -> ensure_started([folsom, folsomite]).

%% internal

ensure_started(Apps) ->
    lists:iter(fun (App) ->
                       case application:start(App) of
                           ok                   -> ok;
                           {already_started, _} -> ok
                       end
               end,
               Apps).
