-module(api_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    R = api_sup:start_link(),
    event_mgr:add_handler(),
    R.

stop(_State) ->
    ok.
