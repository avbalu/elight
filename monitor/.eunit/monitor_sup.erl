%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2014, adoor balasubramanian
%%% @doc
%%%
%%% @end
%%% Created : 28 Dec 2014 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(monitor_sup).

-behaviour(supervisor).

-include("../common/common.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,

    Child1 = {monitor_sup1, {monitor_sup1, start_link, []},
	      Restart, Shutdown, supervisor, [monitor_sup1]},
    ok = supervisor:check_childspecs([Child1]),
    Child2 = {creator, {creator, start_link, []},
	      Restart, Shutdown, worker, [creator]},
    Child3 = {monitor, {monitor, start_link, []},
	      Restart, Shutdown, worker, [monitor]},
    ok = supervisor:check_childspecs([Child1,Child2, Child3]),
    {ok, {SupFlags, [Child1,Child2, Child3]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
