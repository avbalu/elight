%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2015, adoor balasubramanian
%%% @doc
%%% common functions to be shared by all eunit tests
%%% @end
%%% Created : 28 Jan 2015 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(test).
-export([setup/0, cleanup/1]).
-compile(export_all).
-include("../common.hrl").

setup() ->
    {ok,[[Home]]} = init:get_argument(home),
    LibDir = Home ++ "/elight/lib/",
    [code:add_path(LibDir ++ AppDir ++ "/ebin") || AppDir <- app_dirs()],
    [application:load(App) || App <- apps()],
    [ok = application:ensure_started(App) || App <- apps()],
    session_update().

cleanup(_) ->  
    [application:stop(App) || App <- apps()].
 
app_dirs() ->
    ["deps/eper",
     "filer",
     "configdb",
     "controller",
     "deps/thrift",
     "api",
     "monitor",
     "common"].

apps() ->
    [eper, filer, configdb, controller, thrift, api, monitor].

session_update() ->
    SU = #session_update{
	    session = 
		#session{
		   server = <<74,125,239,134>>,
		   port = 80,
		   client = <<10,0,0,8>>},
	    t_start = #timeStamp{seconds = 1419926105, micro = 346530},
	    latency = 0,
	    t_end = #timeStamp{seconds = 1419926105, micro = 346530},
	    end_cause = timeout,error_code = no_syn_ack,syn_retrans = 0,
	    transactions = []},
    admin:session_update(SU).
    
    
