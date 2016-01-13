%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2015, adoor balasubramanian
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2015 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(monitor_tests).


-include_lib("eunit/include/eunit.hrl").
-include("../../api/gen-erl/api_types.hrl").
-include("../../filer/include/filer.hrl").
-include("../../common/common.hrl").
-include("../../api/include/event_mgr.hrl").

-compile(export_all).

-import(monitor, [filter_event/1, t_normalize/2]).
-import(config, [del_filters/2, add_filters/2, dbs/0]).

setup() ->
    code:add_path("../../common/ebin"),
    test:setup(),
    Port = configdb:get(config_tcp_port),
    {ok, C0} = thrift_client_util:new( "localhost", 
				       Port, 
				       config_thrift,[]),
    Filters = [#filter{id = 1, condition 
		       = #condition{client 
				    = #network{ipAddress = <<10,0,0,8>>,
					       mask = 32}}, 
		       action = #action{logSession = 1, notify = 1}}],
    [ begin 
	  catch del_filters(X, [1]),
	  thrift_client:call(C0, add_filters,[X, Filters])
      end || X <- dbs()],
    #session{client = <<10,10,10,1>>, 
		       server = <<192,168,0,1>>,
		       port = 80}.
   

cleanup(X) ->
    test:cleanup(X).

unit_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun test/1
    }.

test(X) ->
    [?_test(log_notify_1(X)),
     ?_test(log_notify_2(X)),
     ?_test(log_notify_negative(X)),
     ?_assert(t_normalize(1,900000) =:= {1,900000}),
     ?_assert(t_normalize(1,1000000) =:= {2,0}),
     ?_assert(t_normalize(2,2111111) =:= {4,111111}),
     ?_test(debounce(X)),
     ?_test(debounce_negative(X))].

log_notify_1(Session) ->
    Filter =  #filter{id = 1,
		      action = #action{min = 0,
				       logSession = 1,
				       notify = 100}},
    Msg = #log_notify{session = Session,
		      filter = Filter,
		      pcap_records = [<<"eunit">>]},
    filter_event(Msg).

log_notify_2(Session) ->
    Filter =  #filter{id = 2,
		      action = #action{min = 2,
				       logSession = 2,
				       notify = 100}},
    Msg = #log_notify{session = Session,
		      filter = Filter,
		      pcap_records = [<<"eunit">>]},
    filter_event(Msg),
    filter_event(Msg),
    filter_event(Msg).

log_notify_3(Session) ->
    Filter =  #filter{id = 3,
		      action = #action{min = 2,
				       logSession = 2,
				       notify = 100}},
    Msg = #log_notify{session = Session,
		      filter = Filter,
		      pcap_records = [<<"eunit">>]},
    [filter_event(Msg) || _ <- lists:seq(5)].

log_notify_negative(Session) ->
    Filter =  #filter{id = 4,
		      action = #action{min = 2,
				       logSession = 1,
				       notify = 100}},
    Msg = #log_notify{session = Session,
		      filter = Filter,
		      pcap_records = [<<"eunit">>]},
    filter_event(Msg).

debounce(Session) ->
    Filter =  #filter{id = 5,
		      action = #action{min = 3,
				       debounce = 1500000, %1.5 s
				       logSession = 1,
				       notify = 100}},
    Ts = #timeStamp{seconds=1,micro = 10},
    Msg = #log_notify{t = Ts,
		      session = Session,
		      filter = Filter,
		      pcap_records = [<<"eunit debounce">>]},
    filter_event(Msg),
    filter_event(Msg),
    Te = #timeStamp{seconds=2,micro = 10},
    filter_event(Msg#log_notify{t = Te}).

debounce_negative(Session) ->
    Filter =  #filter{id = 6,
		      action = #action{min = 3,
				       debounce = 1500000, %1.5 s
				       logSession = 1,
				       notify = 100}},
    Ts = #timeStamp{seconds=1,micro = 10},
    Msg = #log_notify{t = Ts,
		      session = Session,
		      filter = Filter,
		      pcap_records = [<<"eunit debounce">>]},
    filter_event(Msg),
    filter_event(Msg),
    Te = #timeStamp{seconds=2,micro = 900000},
    filter_event(Msg#log_notify{t = Te}).
