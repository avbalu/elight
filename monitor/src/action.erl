%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2015, adoor balasubramanian
%%% @doc
%%%
%%% @end
%%% Created : 25 Jan 2015 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(action).

-export([act/3, store/3]).


-include("../../common/common.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../../api/gen-erl/api_types.hrl").
-include("../../filer/include/filer.hrl").
-include("../../api/include/event_mgr.hrl").


-spec act( Session :: #session{},
	   PcapRecords :: [binary()], 
	   Match :: #filter{} | none) ->
		 ok.
act(Session, PcapRecords, 
    #filter{id = Id, 
	    action = #action{min = Min, 
			     logSession = Log,
			     notify = N}} = Filter) ->
    Matched = ets:update_counter(filterStats, Id, #filterStats.matched),
    
    Min > Matched orelse Matched > Min+Log  orelse 
	store1(PcapRecords, Filter, Matched),
    Min > Matched orelse Matched > Min+N  orelse
	notify(Session, Filter).


store1(PcapRecords, #filter{id = Id} = Filter, Matched) ->
    Name = lists:flatten(io_lib:format("filter/~w_~w", [Id, Matched])),
    PcapFile = [?PCAP_FILE_HDR, lists:reverse(PcapRecords)],
    Descriptor = io_lib:format("~p", [Filter]),
    store(Name, PcapFile, Descriptor).

-spec store(Name :: string(),
	    PcapFile :: iodata(),
	    Descriptor :: iodata()) ->
		   ok.
store(Name, PcapFile, Descriptor) ->
    filer:store([#file{name = Name ++ ".pcap",
		       data = PcapFile},
		 #file{name = Name ++ ".descriptor",
		       data = Descriptor}]).

    

-spec notify(Session :: #session{}, Filter :: #filter{}) -> ok.

notify(Session, Filter) ->
    event_mgr:notify(#filter_event{session = Session, 
				   filter = Filter}).

