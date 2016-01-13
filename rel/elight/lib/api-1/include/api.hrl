%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2015, adoor balasubramanian
%%% @doc
%%%
%%% @end
%%% Created : 13 Jan 2015 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------

-type service_map()  :: dict:dict(K :: #serviceId{},
				  V :: #serviceProfile{}).

%%% @doc Performance Data record
%%% time series data
%%% syn_retrans may be used to explain some unusually large 
%%% connection latency
%%% connection latency =   t_syn_ack - t_syn
-record (perf_data, {	 
	   t_start :: #timeStamp{}, 
	   connection_latency = 0 :: latency(), 
	   syn_retrans = 0 :: non_neg_integer(), 
	   transactions = [] :: [#transaction{}]
	  }).

% @doc perf_db 
% active_since = lists:last(Sers)#ser.t_start
% last_active_time = hd(Sers)#ser.t_start
% Value = {SER summary, list of SERs}	
% SER Summary is the Session Profile
% In each record of perf_db we have Session Profile for a {Client, Server, Port, Proto} combination
% Protocol defaults to TCP
% will use a different DB for UDP
% Session event records of individual sessions are kept in  Sers
%% and are rolled up in SPC

-record(perf_db, { key :: #session{}, service_profile :: #serviceProfile{}, 
			     perf_data :: [#perf_data{}]
			    }).

	  
% @doc host_db
% stores Host Profiles (:: #hostProfile{})
% Each entry in host_db i.e. HostProfile corresponds to one host and
% can be thought of as the summary or rolled up information of
% the perf_db entries of that server  for all the clients

