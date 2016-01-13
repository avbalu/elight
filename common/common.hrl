%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2014, adoor balasubramanian
%%% @doc
%%%
%%% @end
%%% Created : 28 Dec 2014 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------

-include("../api/gen-erl/api_types.hrl").
% -define(dbg(L), error_logger:info_report(debug_message, 
%					[{module, ?MODULE},
%					{line, ?LINE} | L])).
-define(dbg(L), io:format("~p:~p : ~p~n", [?MODULE, ?LINE, L])).
-define(rec_info(T,R),lists:zip(record_info(fields,T),
				tl(tuple_to_list(R)))).
-define(IP_MIN_HDR_LEN, 5).
-define(IP_PROTO_TCP, 6).
-define(TCP_MIN_HDR_LEN, 20).
-define(MSG_TIMEOUT, 8000). % 8s
-define(PCAP_FILE_HDR, <<16#a1b2c3d4:32,
			     2:16,
			     4:16,
			     0:32,
			     0:32,
			     65535:32,
			     1:32>> ).
-define(US_IN_S, 1000000). % micro seconds in seconds

-type pcap_file() :: [<<_:192>> | [binary()]].


-type error_code() :: normal | 
		      no_syn_ack |
		      no_ack_syn_ack |
		      no_client_request |
		      no_server_response.

-type end_cause() :: timeout |
		     c_fin |
		     s_fin |
		     c_rst |
		     s_rst.

-type time() :: #timeStamp{}.
%-type time() :: {Seconds :: non_neg_integer(),
%		 Micro :: non_neg_integer()}.

%-type perf() :: {#timeStamp{}, integer()}.
-type ip_address() :: binary().

-type host() :: client() | server().

-type http_response_code() :: pos_integer().


-type client() :: ip_address().
-type server() :: ip_address().

-type latency()    :: integer(). % in microseconds

-type config_object() :: atom().

-type flag() :: 0 | 1.

-type seq() :: non_neg_integer().

-type server_port() :: non_neg_integer().

%% @doc 
%% time relative to session start time (t - t-start)
%% unit is microseconds
%-type relative_#timeStamp{} :: integer().

%% @doc session
%% @doc session
%% TCP session


-record(session, {server   :: server(),
		  port  :: pos_integer(),
		  client :: client()
		 }
       ).
			      

-record(five_tuple, {sip :: ip_address(),
		     dip :: ip_address(), 
		     proto :: ?IP_PROTO_TCP, 
		     sp :: pos_integer(), 
		     dp :: pos_integer()}).

-record(msg, 
	{ ts :: #timeStamp{},
	  len  :: pos_integer(), % actual length on the wire
	  sip :: ip_address(),
	  dip :: ip_address(),
	  sp :: non_neg_integer(),
	  dp :: non_neg_integer(),
	  seq :: non_neg_integer(),
	  ack :: non_neg_integer(),
	  a :: flag(),
	  r :: flag(),
	  s :: flag(),
	  f :: flag(),
	  data_len :: non_neg_integer(),
	  pcap_record :: binary() % whole frame as captured
	}).

-record(init_msg, {sniffer :: pid(),
		  session_key :: tuple(),
		  filters = [#filter{}],
		  msg :: #msg{}
}).

%%% @ doc perf record
%%%  rx_bytes, rx_frames are logged against the request time
%%%  tx_bytes, tx_frames are logged against the response time
%%% TCP control packets are not included in the above

	  
%% @doc transaction

-record(transaction, { 
	  t_start :: time(), 
	  latency  :: latency(),
	  rx_bytes = 0 :: non_neg_integer(),
	  rx_frames = 0 :: non_neg_integer(),
	  tx_bytes = 0 :: non_neg_integer(),
	  tx_frames = 0 :: non_neg_integer(),
	  response_code :: normal | integer(),
	  retrans = 0 :: integer() % req retransmissions
	 }).

% @doc session_update
% message sent from session to admin
% syn_retrans : The idea is to see if
% any large connection setup delay is due to SYN packet loss
% latenct = t_syn_ack - t_syn
% setup is the setup time for intermediate layer between 
% connection setup and application- e.g. ssl handshake

-record(session_update, {
	  session :: #session{},
	  setup = 0 :: latency(),
	  t_start :: time(),
	  latency :: latency(),
	  t_end :: time(),
	  end_cause :: end_cause(),
	  error_code :: error_code(),
	  syn_retrans = 0 :: non_neg_integer(),
	  transactions = [] :: [#transaction{}]
	 }).

-record (config, { key     :: atom(),
		   value   :: any()
		 }
	).

-type debug_session() :: terminate | unexpected.
-record(debug_session, {key :: debug_session(),
		      matched = 0 :: integer()
		     }).


-record(log_notify, {t :: #timeStamp{},
		     session :: #session{},
		     filter :: #filter{},
		     pcap_records :: [binary()]}).
