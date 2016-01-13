%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2014, adoor balasubramanian
%%% @doc
%%% The end_cause in #session_data{} represents the reason for session termination
%%% When the session ends in aw_response state, the end_cause is not 
%% normal even if some transactions ahve been completed succussfully 
%% in the session.

%%% when timeout occurs beforesession setup or
%%% before any transaction has completed or while in aw_response state, 
%%% end_cause will be set to timeout

%%% in case of timeout, t_last_msg  is used to calculate session duration
%%% 
%%% @end
%%% Created : 30 Dec 2014 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(session).

-behaviour(gen_fsm).

%% API
%% API
-export([start_link/1, send/2]).

%% gen_fsm callbacks
-export([init/1, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% state functions
-export([aw_syn_ack/2, aw_ack_syn_ack/2, established/2, aw_first_request/2,
	aw_first_response/2, aw_request/2, aw_response/2]).

-include("../../common/common.hrl").
-include("../../filer/include/filer.hrl").
-include("../../api/include/event_mgr.hrl").


%%%===================================================================
%%% gen fsm API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_fsm:start_link(?MODULE, Args, []).

%%%===================================================================
%%% Session API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%% Event is a a message containing
%%% packets from Network Device or pcap file
%%% This API is used by sniffer to send messages to already created
%%% session.
-spec send(FsmRef :: pid(), Event :: #msg{}) -> ok.
send(FsmRef, Event) when is_pid(FsmRef)->
    gen_fsm:send_all_state_event(FsmRef, Event).

%%%===================================================================
%%% Internal Data Strutures
%%%===================================================================



%%% doc state()
%%% session can be in any one of these states.
-type state() :: aw_syn_ack | 
		 aw_ack_syn_ack | 
		 first_req |
		 established |
		 aw_first_request |
		 aw_first_response |
		 aw_response |
		 aw_request |
		 idle.




%%% Doc
%%% session_data - Session Data 
%%% Internal state maintained by the fsm
%%% Sequence Numbers are 
%% used to indetify retransmissions
%% seq and ack numbers are used to match response to request
%%% cseq0, sseq0 - client & server initial sequence numbers. These are 
%%% as they are receivced from syn and syn_ack
%%% All other sequence numbers are stored relative to these.
%%% cseq - sequence number from cleint packet last seen.
%%% cseq_next - ack number from client packet last seen
%%% sseq, seq_next are for the server
%%% states - list containing states traversed by fsm - for debugging
%% transaction is used to build stats for the current transaction
%%    it is moved to transactions when one of the following two events 
%% happen
%%      1) A new client request (second or later) comes in, 
%%         to make room for next transaction
%%      2) When session ends, if teh current transaction is complete
%% transactions are in reverse chrnological order
%%%  

-record(session_data, { session_key :: tuple(),
		 sniffer :: pid(),
		 session :: #session{},
		 t_start :: time(),
		 latency :: latency(),
		 end_cause :: end_cause(),
		 retrans = 0 :: non_neg_integer(),
		 transaction = #transaction{} :: #transaction{},
		 transactions = [] :: [#transaction{}],

		 cseq0 :: non_neg_integer(),
		 cseq :: non_neg_integer(),
		 cseq_next :: non_neg_integer(),

		 sseq0 :: non_neg_integer(),
		 sseq :: non_neg_integer(),
		 sseq_next :: non_neg_integer(),

		 states = [] :: [state()], % in reverse order

		 t_last_msg :: time(),
		 pcap_records :: [binary()],
		 filters :: [#filter{}]
	       }).

%% @doc next_state
%% returned by each state function as expected by gen_fsm behaviour.
-record(next_state, {
	  name :: state(),
	  session_data :: #session_data{},
	  timeout = ?MSG_TIMEOUT :: ?MSG_TIMEOUT}).

-record(session_end, {cause :: end_cause() }).		 

-record(syn_ack, {t :: #timeStamp{},
		  seq :: seq()}).

-record(c_ack, {rseq :: seq(),
		rack :: seq()}).

-record(s_ack,  {rseq :: seq(),
		rack :: seq()}).

-record(c_pkt, {rseq :: seq(),
		 rack :: seq(),
		 l :: non_neg_integer(),
		 t :: #timeStamp{}}).

-record(s_pkt, {rseq :: seq(),
		 rack :: seq(),
		 l :: non_neg_integer(),
		 t :: #timeStamp{}}).


%%% @doc event
%%% used to transfer information from handle_cast to state functions

-type state_event() :: 'ack_syn_ack' | 'syn' | #session_end{} | #c_ack{} | #s_ack{} | #syn_ack{} | #c_pkt{} | #s_pkt{}.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------

init(#init_msg{ sniffer = Name,
		session_key = Key,
		filters = Filters,
		msg = #msg{ts = T, 
			   sip = SIP, 
			   dip = DIP, 
			   dp = DP,
			   seq = Seq,
			   pcap_record = PcapRec}}) ->
	    {ok, aw_syn_ack, 
	     #session_data{ sniffer = Name,
		     session_key = Key,
		     session = #session{client = SIP, 
					server = DIP, 
					port = DP},
		     t_start = T,
		     cseq0 = Seq,
		     cseq = 1,
		     cseq_next = 1,
		     t_last_msg = T,
		     pcap_records = [PcapRec],
		     filters = Filters
		   },
	     ?MSG_TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%% In all States we need to take care of 
%%
%% timeout
%% session end (FIN/RST)
%% retransmission,
%% just ACK without Data  
%% TCP fragments/ message from unexpected direction
%% normal case
%% unexpected message
%% 
%% normal case - one that moves the fsm forward

%% @spec   aw_syn_ack(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

%% @doc aw_syn_ack
%% syn message already received as part of session init
-spec aw_syn_ack(Event :: timeout | state_event(), SDB :: #session_data{}) -> 
			R :: #next_state{}.
%% timeout
aw_syn_ack(timeout, SessionData) ->
    timeout(SessionData, no_syn_ack);

%% session end (FIN/RST)
aw_syn_ack(#session_end{cause = Cause}, SD) ->
    end_session(Cause, SD, no_syn_ack);

%% SYN retransmission
aw_syn_ack(syn, SD) ->
    #next_state{name = aw_syn_ack,
		session_data = control_retrans(SD)};

%% normal case
aw_syn_ack( #syn_ack{t = T, seq = Seq}, 
	    #session_data{t_start = T0} = SD) ->
    #next_state{name = aw_ack_syn_ack,
		session_data = SD#session_data{ latency = latency(T0,T),
					    sseq0 = Seq,
					    sseq = 1,
					    sseq_next = 1}}.

%%@@ @doc aw_ack_syn_ack
-spec aw_ack_syn_ack(Event :: timeout | state_event(), SDB :: #session_data{}) -> 
			R :: #next_state{}.

%% timeout
aw_ack_syn_ack(timeout, SD) ->
    timeout(SD, no_ack_syn_ack);


%% session end (FIN/RST)
aw_ack_syn_ack(#session_end{cause = Cause}, SD) ->
    end_session(Cause, SD, no_client_request);

%% retransmission,
aw_ack_syn_ack(Event, SD) 
  when Event =:= syn; is_record(Event, syn_ack)->    
    #next_state{name = aw_ack_syn_ack,
		session_data = control_retrans(SD)};

%% normal case
aw_ack_syn_ack(ack_syn_ack, SD) -> 
    #next_state{name = established,
		session_data = SD}.

%---------------------------
%% @doc established
%% control message retransmission (synAck/AckSynAck) possible
%% regular ACK not possible 
%% Client Request or Server Greetings is the normal case
%----------------------------------
-spec established(Event :: timeout | state_event(), SDB :: #session_data{}) -> 
			R :: #next_state{}.

%% timeout
established(timeout, SD) ->
    timeout(SD, no_client_request);

%% session end (FIN/RST)
established(#session_end{cause = Cause}, SD) ->
    end_session(Cause, SD, no_client_request);

%% retransmission,
established(#syn_ack{}, SD) ->
    #next_state{name = established,
		session_data = control_retrans(SD)};
established(ack_syn_ack, SD) ->
    #next_state{name = established,
		session_data = control_retrans(SD)};
    
%% normal case
%% Server Greeting
%% unsolicited
%% usually comes before client request
established(#s_pkt{rseq = 1, rack = 1, l = L}, SD) ->
    #next_state{name = aw_first_request,
		session_data = tx(SD, L)};
    


%% normal case
%% client's first Request
established(#c_pkt{t = T, rseq = 1, rack = 1, l = L}, SD) ->    
    NewSD = rx(SD#session_data{
		    transaction = #transaction{t_start = T}},
		  L),
    #next_state{name = aw_first_response,
		session_data = NewSD}.

%%------------------------------------------------------
%% @doc aw_first_request
%% no control message retransmissions 
%% ACK only packets possible but only from Client 
%%     possibly to server greetins
%% Server greetings retransmission possible.
%%    However, we do not update retrans count as it does not 
%% No retrans from client
%%    response time or first request time
%% normal case is client request
%%------------------------------
-spec aw_first_request(Event :: timeout | state_event(), 
		       SDB :: #session_data{}) -> 
			      R :: #next_state{}.

%% timeout
aw_first_request(timeout, SD) ->
    timeout(SD, no_client_request);

%% session end (FIN/RST)
aw_first_request(#session_end{cause = Cause}, SD) ->
    end_session(Cause, SD, no_client_request);

%% retransmission
%% Server greetings
aw_first_request(
  #s_pkt{rseq = Seq, l = L},
  #session_data{sseq = Seq} = SD) ->
    NewSD = data_retrans(tx_stats(SD, L)),
    #next_state{name = aw_first_request,
		session_data = NewSD};

%% just ACK without Data  
%% from client to server greetings
aw_first_request(
  #c_ack{rseq = 1, rack = Ack},
  #session_data{sseq_next = Ack} = SD) ->
    #next_state{name = aw_first_request, session_data = SD};

%% TCP fragments/ message from unexpected direction
%% We assume that serevr greetings usually fits in one TCP Segment

%% normal case
%% client Request
aw_first_request(
  #c_pkt{t = T, rseq = 1, rack = Ack, l = L},
  #session_data{sseq_next = Ack} = SD) ->
    NewSD = rx(SD#session_data{ 
		    transaction = 
			#transaction{t_start = T}}, 
		  L),
    #next_state{name = aw_first_response,
		session_data = NewSD}.

%%------------------
%% @doc aw_first_response
%% normal message is server response
%% retransmission of client request
%% Ack only from Client for Server greetingsis possible, 
%%   however, we assume that usually Server greeings, if any,
%%   usually gets delivered before 
%% Ack only from Server is possible and is usual
%%-----------------
-spec aw_first_response(Event :: timeout | state_event(), 
			   SDB :: #session_data{}) -> 
				  R :: #next_state{}.

%% timeout
aw_first_response(timeout, SD) ->
    timeout(SD, no_server_response);


%% session end (FIN/RST)
aw_first_response(#session_end{cause = Cause}, SD) ->
    end_session(Cause, SD, no_server_response);

%% just ACK without Data  
%% ack from server for client request
aw_first_response(
  #s_ack{rseq = Seq},
  #session_data{sseq_next = Seq} = SD) ->
    #next_state{name = aw_first_response, session_data = SD};
    

%% retransmission
%% client request
aw_first_response(
  #c_pkt{rseq = 1, rack = Ack, l = L},
  #session_data{sseq_next = Ack} = SD) ->
    #next_state{name = aw_first_response,
		session_data = rx_stats(data_retrans(SD), L)};

%% TCP fragments/ message from unexpected direction
%% case - first request did not fit in a single TCP segment
%%      say , for e.g. HTTP Post
aw_first_response(
  #c_pkt{rseq = Seq, rack = Ack, l = L},
  #session_data{cseq_next = Seq, sseq_next = Ack} = SD) ->
    #next_state{name = aw_first_response,
	       session_data = rx(SD,L)};


%% normal case
%% first response from server
aw_first_response(
  #s_pkt{t = T, rseq = Seq, rack = Ack, l = L},
  #session_data{t_start = T0, sseq_next = Seq, 
		transaction = Tr, cseq_next = CSeqN} = SD) 
  when Ack =:= CSeqN -> 
    NewTr = Tr#transaction{latency = latency(T0, T)},
    NewSD = SD#session_data{transaction = NewTr},
    #next_state{name = aw_request, 
		session_data = tx(NewSD, L)}.

%%--------------------------------------
%% @doc aw_request
%% Entry Condition - length(transactions) 
%%                      == #of times this state has been transitioned to, 
%%                          including this one
%%                   transaction = undefined
%% Re-entry condition - length(transactions) > 1
%%                      transaction = undefined
%% Ack only packets possible but only from Client
%%    This is for previous server response
%% Retransmission possible but from server only
%%     This is previous server response
%% Normal case - Client Request or Session End
%% Fragments from Server also possibe 
%%         This will be the case when previous server response
%%         did not fit in single segment
%%--------------------------------------
-spec aw_request(Event :: timeout | state_event(), 
			   SDB :: #session_data{}) -> 
				  R :: #next_state{}.

%% timeout
aw_request(timeout, SD) ->
    timeout(SD, normal);

%% session end (FIN/RST)
aw_request(#session_end{cause = Cause}, SD) ->
    end_session(Cause, SD, normal);


%% retransmission
%% server response
aw_request(#s_pkt{rseq = Seq, l = L},
	   #session_data{sseq_next = SSeqN} = SD) 
  when Seq < SSeqN->
    #next_state{name = aw_request,
		session_data = tx_stats(data_retrans(SD), L)};
    
%% just ACK without Data  
%% from Client
aw_request(#c_ack{rseq = Seq, rack = Ack},
	   #session_data{cseq_next = Seq, sseq_next = SSeqN} = SD)
  when Ack =< SSeqN ->
    #next_state{name = aw_request,
		session_data =  SD};

%% just ACK without Data  
%% from Server
%% This is possible - e.g. lib/monitor/test/4.pcap
aw_request(#s_ack{rseq = Seq, rack = Ack},
	   #session_data{cseq_next = CSeqN, sseq_next = Seq} = SD)
  when Ack =< CSeqN ->
    #next_state{name = aw_request,
		session_data =  SD};

  
%% TCP fragments
%% from server
aw_request(#s_pkt{rseq = Seq, rack = Ack, l = L},
	   #session_data{cseq_next = CSeqN, sseq_next = Seq} = SD) 
  when Ack =< CSeqN ->
    #next_state{name = aw_request,
		session_data = tx(SD, L)};

%% loss of server packet(s)
%% elight has missed one or more server pkts corresponding to Seq - SSeqN
%% This may result in loss of some data points
%% However this does not affect the accuracy of data points measured.
aw_request(#s_pkt{rseq = Seq, rack = Ack, l = L},
	   #session_data{cseq_next = CSeqN, sseq_next = SSeqN} = SD) 
  when Ack =< CSeqN,  Seq > SSeqN ->
    #next_state{name = aw_request,
		session_data = tx(SD, L)};
    
%% normal case
%% client request
aw_request(#c_pkt{t = T, rseq = Seq, l = L},
	   #session_data{cseq_next = Seq,
			 transaction = Tr,
			 transactions = Trs} = SD) ->
    NewSD = SD#session_data{transaction = #transaction{t_start = T},
			    transactions = tr(Tr,Trs)},
    #next_state{name = aw_response,
		session_data = rx(NewSD, L)}.

    
%%------------------------------------
%% @doc aw_response
%%------------------------------

-spec aw_response(Event :: timeout | state_event(), 
			   SDB :: #session_data{}) -> 
				  R :: #next_state{}.

aw_response(timeout, SD) ->
     timeout(SD, no_server_response);

%% session end (FIN/RST)
aw_response(#session_end{cause = Cause}, SD) ->
    end_session(Cause, SD, no_server_response);


%% retransmission
%% client request
aw_response(#c_pkt{rseq = Seq, l = L},
	    #session_data{cseq = Seq} = SD) ->
    NewSD = rx_stats(data_retrans(SD), L),
    #next_state{name = aw_response,
		session_data =  NewSD};

     
%% just ACK without Data  
%% ack from server for client request
aw_response(#s_ack{rseq = Seq, rack = Ack},
	    #session_data{sseq_next = Seq, cseq_next = CSeqN} = SD)
  when Ack =< CSeqN ->
    #next_state{name = aw_response,
		session_data =  SD};

    
%% TCP fragments/ message from unexpected direction
%% case -  request did not fit in a single TCP segment
%%      say , for e.g. HTTP Post
aw_response(#c_pkt{rseq = Seq, l = L},
	    #session_data{cseq_next = Seq} = SD) ->
    #next_state{name = aw_response,
		session_data =  rx(SD, L)};
    

%% normal case
%% response from server
aw_response(#s_pkt{t = T, rseq = Seq, rack = Ack, l = L},
	    #session_data{t_start = T0, transaction = Tr,
			  sseq_next = Seq, cseq_next = CSeqN} = SD)
  when Ack =:= CSeqN ->
    NewTr = Tr#transaction{latency = latency(T0, T)},
    NewSD = SD#session_data{transaction = NewTr},
    #next_state{name = aw_request,
		session_data =  tx(NewSD, L)};


%% elight server packet loss
%% first fragment of the response has been missed.
%% This data point will skew the response time measurement
%% record this info and
%% drop this data point
aw_response(#s_pkt{rseq = Seq, rack = Ack},
	    #session_data{sseq_next = SSeqN, 
			  cseq_next = CSeqN} = SD)
  when Ack =:= CSeqN, Seq > SSeqN ->
    #next_state{name = aw_request,
		session_data =  SD}.

    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(Msg :: #msg{}, 
		   State :: atom(),
%		   State :: fun((_,_) -> any()), 
		   SD :: #session_data{}) ->
			  R :: #next_state{}.
handle_event(#msg{ts = T, pcap_record = PcapRecord, sip = SIP} = Msg, 
	     State,
	     #session_data{pcap_records = PcapRecords,
			   session = #session{client = C}} = SD) ->
		    ClientPkt = SIP =:= C,
		    NewSD = 
			SD#session_data{ t_last_msg = T,
					 pcap_records = 
					     [PcapRecord|PcapRecords]},
		    handle_event1(ClientPkt, Msg, State, NewSD).

handle_event1(ClientPkt, #msg{r = R, f = F}, State, SD) when R + F > 0 ->
    Cause = end_cause(ClientPkt, R),
    process_event(#session_end{cause = Cause}, State, SD);
handle_event1(true, #msg{s=1,a=0}, State, SD) ->
    process_event(syn, State, SD);
handle_event1(false, #msg{s=1, a =1 , ts = T, seq = Seq}, State, SD) ->
    process_event(#syn_ack{t=T, seq=Seq}, State, SD);
handle_event1(true, #msg{data_len = 0, seq = Seq, ack = Ack}, State, 
	      #session_data{cseq0 = CSeq0, sseq0 = SSeq0} = SD) 
  when Seq - CSeq0 =:= 1,  Ack - SSeq0 =:= 1 ->
    process_event(ack_syn_ack, State, SD);
handle_event1(true, #msg{data_len = 0, seq = _Seq, ack = _Ack}, State, 
	      #session_data{cseq0 = _CSeq0, sseq0 = _SSeq0} = SD) ->
%    process_event(#c_ack{ rseq = Seq - CSeq0, rack = Ack - SSeq0}, 
%		  State, SD);
    #next_state{name = State, session_data = SD};
handle_event1(_, #msg{data_len = 0, seq = _Seq, ack = _Ack}, State, 
	      #session_data{cseq0 = _CSeq0, sseq0 = _SSeq0} = SD) ->
%    process_event(#s_ack{rseq = Seq - SSeq0, rack = Ack - CSeq0}, 
%		  State, SD);
    #next_state{name = State, session_data = SD};
handle_event1(true, #msg{ts = T, data_len = L, seq = Seq, ack = Ack}, 
	      State, 
	      #session_data{cseq0 = CSeq0, sseq0 = SSeq0} = SD) ->
    process_event(#c_pkt{t = T, l=L, rseq = Seq - CSeq0, 
			 rack = Ack - SSeq0}, 
		  State, SD);
handle_event1(_, #msg{ts = T, data_len = L, seq = Seq, ack = Ack}, 
	      State, 
	      #session_data{cseq0 = CSeq0, sseq0 = SSeq0} = SD) ->
    process_event(#s_pkt{t = T, l = L, rseq = Seq - SSeq0, 
			 rack = Ack - CSeq0}, 
		  State, SD).

-spec process_event(Event :: state_event(), 
		    State :: atom() | fun((any(), any()) -> #next_state{}), 
		    SD :: #session_data{}) -> R :: #next_state{}.
process_event(Event, State, SD) ->
    Result = (catch ?MODULE:State(Event, SD)),
    process_result(Result , Event, SD).

-spec process_result(Result :: any(), 
		     Event :: state_event(), 
		     SD :: #session_data{}) -> 
			    #next_state{}.
process_result(unexpected, Event, 
	       #session_data{states = [State|_]=States} = SD) ->
    NewSD = SD#session_data{states=[State|States]},
    store_unexpected(Event, NewSD),
    #next_state{name = State, session_data = NewSD};
process_result(#next_state{name = Name, 
			   session_data = 
			       #session_data{states = States} = SD} 
	       = NextState, _Event, _oldSD) ->
    NextState#next_state{session_data 
			 = SD#session_data{states = [Name|States]}};
process_result({stop, normal, #session_data{states = States} = SD},
	      _Event, _OldSD) ->
    {stop, normal, SD#session_data{states = [idle|States]}};
process_result({'EXIT', {function_clause, _}}= Reason,
	       Event, 
	       #session_data{states = [State|_]=States} = SD) ->    
    NewSD = SD#session_data{states=[State|States]},
    store_unexpected(Event, NewSD),
    {stop, Reason, NewSD}.

    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------

terminate(normal, _StateName, _SD) ->
    ok;
terminate(Reason, StateName, SD) ->
    store_terminate(Reason, StateName, SD),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


-spec control_retrans(SD :: #session_data{}) ->
			  NewSD :: #session_data{}.
control_retrans(#session_data{retrans = R} = SD) ->
    SD#session_data{retrans = R+1}.

-spec data_retrans(SD :: #session_data{}) ->
			  NewSD :: #session_data{}.
data_retrans(#session_data{transaction = #transaction{retrans = R} 
			   = Tr} = SD) ->
    SD#session_data{transaction = Tr#transaction{retrans = R + 1}}.

tx_stats(#session_data{
	    transaction = #transaction{rx_bytes = RxB, rx_frames = RxF} 
	    = Tr} = SD, L) ->
    SD#session_data{
      transaction = Tr#transaction{rx_bytes = RxB + L, 
				   rx_frames = RxF + 1}}.
rx_stats(#session_data{
	    transaction = #transaction{tx_bytes = TxB, tx_frames = TxF} 
	    = Tr} = SD, L) ->
    SD#session_data{
      transaction = Tr#transaction{tx_bytes = TxB + L, 
				   tx_frames = TxF + 1}}.

cseq(#session_data{cseq_next = CSeqN} = SD, L) ->
    SD#session_data{cseq = CSeqN, cseq_next = CSeqN + L}.

sseq(#session_data{sseq_next = SSeqN} = SD, L) ->
    SD#session_data{sseq = SSeqN, sseq_next = SSeqN + L}.

rx(SD, L) ->
    SD1 = rx_stats(SD, L),
    cseq(SD1, L).

tx(SD, L) ->
    SD1 = tx_stats(SD, L),
    sseq(SD1, L).


end_cause(true, 1) -> c_rst;
end_cause(true, 0) -> c_fin;
end_cause(_, 1) -> s_rst;
end_cause(_, 0) -> s_fin.

-spec end_session(Cause :: end_cause(), SD :: #session_data{}, 
		  Error :: error_code()) -> 
			 {stop, normal, NewSD :: #session_data{}}. 

end_session(Cause, SD, Error) ->
    end_session(SD#session_data{end_cause = Cause}, Error).

-spec end_session(SD :: #session_data{}, Error :: error_code()) ->
			 {stop, normal, SD :: #session_data{}}.
end_session(#session_data{session_key = Key,
		   sniffer = Sniffer,
		   t_last_msg = Tl,
		   session = Session,
		   t_start = Ts,
		   latency = L,
		   end_cause = Cause,
		   retrans = SR,
		   transaction = Tr,
		   transactions = Trs,
		   pcap_records = Rs,
		   filters = Filters
		  } = SD, Error) ->
    SU = #session_update{
	    session = Session,
	    t_start = Ts,
	    latency = L,
	    t_end = Tl, 
	    end_cause = Cause,
	    error_code = Error,
	    syn_retrans = SR,
	    transactions = [Tr|Trs]},
    act(Ts, Session, Rs, match:match(SU, Filters)),
    admin:session_update(SU),
    sniffer:end_session(Sniffer,Key),
    {stop, normal, SD}. 




act(_Ts, _Session, _PcapRecords, none) ->
    ok;
act(Ts, Session, PcapRecords, Filter) ->
    monitor:filter_event(#log_notify{t = Ts, 
				     session = Session,
				     filter = Filter,
				     pcap_records = PcapRecords}).

%% @doc
-spec latency(T1 :: time(), T2 :: time()) ->
			   T :: latency().
latency(#timeStamp{seconds = S1, micro = U1} = T1,
	 #timeStamp{seconds = S2, micro = U2}      = T2) when T1 < T2 ->
    (S2-S1)*1000000 + (U2-U1).

-spec timeout( SD :: #session_data{} , Error :: error_code()) ->
	 {stop, normal, NewSD :: #session_data{}}. 
timeout(SD, Error) ->
    end_session(SD#session_data{
		  end_cause = timeout}, Error).
	

-spec store_terminate(Reason::any(), StateName :: state(), 
		      SD :: #session_data{}) ->
			     ok.
store_terminate(Reason, StateName, SD) ->
   Descriptor = io_lib:format(
		  "session:~w with Session Data =~n~n~w~n~nReason=~n~n~w ", 
			       [StateName, SD, Reason]),
    store(terminate, SD, Descriptor).


-spec store_unexpected(Event:: state_event(), SD :: #session_data{}) -> ok.
store_unexpected(Event, SD) ->
    Descriptor = io_lib:format(
"unexpected Event ~n~n~w~n~n received by session with Session Data =~n~n~w~n~n", 
		   [Event,
		    ?rec_info(session_data, SD#session_data{pcap_records = []})]),
    store(unexpected, SD, Descriptor).


-spec store(Type :: debug_session(), 
	    SD :: #session_data{}, Descriptor :: iodata()) -> ok.
store(Type, SD, Descriptor) ->
    Matched = ets:update_counter(debug_session, 
				 Type, 1),
    store(Type, SD, Descriptor, Matched).

-spec store(Type :: debug_session(), 
	    SD :: #session_data{}, Descriptor :: iodata(),
	   Matched :: pos_integer()) -> ok.
store(_Type, _SD, _Descriptor, Matched) when Matched > 128 -> ok;
store(Type, #session_data{pcap_records = Rs}, Descriptor, Matched) ->
    Name = lists:flatten(io_lib:format("debug/session/~w/~w",
				       [Type, Matched])),
    PcapFile = [?PCAP_FILE_HDR, lists:reverse(Rs)],
    action:store(Name, PcapFile, Descriptor).

-spec tr(Tr :: #transaction{}, Trs :: [#transaction{}]) -> 
		NewTrs :: [#transaction{}].
tr(#transaction{latency = undefined}, Trs) ->
    Trs;
tr(Tr, Trs) ->
    [Tr|Trs].
    

