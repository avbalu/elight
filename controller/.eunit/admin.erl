%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2014, adoor balasubramanian
%%% @doc This is part of <em>Controller application<em> 
%% 
%% Manages Host DB (host_db), Dependency Graph and Performance DB(perf_db)
%% Creates schema and tables first time (installation)
%%
%% handles persistence of perf_db
%% During init, creates adn populates host_db using perf_db
%% Provides API for populating perf_db and host_db using Session Event Record aka Ser
%% 
%% Uses Mnesia for Host DB to support persistence, distribution, scalability
%% and redundancy
%%
%%    == Host DB ==
%%
%% Host DB
%% Stores Host Profile
%% supports flexible and fast query compared querying perf_db 
%% directly in host level queries (as against session related queries)
%% Th eonly reason thsi module is modelled as a gen_server behaviour is
%% to get the benefits of supervision strategies/sasl logging facilities
%%%
%%% @end
%%% Created :  6 Nov 2014 by adoor balasubramanian <balu@localhost.localdomain>

%% @TODO - build multi-node support
%%%-------------------------------------------------------------------
-module(admin).
-behaviour(gen_server).
-compile(export_all).
%% API
-export([start_link/0, session_update/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include("../../api/gen-erl/api_types.hrl").
-include("../../common/common.hrl").
-include("../../api/include/api.hrl").

-compile(export_all).
-define(SERVER, ?MODULE).

-record(state, {digraph :: digraph:graph()}).
		    

%%%===================================================================
%%% API
%%%===================================================================
%%% @ session_update
%%% called at the end of a client-server conversation session
%%% to update dbs

-spec session_update(SessionUpdate::#session_update{}) -> any().
session_update(SessionUpdate) ->
	gen_server:cast({global,admin}, SessionUpdate).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% creates Host DB table in mnesia
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
%    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(perf_db,
			[{attributes, record_info(fields, perf_db)}
%,
%			 {disc_copies, [node()]}
]),
    mnesia:create_table(host_db,
			[{attributes, record_info(fields, hostProfile)},
			 {record_name, hostProfile}]),
    G = digraph:new(),
    ets:new(digraph, [named_table, {read_concurrency, true}]),
    ets:insert_new(digraph, {digraph, G}),
    {ok, Mnesia_wait_time} = application:get_env(mnesia_wait_time),
    ok = mnesia:wait_for_tables([perf_db,host_db],Mnesia_wait_time ),
    init_db(G),
   {ok, #state{digraph = G}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(#session_update{latency = undefined}, State) -> 
   {noreply, State}; 
handle_cast(#session_update{session = #session{client = C,
						server = S} = Key, 
	t_start = T,
	latency = L,
	syn_retrans = R,
	transactions = Trs} = SU,
 #state{digraph = G} = State) ->
	P = #perf_data{t_start = T,
			connection_latency = L,
			syn_retrans = R,
			transactions = Trs},

   F = fun() -> session_update(SU, P,
			   mnesia:read(perf_db, Key, write))
       end,
    mnesia:activity(transaction,F),
	update_digraph(G,C,S),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-record(transaction_summary, {
	prt = 0 :: latency(),
	pt  = #timeStamp{seconds = 0,  micro = 0} :: time(),
	total_latency = 0 :: non_neg_integer(),
	rxb = 0 :: non_neg_integer(),
	rxf  = 0 :: non_neg_integer(),
	txb = 0 :: non_neg_integer(),
	txf = 0 :: non_neg_integer(),
	retrans = 0 :: non_neg_integer(),
	n = 0 :: non_neg_integer()}).



-spec transaction_summary(TransactionList::[#transaction{}]) -> #transaction_summary{}.

transaction_summary([]) -> #transaction_summary{};
transaction_summary(TransactionList) ->
	lists:foldl(fun(#transaction{
			latency = undefined},
	 #transaction_summary{n = N} = AccIn) -> AccIn#transaction_summary{n = N + 1};
			(#transaction{
			t_start = T, latency = RT,
			rx_bytes = RxB, rx_frames = RxF,
			tx_bytes = TxB, tx_frames = TxF,
			retrans = R},
			#transaction_summary{pt = PT,prt = PRT, 
			total_latency = RTSum,
			rxb = RxBSum, rxf = RxFSum,
			txb = TxBSum, txf = TxFSum,
			retrans = RSum,
			n = N}) ->
			{NewPRT,NewPT} = max({PRT,PT},{RT,T}),
			#transaction_summary{ prt = NewPRT, pt = NewPT,
			total_latency = RT + RTSum,
			rxb = RxB + RxBSum, rxf = RxF + RxFSum,
			txb = TxB + TxBSum, txf = TxF + TxFSum,
			retrans = R + RSum,
			n = N + 1}
			end, 
			#transaction_summary{},
		        TransactionList).

service_profile_from_session_update(#session_update{t_start = T,
			      error_code = no_syn_ack,
					    end_cause = End,
				       t_end = TE}) ->
   #serviceProfile{activeSince = T,
		   lastActive = TE,
		   count = dict:store(end_status(End,no_syn_ack),
				      1, dict:new()),
		   nConnections = 1,
		   totalSessionLength = #duration{},
		   maxSessionLength = #duration{},
		   trSt = #trafficStats{},
		   rto = #responseTimeStats{},
		   cto = #responseTimeStats{}
		  };
service_profile_from_session_update(
  #session_update{t_start = T,
		  error_code = Error,
		  latency = L,
		  end_cause = End,
		  t_end = TE,
		  transactions = TransactionList}) ->
    #transaction_summary{pt = PT,prt = PRT, 
			 total_latency = RTsum,
			 rxb = RxB, rxf = RxF,
			 txb = TxB, txf = TxF,
			 retrans = RSum,
			 n = N}
		= transaction_summary(TransactionList),
    D = duration(T,TE),
    #serviceProfile{activeSince = T,
		    lastActive = TE,
		    count = dict:store(end_status(End,Error),
				       1, dict:new()),
		    nTransactions = N,
		    nConnections = 1,
		    totalSessionLength = D,
		    maxSessionLength = D,
		    trSt = #trafficStats{syn_retrans = RSum,
					 rxBytes = RxB,
					 rxFrames = RxF,
					 txBytes = TxB,
					 txFrames = TxF
					},
		    rto = #responseTimeStats
		    {responseTimeSum = RTsum,
		     peakResponseTime =PRT,
		     peakTime = PT},
		    cto = #responseTimeStats{responseTimeSum = L,
					     peakResponseTime = L,
					     peakTime = T}
		   }.

-spec update_service_profile(SP :: #serviceProfile{},
			     SessionUpdate :: #session_update{}) ->
				    UpdatedServiceProfile :: #serviceProfile{}.
update_service_profile(
   #serviceProfile{activeSince = TS,count = Dict,
					nTransactions = N,
					nConnections = NConn,
		   totalSessionLength = D,
		   maxSessionLength = Dmax,
		   trSt = #trafficStats{
					rxBytes = RxBAcc,
					rxFrames = RxFAcc,
					txBytes = TxBAcc,
					txFrames = TxFAcc
				       },
		   rto = #responseTimeStats{responseTimeSum = RTSum,
					    peakResponseTime = PRTAcc,
					    peakTime = PTAcc},
		   cto = #responseTimeStats{responseTimeSum = CRTSum,
					    peakResponseTime = CPRTAcc,
					    peakTime = CPTAcc}},

		   #session_update{error_code = Error,
			t_start = T,
		      latency = L,
			end_cause = Cause,
		t_end = TE,
		      transactions = TransactionList
		     }) ->
    #transaction_summary{pt = PT,prt = PRT, 
			total_latency = RTsum1,
			rxb = RxB, rxf = RxF,
			txb = TxB, txf = TxF,
			retrans = RSum,
			n = N1}
 = transaction_summary(TransactionList),
    {NewPRT,NewPT} = max({PRT,PT},{PRTAcc,PTAcc}),
	 {CPRT, CPT} = max({CPRTAcc,CPTAcc},{L,T}),
    NewD = duration(T,TE),
    
    #serviceProfile
	{activeSince = TS,
	 lastActive = TE,
		   count = dict:update_counter(end_status(Cause,Error), 1, Dict),
		   nTransactions = N + N1,
		   nConnections = NConn + 1,
	 totalSessionLength = sum_duration(Dmax, NewD),
	 maxSessionLength = max(D,NewD),
	 trSt = #trafficStats{syn_retrans = RSum,
		   rxBytes = RxBAcc + RxB,
		   rxFrames = RxFAcc + RxF,
		   txBytes = TxBAcc + TxB,
		   txFrames = TxFAcc + TxF
		  },
	 rto = #responseTimeStats{
		  responseTimeSum = 
		      RTSum + RTsum1 ,
		  peakResponseTime = NewPRT,
		  peakTime = NewPT},
	 cto = #responseTimeStats{responseTimeSum = CRTSum + L,
				  peakResponseTime = CPRT,
				  peakTime = CPT}
	}.

-spec session_update(SU :: #session_update{}, Data :: #perf_data{},
	R :: [] | [#perf_db{}]) -> any().
session_update(#session_update{session = #session{port = P, server = S} = Key} = SU, Data, []) ->
    SPC = service_profile_from_session_update(SU),
    PerfDbRec = #perf_db{key = Key, service_profile = SPC, perf_data = [Data]},
    mnesia:write(perf_db,PerfDbRec, write),
    update_host(S, 
		#serviceId{port=P,proto= ?api_ServiceProtocol_TCP},
		SPC);
session_update(#session_update{session=#session{port = P, server = S} = Key}= 
	       SU, Data, [PerfDbRec]) ->
    #perf_db{key = Key,service_profile = SPC, perf_data = DataList} = PerfDbRec,
    NewSPC = update_service_profile(SPC, SU),
    NewDataList = [Data | DataList],
    mnesia:write(perf_db,
		 PerfDbRec#perf_db{service_profile = NewSPC, perf_data = NewDataList}, 
		 write),
    update_host(S, 
		#serviceId{port=P,proto= ?api_ServiceProtocol_TCP},
		SPC).
    

%% @doc init_db/1
%% traverse perf db and update host_db
init_db(G) ->
    Iterator = fun(#perf_db{key = #session{port=P,server=S, client = C},
			    service_profile = SP}, _Acc) ->
		       update_digraph(G, C,S),
		       update_host(S,#serviceId{port=P,proto= ?api_ServiceProtocol_TCP},SP)
	       end,
    F = fun() ->
		mnesia:foldl(Iterator, dict:new(), perf_db)
	end,
    mnesia:activity(transaction, F).
	       

-spec update_digraph(G :: digraph:graph(), C :: client(), S :: server()) -> ok.
update_digraph(G, C, S) ->
 	V1 = digraph:add_vertex(G,C),
 	V2 = digraph:add_vertex(G,S),
 	digraph:add_edge(G, V1, V2).

%% @doc update_host/3
%% update host_db given HostId, ServiceId and ServiceProfile for a client
%%  common for init_host_db and session_update
-spec update_host(S :: server(), 
		  ServiceId :: #serviceId{},
		  ServiceProfifle :: #serviceProfile{}) ->
			 ok.
update_host(S, ServiceId, SPC) ->			    
    update_host(S, 
		ServiceId,
		SPC,
		mnesia:read(host_db, S, write)).

update_host(S,ServiceId,
		   #serviceProfile{activeSince = Ts,
				   lastActive = Tl} = SP,
		   []) ->
    ServiceDict = dict:store(ServiceId, SP, dict:new()),
    mnesia:write(host_db, 
	       #hostProfile{key = S,
			    activeSince = Ts,
			    lastActive = Tl,
			    sps = ServiceDict}, 
	       write);
update_host(_S,ServiceId,
		   #serviceProfile{lastActive = Tl} = SP, 
		   [#hostProfile{sps = ServiceDict}] = [HP]) ->
    NewServiceDict =
	update_service_dict(ServiceDict,
			    ServiceId, 
			    SP),
    mnesia:write(host_db, HP#hostProfile{lastActive=Tl,
						 sps=NewServiceDict},
		 write).


-spec update_service_dict(Dict :: service_map(),
			  Key :: #serviceId{},
			  SP :: #serviceProfile{}) ->
				 NewDict :: service_map().
update_service_dict(Dict, Key, SP) ->
    dict:update(Key, 
		fun(OldSP) -> merge_service_profile(OldSP, SP) end,
		SP,
		Dict).


merge_service_profile(#serviceProfile{activeSince = Ts1,
				      lastActive = Tl1,
					count = Dict1,
		  nTransactions = N1,
		  nConnections = NConn1,
				      trSt = TrSt1,
				      rto = RTO1,
				      cto = CTO1},
   
		       #serviceProfile{activeSince = Ts2,
				       lastActive = Tl2,
					count = Dict2,
		  nTransactions = N2,
		  nConnections = NConn2,
				       trSt = TrSt2,
				       rto = RTO2,
				       cto = CTO2}) ->
    Dict = dict:merge(fun(_K, V1, V2) 
			 -> V1 + V2 end, Dict1, Dict2), 
    #serviceProfile{activeSince = min(Ts1,Ts2),
		    lastActive = max(Tl1, Tl2),
		count = Dict,
		  nTransactions = N1 + N2,
		  nConnections = NConn1 + NConn2,
		    trSt = merge_traffic_stats(TrSt1, TrSt2),		    
		    rto = merge_response_time_stats(RTO1, RTO2),
		    cto = merge_response_time_stats(CTO1, CTO2)}.

merge_traffic_stats(#trafficStats{
				  rxBytes = RxB1,
				  rxFrames = RxF1,
				  txBytes = TxB1,
				  txFrames = TxF1
				 },
		    #trafficStats{
				  rxBytes = RxB2,
				  rxFrames = RxF2,
				  txBytes = TxB2,
				  txFrames = TxF2
				 }) ->
    #trafficStats{
		  rxBytes = RxB1 + RxB2,
		  rxFrames = RxF1 + RxF2,
		  txBytes = TxB1 + TxB2,
		  txFrames = TxF1 + TxF2
		 }.

merge_response_time_stats(
  #responseTimeStats{responseTimeSum = RTSum1,
		     peakResponseTime = PRT1,
		     peakTime = PT1},
  #responseTimeStats{responseTimeSum = RTSum2,
		     peakResponseTime = PRT2,
		     peakTime = PT2}) ->
    {NewPRT,NewPT} = max({PRT1,PT1},{PRT2,PT2}),
    #responseTimeStats{responseTimeSum = RTSum1 + RTSum2,
		       peakResponseTime = NewPRT,
		       peakTime = NewPT}.
-spec end_status(End :: end_cause(), Error :: error_code()) -> #endStatus{}.
end_status(End, Error) ->
	#endStatus{endCause = api_end_cause(End),
	   		errorCode = api_error_code(Error)}.

api_end_cause(timeout) -> ?api_EndCause_TIMEOUT;
api_end_cause(c_fin) -> ?api_EndCause_CLIENT_INITIATED_FIN;
api_end_cause(s_fin) -> ?api_EndCause_SERVER_INITIATED_FIN;
api_end_cause(c_rst) -> ?api_EndCause_CLIENT_RST;
api_end_cause(s_rst) -> ?api_EndCause_SERVER_RST.

api_error_code(normal) -> ?api_ErrorCode_NORMAL;
api_error_code(no_syn_ack) -> ?api_ErrorCode_NO_SYN_ACK;
api_error_code(no_ack_syn_ack) -> ?api_ErrorCode_NO_ACK_SYN_ACK;
api_error_code(no_client_request) -> ?api_ErrorCode_NO_CLIENT_REQUEST;
api_error_code(no_server_response) -> ?api_ErrorCode_NO_SERVER_RESPONSE.

%-spec time_stamp(T :: time()) -> TS :: #timeStamp{}.
%time_stamp({S,U}) -> #timeStamp{seconds = S, micro = U}.

-spec duration(T1 :: time(), T2 :: time()) -> Duration :: #duration{}.
duration(#timeStamp{seconds = S1, micro = U1}, 
	 #timeStamp{seconds = S2, micro = U2}) when S1 =< S2, U1 > U2 ->
    #duration{seconds = S2-S1-1, micro = U2 - U1 + 1000000};
duration(#timeStamp{seconds = S1, micro = U1}, 
	 #timeStamp{seconds = S2, micro = U2}) when S1 =< S2 ->
    #duration{seconds = S2-S1, micro = U2-U1}.

-spec sum_duration(T1 :: #duration{}, T2 :: #duration{}) ->
			  T :: #duration{}.
sum_duration(#duration{seconds = S1, micro = U1}, 
	     #duration{seconds = S2, micro = U2}) 
  when U1 + U2 < 1000000 ->
    #duration{seconds = S1+S2, micro = U1+U2};
sum_duration(#duration{seconds = S1, micro = U1}, 
	     #duration{seconds = S2, micro = U2}) ->
    #duration{seconds = S1+S2+1, micro = U1+U2 - 1000000}.
    

