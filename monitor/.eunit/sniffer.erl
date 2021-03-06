%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2014, adoor balasubramanian
%%% @doc
%%%
%%% @end
%%% Created : 28 Dec 2014 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(sniffer).

-behaviour(gen_server).

-include("../../common/common.hrl").

%% API
-export([start_link/2, end_session/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Device) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%---------------------------------------------------------------------
-spec start_link(string(), string()) ->
    {ok, pid()}.

start_link(Type, Device) ->
    gen_server:start_link({local, list_to_atom(Device)}, ?MODULE, [Type, Device], []).

end_session(ServerRef, Key) ->
    gen_server:cast(ServerRef, Key).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-record(state, {name :: atom(), % registered name
		port :: port(),
		db :: atom(),
		completed_sessions = 0 :: non_neg_integer(),
		ignore_ports = [8000, 443, 5228] 
		:: [ServerPort :: server_port()],
		filters :: [#filter{}]
	       }).

init([Type, Device]) ->
    {registered_name, Name} = erlang:process_info(self(), registered_name),
%    Command = filename:join(code:priv_dir(monitor) , "monitor"),
    Command = "/home/balu/elight/lib/monitor/priv/monitor",
    PortName = {spawn_executable, Command},
    PortSettings = [{packet, 2},
		    {args, [Type, Device]},
		    nouse_stdio,
		    exit_status,
		    in,
		    binary,
		    eof],
    Port = erlang:open_port(PortName, PortSettings),
    ets:new(list_to_atom(Device), [named_table]),
    [#config{key = filters,value = Filters}]
	= configdb:mread(running, filters),
    erlang:process_flag(trap_exit, true),
    {ok, #state{name = Name,
		port = Port, 
		db = list_to_atom(Device),
		filters = Filters}}.

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
handle_cast(_Msg, State) ->
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
handle_info({Port,{data,Data}}, 
	     #state{port = Port}=State) ->
    process_frame(Data, State),
    {noreply, State};
handle_info({Port,{exit_status,0}},
	    #state{port = Port} = State) ->
%    ?dbg([]),
    {stop, normal, State};
handle_info({Port,{exit_status,Status}},
	    #state{port = Port, db = Device}=State) ->
    ?dbg([Device,Status]),
    {stop, Status, State};
handle_info({Port, eof}, 
	    #state{port = Port, db = Device}=State) ->
    ?dbg([Device]),
    Port ! {self(), close},
    {noreply, State};
handle_info({Port, closed},    
	    #state{port = Port, db = Device}=State) ->
    ?dbg([Device]),
    {stop,<<"Port Closed">>,State};

handle_info({'EXIT', Port, PosixCode},    
	    #state{port = Port, db = Device}=State) ->
    ?dbg([Device,PosixCode]),
    {stop,PosixCode,State};
handle_info({'EXIT', From, Reason},
	    #state{port = Port, db = Db, completed_sessions = N}=State) ->
    (N rem 128 =:= 0) andalso ?dbg([Db,From,Reason, Port]),
%    1 = ets:select_delete(Db, 
%			  ets:fun2ms( fun({_,Pid}) 
%			    when Pid == From -> true end)),
    {noreply, State#state{completed_sessions = N+1}};
handle_info(Info, State) ->
    ?dbg([Info, State]),
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
terminate(Reason, #state{port = Port, db = Db} = State) ->
    terminate1(Reason,State),
    erlang:port_close(Port),
    ets:delete(Db),
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

%%--------------------------------------------------------------------
%% @doc process_frame
%% @spec process_frame(Data) -> ok
%% processes ethernet frame received from device
%% @end
%%--------------------------------------------------------------------

-spec process_frame(binary(), #state{}) ->
    ok.
process_frame(<<PcapRecHdr:24/binary, MACFrame/binary>>, 
	      State) ->
    process_frame(PcapRecHdr, MACFrame, State);
process_frame(PcapRec, _State) ->
    ?dbg(PcapRec).

process_frame( <<Seconds:32/native,
		 0:32,
		 Micro:32/native,
		 0:32,
		 CapLen:32/native,
		 Len:32/native>>,

	       <<_DA:6/binary,
		 _SA:6/binary,
		 8,0, % IP
		 4:4, % IPv4
		 IPHdrLen:4,
		 _TOS,
		 TotalLen:16,
		 _Id:16,
		 _FlagsOffset:16,
		 _TTL,
		 6, % TCP
		 _HdrChkSum:16,
		 SIP:4/binary,
		 DIP:4/binary,
		 RestFrame/binary>> = Frame, 
	       #state{db = Db,
		     ignore_ports = Ignore} = State) 
  when IPHdrLen >= 5,
       4*IPHdrLen + ?TCP_MIN_HDR_LEN =< size(Frame) ->
    OptsLen = 4 * (IPHdrLen - ?IP_MIN_HDR_LEN),
    << _Opts:OptsLen/binary, PDU/binary>> = RestFrame,
    <<SP:16, DP:16, 
       Seq:32, Ack:32, 
       HdrLen:4, _Reserved:6,
       _URG:1, ACK:1, _PSH:1, 
       RST:1, SYN:1, FIN:1,
       _Window:16,
       _ChkSum:16, _UrgP:16,
       _RestPDU/binary >> 
	= PDU,
    DataLen = TotalLen - 4 * (IPHdrLen + HdrLen),
    Key = list_to_tuple(lists:sort([SIP,DIP,SP,DP])),
    PcapRec = <<Seconds:32, Micro:32, CapLen:32, Len:32, 
		Frame/binary>>,
 	    Msg = #msg{ts = #timeStamp{seconds = Seconds, 
			     micro = Micro},
		       len = Len,
		       sip = SIP,
		       dip = DIP,
		       sp = SP,
		       dp = DP,
		       seq = Seq,
		       ack = Ack,
		       a = ACK,
		       r = RST,
		       s = SYN,
		       f = FIN,
		       data_len = DataLen,
		       pcap_record = PcapRec},
    lists:member(DP, Ignore) orelse
	SIP =:= DIP orelse
	process_session(Msg,State,Key, ets:lookup(Db,Key)),
    ok;
process_frame(_Hdr_, _Frame, _Db) ->
%    io:format("~p~n", [Frame]).
 ok.

process_session(#msg{
		   ack = 0,
		   s = 1,
		   r = 0,
		   f = 0,
		   a = 0} = Msg,
		#state{name = Name, db = Db, filters = Filters},
		Key, []) ->
    InitMsg = #init_msg{sniffer = Name,
			session_key = Key,
			filters = Filters,
			msg = Msg},
    {ok, Session} = session:start_link(InitMsg),
    true = ets:insert_new(Db, {Key,Session});

process_session(Msg, _State, Key, [{Key,Pid}]) ->
    session:send(Pid, Msg);
process_session(_Msg, _State, _Key, _) ->
    ok.


% terminate1(normal,_State) -> ok;
terminate1(Reason, State) -> ?dbg([Reason,State]).


