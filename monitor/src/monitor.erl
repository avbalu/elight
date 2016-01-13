%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2015, adoor balasubramanian
%%% @doc Monitor
%%% House keeping functions for Monitor app
%%% Maintains common tabels for sessions
%%% Handles stats for common tables
%%% @end
%%% Created : 27 Jan 2015 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(monitor).

-behaviour(gen_server).

%% API 
-export([start_link/0]).

-export([add_filters/1, del_filters/1, filter_event/1]).

-include_lib("eunit/include/eunit.hrl").

-ifdef(EUNIT).
-compile(export_all).
-endif.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../common/common.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../../api/gen-erl/api_types.hrl").
-include("../../filer/include/filer.hrl").
-include("../../api/include/event_mgr.hrl").

-define(SERVER, ?MODULE).

-record(state, {fsmap = #{} :: #{FilterId :: integer() =>
			     Stats :: #filterStats{}}}).

%%%===================================================================
%%% API
%%%===================================================================
add_filters(Filters) ->
    gen_server:call(monitor, {add_filters, Filters}).

del_filters(Filters) ->
    gen_server:call(monitor, {del_filters, Filters}).

-spec filter_event(Msg:: #log_notify{}) -> ok.
filter_event(Msg) ->
    gen_server:cast(monitor, Msg).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    filterStats = ets:new(filterStats, [named_table, 
			   public, 
			   {keypos, #filterStats.id}]),
    debug_session = ets:new(debug_session, [named_table, 
			  public,
			  {keypos, #debug_session.key}]),
    true = ets:insert_new(debug_session, #debug_session{key = terminate}),
    true = ets:insert_new(debug_session, #debug_session{key = unexpected}),
    Filters = configdb:get(filters),
    add_filters1(Filters),
    {ok, #state{}}.
    
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
handle_call({add_filters, Filters}, _From, State) ->
    Reply = add_filters1(Filters),
    {reply, Reply, State};
    
handle_call({del_filters, Ids}, _From, State) ->
    Reply = del_filters1(Ids),
    {reply, Reply, State};
    
handle_call(filterStats, _From, State) ->
    Reply = ets:tab2list(filterStats),
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
handle_cast(#log_notify{
	       filter = #filter{id = Id, 
				action = 
				    #action{min = Min} = A}} = Msg, 
	    #state{fsmap = Map} = State) ->
    FilterStats = maps:get(Id, Map, #filterStats{id = Id}),
    #filterStats{matched = M,
		debounce_count = C} = FilterStats,
    NewC = C + 1,
    FilterStats1 = FilterStats#filterStats{matched = M + 1,
					  debounce_count = NewC},
    NewFilterStats = log_notify(NewC >= Min, A,
				Msg, FilterStats1),
    {noreply, State#state{fsmap = maps:put(Id, NewFilterStats, Map)}}.


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
add_filters1(Filters) ->
    Stats = [#filterStats{id = Id} || #filter{id = Id} <- Filters],
    true = ets:insert_new(filterStats, Stats),
    ok.

del_filters1(Ids) ->
    [true = ets:delete(filterStats, Id)
     || Id <- Ids], 
    ok.

store1(PcapRecords, #filter{id = Id} = Filter, Matched) ->
    Name = lists:flatten(io_lib:format("filter/~w_~w", [Id, Matched])),
    PcapFile = [?PCAP_FILE_HDR, lists:reverse(PcapRecords)],
    Descriptor = io_lib:format("~p", [Filter]),
    store(Name, PcapFile, Descriptor).


-spec store(Name :: string(),
	    PcapFile :: pcap_file(),
	    Descriptor :: io_lib:chars()) ->
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



log_notify(true,  #action{debounce = 0} = A, Msg, FilterStats) ->
    NewFilterStats = FilterStats#filterStats{debounce_count = 0},
	   log_notify1(A, Msg, NewFilterStats);
log_notify(true, Action, 
	   #log_notify{t = Tmsg} = Msg,
	   #filterStats{debounce_end = Tend} = FilterStats) ->
    debounce_log_notify(Tmsg =< Tend, Action, Msg, 
	       FilterStats#filterStats{debounce_end = undefined});
log_notify(_, #action{debounce = D}, 
	   #log_notify{t = #timeStamp{seconds = S, micro = M}},
	   #filterStats{debounce_count = 0} = FilterStats) ->
    {S1, M1} = t_normalize(S, M+D),
    FilterStats#filterStats{debounce_end 
			    = #timeStamp{seconds = S1, micro = M1}};
log_notify(_, _Action,  _Msg, FilterStats) ->
    FilterStats.

debounce_log_notify(true, Action, Msg, FilterStats) ->
    log_notify1(Action, Msg, FilterStats);
debounce_log_notify(_, _Action, _Msg, FilterStats) ->
    FilterStats.

log_notify1(#action{logSession = Log, notify = Notify}, Msg,
	    #filterStats{logged = Logged, notified = Notified,
			 matched = M} = FilterStats) ->
    L = log(Logged < Log, Msg, M),
    N = notify1(Notified < Notify, Msg),
    FilterStats#filterStats{logged = Logged + L,
			    notified = Notified + N,
			    debounce_count = 0}.

log(true, #log_notify{filter = Filter, pcap_records = PcapRecords}, 
    Matched) ->
    store1(PcapRecords, Filter, Matched),
    1;
log(_, _Msg, _Matched) -> 0.

notify1(true, #log_notify{session = Session, filter = Filter}) ->
    notify(Session, Filter),
    1;
notify1(_, _Msg) -> 0.

t_normalize(S, M)  ->
    {S + M div ?US_IN_S, M rem ?US_IN_S}.

    
