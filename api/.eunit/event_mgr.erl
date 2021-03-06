%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2015, adoor balasubramanian
%%% @doc
%%%
%%% @end
%%% Created : 25 Jan 2015 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(event_mgr).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0]).

-export([notify/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-include("../../common/common.hrl").
-include("../include/event_mgr.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SERVER, {global, ?MODULE}).

-record(state, {notiftications_done = 0 :: non_neg_integer(),
		nConnections = 0 :: non_neg_integer(),
		thrift_client :: any()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec notify(Event :: event()) -> ok.
notify(Event) ->
    gen_event:notify(?SERVER, Event).

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link(?SERVER).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Port = configdb:get(api_tcp_port),
    {ok, C0} = thrift_client_util:new( "localhost", 
				       Port, 
				       alerts_thrift,[]),
    {ok, #state{thrift_client = C0}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event(#filter_event{
		session = #session{server = S, port = P, client = Client},
		filter = #filter{action = #action{alertLevel = L}} = Filter	       } , 
	     #state {thrift_client = C} = State) ->
    Args = [ L, #filterAlert {server = S,
			      serviceId = 
				  #serviceId{
				     port = P,
				     proto = ?api_ServiceProtocol_TCP},
			      client = Client,
			      filter = Filter} ],
    R = (catch thrift_client:call(C, filter_triggered, Args)),
    alert(State, R, filter_triggered, Args).

		 
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
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

alert(#state{notiftications_done = N}  = State, 
      {C, {ok,ok}}, _, _Args) ->
    {ok,
     State#state{notiftications_done = N + 1,
		thrift_client = C}};
alert(#state{notiftications_done = N,
	    nConnections = C}  = State, _,
     Method, Args) ->
    Port = configdb:get(api_tcp_port),
    {ok, C0} = thrift_client_util:new( "localhost", 
				       Port, 
				       alerts_thrift,[]),
    thrift_client:call(C, Method, Args),
    {ok,
     State#state{notiftications_done = N + 1,
		nConnections = C + 1,
		thrift_client = C0 }}.
  
