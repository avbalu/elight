%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2014, adoor balasubramanian
%%% @doc
%%% Configuration Data Base Manager
%%% Provides API for all the configuration db services
%%% There are three types of config dbs
%%% 
%%% This is a gen_server
%%% == Running db
%%% == Start up db
%%% == Default db 
%%% @end
%%% Created : 10 Nov 2014 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(configdb).

-behaviour(gen_server).

-include ("../../common/common.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).


%% API
-export([start_link/0, get/2, add/3, 
	 mread/1, mread/2, mwrite/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
get(Key) ->
    get(running, Key).

get(Table,Key) ->
    F = fun() -> 
		mnesia:read(Table, Key)
	end,
     [#config{value = Value}]
	= mnesia:activity(transaction, F),
    Value.

add(Table, Key, Val) ->
   Fun = fun() ->
		  R = #config{value=CurVal} = mnesia:read(Table,Key),
		  NewR = R#config{value=ordsets:union(CurVal,Val)},
		  add_if_new(Table,NewR, ordsets:intersection(Val,CurVal))
	  end,
    mnesia:activity(transaction, Fun).

set(Table, Key, Val) ->
    Rec = #config{key = Key, value = Val},
    F = fun() ->
		mnesia:write(Table, Rec, write)
	end,
    mnesia:activity(transaction, F).

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
%% creates default table from default.config
%% creates running table from default and startup
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    first_time_init(),
%    {ok, Mnesia_wait_time} = application:get_env(configdb, 
%						 mnesia_wait_time),
    ok = mnesia:wait_for_tables([startup], 5000 ),
    init_tables(),

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
handle_call(_, _From, State) ->
    {noreply, State}.

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

%% @doc
%% init_tables
%% load the default table from file default.config in configdb dir
%% Derive the running table from config table and default table using the 
%% following algorithm
%% running = if an object is in startup take it, if not take that object 
%% from default
%% create running table in all session mgr nodes and controller nodes
%% store validation and transformation infromation from validate.config in ets (this has been moved to config.erl)
default_config() ->   
    File = filename:join(code:lib_dir(configdb), "default.config"),
    default_config(file:script(File)).

default_config({ok, L}) ->
    L;
default_config({error,enoent}) ->
    [].

init_tables() ->			  
    L = default_config(),
    mnesia:create_table(default,
			[{attributes, record_info(fields, config)},
			 {record_name, config},
			 {type,ordered_set},
			 {ram_copies, [node()]}
			]
		       ),
    Default = lists:ukeymerge(#config.key,
			      lists:ukeysort(#config.key, L),
			      factory_defaults()),

    Startup =  mread(startup),
    NewStartup = lists:ukeymerge(#config.key, 
				 Startup, 
				 Default),

    Running = NewStartup,

    


    #config{value=SessionMgrs} = 
	lists:keyfind(session_mgrs, #config.key, Running),

    mnesia:create_table(running,
			[{attributes, record_info(fields, config)},
			 {record_name, config},
			 {type,ordered_set},
			 {ram_copies, [node()|SessionMgrs]}
			]
		       ),
    UpdateStartup
	= fun(true) -> 
		  ok;
	     (_) -> 
		  [mnesia:write(startup, X, write) || X <- NewStartup]
	  end,
    F = fun() ->
		[mnesia:write(default, X, write) || X <- Default],
		[mnesia:write(running, X, write) || X <- Running],
		UpdateStartup(Startup == NewStartup)
		
	end,

    mnesia:activity(transaction, F).
    
factory_defaults() ->
    L = [#config{ key = service_down_threshold, value = 3 },
	 #config{ key = session_mgrs, value = [] },
	 #config{ key = filters, value = [] },
	 #config{ key = config_tcp_port, value = 10001},
	 #config{ key = status_tcp_port, value = 10002},
	 #config{ key = api_tcp_port, value = 10003}],
    lists:ukeysort(#config.key, L).


%% @doc
%% first_time_init
%% create schema and startup table it if first time

first_time_init() ->
    stopped = mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(startup,
			[ {attributes, record_info(fields, config)},
			  {record_name, config},
			  {type, ordered_set},
			  {ram_copies, [node()]}
			]).


mread(Table) ->
    CatchAll = [{'_',[],['$_']}],
    SelectFun = fun() -> mnesia:select(Table, CatchAll) end,
    {atomic, L} = mnesia:transaction(SelectFun),
    L.

mread(Table, Key) ->
    {atomic, L} = mnesia:transaction(fun()->
					     mnesia:read(Table,Key)
				     end),
    L.

mwrite(Table, Key, Value) ->
	{atomic, _L} = 
		mnesia:transaction(fun()->
			mnesia:write(Table, #config{key = Key, 
					value= Value}, write)
				end),
		ok.
    

add_if_new(Table,NewR,[]) -> 
    mnesia:write(Table, NewR, write);
add_if_new(_Table,_NewR,L) ->
    {duplicate,L}.

keys() ->
    {atomic, Keys}  = mnesia:transaction(fun() -> mnesia:all_keys(running) end),
    Keys.

tables() ->
    [default, startup, running].
