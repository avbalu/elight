%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2015, adoor balasubramanian
%%% @doc Filer
%%% Provides filing service for pcap files
%%% @end
%%% Created : 22 Jan 2015 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(filer).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([store/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/filer.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SERVER, ?MODULE).
-record(state,{
	  file_cabinet :: string() % file cabinet
	 }).

%%%===================================================================
%%% API
%%%===================================================================

-spec store(Files :: [#file{}]) -> ok.
store(Files) ->
    gen_server:cast({global, filer}, {store, Files}).

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
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, Application} = application:get_application(),
    Dir = code:lib_dir(Application),
    FilingCabinet = filename:join(Dir, "cabinet"),
    {ok, #state{file_cabinet = FilingCabinet}}.

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
handle_cast({store,Files},  
	    #state{file_cabinet = FC} = State) ->
    F = fun(#file{name = Name, data = Data}) ->
		Path = filename:join(FC,Name),
		ok = file:write_file(Path, Data)
	end,
    [F(File) || File <- Files],
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

files() ->
    [#file{name = "test.pcap",
	  data = [ 
%global header
<<16#a1b2c3d4:32,
			     2:16,
			     4:16,
			     0:32,
			     0:32,
			     65535:32,
			     1:32>>,
		     <<
%pcap record hdr
		      84,162,90,89,
		      0,5,73,162,
		      0,0,0,74,
		      0,0,0,74,

%MAC frame
		      132,27,94,239,126,228,
		      248,15,65,181,238,126,
		      8,0,

% IP hdr
		      69,0,0,60,144,7,64,0,64,6,102,169,
		      10,0,0,8,
		      74,125,239,134,
%TCP SYN
		      182,141,
		      0,80,
		      37,71,
		      224,168,0,0,0,0,160,2,114,16,90,
		      203,0,0,2,4,5,180,4,2,8,
                    10,9,156,112,175,0,0,0,0,1,3,3,7>>]},
		   #file{ name = "test.descriptor",
			  data = [<<"some">>, <<" stuff">>]}].



store_test() ->
    handle_cast({store, files()}, #state{file_cabinet = "/home/balu/elight/lib/filer/test"}).
