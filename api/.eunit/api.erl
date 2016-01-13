%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2014, adoor balasubramanian
%%% @doc
%%% modelled after "[https://git-wip-us.apache.org/repos/asf/thrift/?p=thrift.git;a=blob;f=tutorial/erl/server.erl;h=4915606f18ee724262bcc57cf986979dc47366a4;hb=HEAD]"
%%% @end
%%% Created : 10 Nov 2014 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(api).

-export([handle_function/2, handle_error/2]).

-include ("../../common/common.hrl").

handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
    case alert(Function, tuple_to_list(Args)) of
        ok -> ok;
        Reply -> {reply, Reply}
    end.

handle_error (_, closed) -> ok;
handle_error (Function, Reason) ->
    ?dbg([Function, Reason]).


alert(ping, []) ->
    "pong";
alert(filter_triggered, [L, FilterAlert]) ->
    error_logger:info_report(
      [{"Elight Alert!", filter_triggered},
       {severity, L},
       {info, ?rec_info(filterAlert, FilterAlert)}]).


%% tests %%
-ifdef(TEST).  

-include_lib("eunit/include/eunit.hrl").

setup() ->
    code:add_path("../../common/ebin"),
    test:setup(),
    Port = configdb:get(status_tcp_port),
    {ok, C0} = thrift_client_util:new( "localhost", 
				       Port, 
				       status_thrift,[]),
    C0.

cleanup(X) ->
    test:cleanup(X).

ping(C0) ->
 		      {C1, {ok, <<"pong">>} } 
	= thrift_client:call (C0, ping, []),
		      {_C2, {ok, <<"pong">>} } 
	= thrift_client:call (C1, ping, []).
-endif.

-ifdef(EUNIT).   
unit_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun test/1
    }.


test(C0) ->
    ?_test(ping(C0)).
-endif.
