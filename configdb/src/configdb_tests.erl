%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2015, adoor balasubramanian
%%% @doc
%%%
%%% @end
%%% Created : 29 Jan 2015 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(configdb_tests).

-import(configdb, [first_time_init/0, init_tables/0, 
		   mread/1, get/2, tables/0, keys/0]).

-include("../../common/common.hrl").

-include_lib("eunit/include/eunit.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
 
unit_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun test/1
    }.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
 

setup() ->
    code:add_path("../../common/ebin"),
    test:setup().

cleanup(X) ->
    test:cleanup(X).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%


test(_) ->
    [?_test(first_time_init()),
     ?_test(init_t()),
     ?_test(get_t())].


init_t() ->
    first_time_init(),
    init_tables(),
    [ [#config{} = X || X <- mread(Table)] 
      || Table <- [default,startup,running] 
    ].
    

get_t() ->
    [{Table, Key, get(Table,Key)} || Table <- tables(), 
		       Key <- keys()].

