%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2015, adoor balasubramanian
%%% @doc
%%%
%%% @end
%%% Created : 28 Jan 2015 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(monitor_sup_tests).

-import(monitor_sup, [init/1]).

-include_lib("eunit/include/eunit.hrl").

init_test() ->
    init([]).

unit_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun test/1
    }.

setup() ->
    code:add_path("../../common/ebin"),
    test:setup().

cleanup(X) ->
    test:cleanup(X).

test(_) ->
    ?_test(reg()).

reg() ->
    L = erlang:registered(),
    R = [monitor_sup, monitor_sup1, monitor, creator],
    [ true = lists:member(X, L) || X <- R ].

