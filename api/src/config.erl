%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2014, adoor balasubramanian
%%% @doc
%%% Handler Module for config thrift service
%%% This defines elight system boundary
%%% All input parameters are validated here unless they are expected to be 
%% validated by Thrift
%%% @end
%%% Created : 12 Nov 2014 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(config).

-export([handle_function/2, ping/0, init/0]).
-compile(export_all).

-include("../../common/common.hrl").
-include ("../gen-erl/config_thrift.hrl").


-record(transform, {key    :: config_object(),
		    user2sys :: fun(),
		    sys2user :: fun()
		   }
       ).
handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
%    ?dbg([Function, Args]),
    function(Function, Args, atom_to_list(Function)).

function(_Function, {Db}, [$g, $e, $t, $_ | L]) ->
    Object = list_to_atom(L),
    {reply, configdb:get(db_type(Db), Object)};
function(_Function, {Db, Value}, [$s, $e, $t, $_ | L]) ->
    Object = list_to_atom(L),
    configdb:set(db_type(Db), Object, Value),
    ok;
function(Function, Args, _) ->
    case apply(?MODULE, Function, tuple_to_list(Args)) of
        ok -> ok;
        Reply -> 
%	    ?dbg(Reply),
	    {reply, Reply}
    end.

handle_error (_, closed) -> ok;
handle_error (Function, Reason) -> 
    ?dbg([Function, Reason]).

db_type(?api_DbType_RUNNING) ->
    running;
db_type(?api_DbType_STARTUP) ->
    startup;
db_type(?api_DbType_DEFAULT) ->
    default.

duplicates(L) ->
    duplicates(L,[]).

duplicates([], Acc) -> lists:reverse(Acc);
duplicates([H|T], Acc) ->
    duplicates(H,T,Acc,lists:member(H,T)).

duplicates(H, T, Acc, true) ->
    duplicates(T, [H|Acc]);
duplicates(_H, T, Acc, _) ->
    duplicates(T, Acc).


%	 void del_filters(1: DbType Db, 2: list<i32> id)
%		throws (1:NoEntry error),
-spec del_filters(Db :: integer(),
		  FilterIds :: [integer()]) -> ok.
del_filters(Db, FilterIds) ->
    ConfigDb = db_type(Db),
    F1 = fun(L, []) ->
		 NewL = [X || X <- L, not lists:member(X#filter.id, FilterIds)],
		 mnesia:write(ConfigDb,
			      #config{key = filters,
				      value = NewL},
			      write);
	    (_L, NoEntries) -> NoEntries
	 end,
    F2 = fun([]) -> FilterIds;
	    ([#config{value = L}]) ->
		 NoEntries = [X || X <-FilterIds,
				   not lists:keymember(X,
						       #filter.id,
						       L)],
		 F1(L, NoEntries)
	end,
		 
    F = fun() ->
		F2(mnesia:read(ConfigDb, filters))
	end,
    del_filters1(mnesia:activity(transaction, F), ConfigDb, FilterIds).


del_filters1(ok, running, FilterIds) ->
    monitor:del_filters(FilterIds);
del_filters1(ok, _, _FilterIds) ->
    ok;
del_filters1(NoEntries, _, _FilterIds) when is_list(NoEntries) -> 
    throw(#noEntry{object = list_to_binary(io_lib:format("~p",[NoEntries]))}).

% void  add_filters(1: DbType Db, 2: list<Filter> filter)
% throws (1:InvalidInput error, 2: DuplicateEntry duplicate),
-spec add_filters(Db:: integer(),
		  Filters :: [#filter{}] ) -> ok.
add_filters(Db, Filters) ->
    add_filters(db_type(Db),
		Filters, duplicates([Id || #filter{id=Id} <- Filters])).

add_filters(ConfigDb, Filters, []) -> %no duplicates in input
    F1 = fun(L, []) -> 
		 mnesia:write(ConfigDb, 
			      #config{key = filters,
				      value = lists:umerge(Filters,L)},
			     write);
	    (_L, Duplicates) -> [ Id || #filter{id = Id} <- Duplicates]
	 end,
    F2 = fun([]) -> F1([], []);
	    ([#config{value = L}]) ->
		 Duplicates = 
		     [ X || X <- Filters, 
			    lists:keymember(X#filter.id,
					    #filter.id,
					    L)],
		 F1(L, Duplicates)
	 end,
    F = fun() ->  F2(mnesia:read(ConfigDb, filters)) end,
    add_filters1(mnesia:activity(transaction, F), ConfigDb, Filters);
add_filters(_, _Filters, Duplicates) -> 
    D = list_to_binary(io_lib:format("Duplicate Filter Ids ~p in input", 
				     [Duplicates])),
    throw (#invalidInput{object = D}).
			     
							      
add_filters1(ok, running, Filters) -> 			
    monitor:add_filters(Filters);
add_filters1(ok, _, _Filters) -> 
    ok;
add_filters1(Duplicates, _, _Filters) when is_list(Duplicates) ->
     throw(#duplicateEntry{object = list_to_binary(io_lib:format("~p",[Duplicates]))}).


transform (_Key, []) -> {ok, []};
transform (Key, [Val]) ->
    F = ets:lookup_element(validate, Key, #transform.user2sys),
    F(Val).

apply_if_valid(Op, Table, Key, {ok, L}) ->
    response(Key, apply(configdb, Op, [Table, Key|L]));
apply_if_valid(_Op, _Table, _Key, {error, Error}) ->
    throw(#invalidInput{object = Error}).


response(_Key, ok) -> ok;
response(Key, {dupliacte,L}) ->
    F = ets:lookup_element(validate, Key, #transform.sys2user),
    Obj = [F(X)++", " || X <- L],
    throw(#duplicateEntry{object = Obj});
response(_Key, Reply) -> {reply, Reply}.



init() ->
    {ok, Transforms} = file:script("/home/balu/elight/lib/configdb/validate.config"),
    ets:new(transform, [named_table]),
    ets:insert(transform, Transforms).
    
ping() ->
    "pong".

   
dbs() ->
    [?api_DbType_RUNNING, ?api_DbType_STARTUP, ?api_DbType_DEFAULT].
