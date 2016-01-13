-module(status).

-export([handle_function/2, ping/0, handle_error/2]).

-compile(export_all).

-include ("../gen-erl/status_thrift.hrl").
-include ("../gen-erl/api_types.hrl").
-include ("../../common/common.hrl").
-include("../../api/include/api.hrl").

handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
%    ?dbg([Function, Args]),
    case apply(?MODULE, Function, tuple_to_list(Args)) of
        ok -> ok;
        Reply -> 
%	    ?dbg(Reply),
	    {reply, Reply}
    end.

handle_error (_, closed) -> ok;
handle_error (Function, Reason) ->
    ?dbg([Function, Reason]).

ping() ->
    "pong".


-spec hostProfile(Host::binary()) -> HP :: #hostProfile{} | #invalidInput{}.
hostProfile(Host) when size(Host) =:= 4 ->
    hostProfile1( read(host_db, Host));
hostProfile(_Host) -> 
    throw(#invalidInput{ object = <<"HostId should be 4 bytes long">> }).

hostProfile1([HP]) ->
    HP;
hostProfile1([]) ->
    throw(#noEntry{object = <<"No Such Host!">>}).

%% @doc Servers
%% 	ServiceProfile serviceProfileByClient(3:HostId client, 1:HostId server, 2:ServiceId eservice) throws (1:NoEntry error1, 2:InvalidInput error2),

-spec serviceProfileByClient(Client :: binary(), 
		     Server :: binary(),
		     Service :: #serviceId{}) ->
			    ServiceProfile :: #serviceProfile{}.
serviceProfileByClient(C, S, #serviceId{port=P, proto = Proto}) ->
    serviceProfileByClient(C,S,P,check_proto(Proto)).

serviceProfileByClient(C,S,P, true) 
  when size(C) =:= 4, 
       size(S) =:= 4,
       P > 0 -> 
    serviceProfileByClient(read(perf_db,
			  #session{ server = S,
				    port = P,
				    client = C}));
serviceProfileByClient(_C,_S,_P,_) -> 
    throw(#invalidInput{object = <<"One or more inputs invalid">>}).

serviceProfileByClient([]) ->
    throw(#noEntry{object = <<"No matching Entry found">>});
serviceProfileByClient([#perf_db{service_profile = SPC}]) ->
    SPC.

check_proto(?api_ServiceProtocol_TCP) ->
    true;
check_proto(_Proto) ->
    false.

read(Table, Key) ->
%    ?dbg([Table,Key]),
    mnesia:activity(transaction, fun()->
					 mnesia:read(Table, Key)
				 end).
match(Pattern) ->
	mnesia:activity(transaction, 
	fun() -> mnesia:match_object(Pattern) end).

match(Table, Pattern) ->
	mnesia:activity(transaction, 
	fun() -> mnesia:match_object(Table, Pattern, read) end).



% doc ServerPerformance Data
%list<PerformanceData>  servicePerformanceByClient(1:HostId Client, 2:HostId server, 3:ServiceId eservice),
-spec servicePerformanceByClient(C :: client(),
				 S :: server(),
				ServiceId :: #serviceId{}) ->
	P :: [#performanceData{}] | [].

servicePerformanceByClient(C, S, #serviceId{port = P}) ->
	Res = read(perf_db, #session{server = S,
 client = C, port = P}),
servicePerformanceByClient(Res).

servicePerformanceByClient([]) -> [];
servicePerformanceByClient([#perf_db{perf_data = L}]) ->
	F1 = fun
	(#transaction{  
	  t_start = T, 
	  latency  = Latency,
	  rx_bytes = RxB,
	  rx_frames = RxF,
	  tx_bytes = TxB,
	  tx_frames = TxF,
	  retrans = R % req retransmissions
	 }) -> #transactionData{tStart = T,
                          latency = Latency,
                          rxBytes = RxB,
                          rxFrames = RxF,
                          txBytes = TxB,
                          txFrames = TxF,
                          retransmissions = R } end,
	F2 = fun(#perf_data{t_start = T, connection_latency = Latency, syn_retrans = SR, transactions = Tr}) ->
		#performanceData{ tStart = T, connectionLatency = Latency, retransmissions = SR, transactions = [F1(Y) || Y <- Tr]} end,
	[ F2(X) || X <- L].
	
%% @doc	list<PerformanceData>  serverPerformance(1:HostId server, 2:ServiceId eservice)
%% go through all the records in perf_db corresponding to S and aggregate them to to a single record


-spec servicePerformance(S :: server(), ServiceId :: #serviceId{}) ->
	P :: [#performanceData{}].
servicePerformance(S, #serviceId{port = P}) ->
	Pattern = #perf_db{_ = '_', key = #session{server = S, port = P, _ = '_'}},
	Res = match(Pattern),
	lists:flatten([servicePerformanceByClient([X]) || X <- Res]).

%% 	lists<HostProfile> hostProfiles(),

-spec hostProfiles() -> [#hostProfile{}].
hostProfiles() ->
Pattern = #hostProfile{_ = '_'},
match(host_db, Pattern).

% 	list<HostId> hosts(),
-spec hosts() -> [host()].
hosts() ->
    G = digraph(),
    digraph:vertices(G).

%	list<HostId> clientsUsing(1:HostId server),
-spec clientsUsing(S::server()) -> [C::client()].
clientsUsing(S) ->
    G = digraph(),
    digraph:in_neighbours(G, S).

%	list<HostId> serversUsedBy(1:HostId client),
-spec serversUsedBy(C :: client()) -> [S :: server()].
serversUsedBy(C) ->
    G = digraph(),
    digraph:out_neighbours(G, C).

% 	list<Group> groups(),
-spec groups() -> Groups :: [[host()]].
groups() ->
    G = digraph(),
    digraph_utils:components(G).

digraph() ->
    [{digraph, G}] = ets:lookup(digraph, digraph),
    G.

