-module(api_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->

%    config:init(),

    APIPort = configdb:get(api_tcp_port),
    ConfigPort = configdb:get(config_tcp_port),
    StatusPort = configdb:get(status_tcp_port),
    
    { ok, 
     { { one_for_one, 5, 10 },

% begin child specs
       [ 
% config child spec
	 child_spec(api_config,config,config_thrift,ConfigPort),

% status child spec
	 child_spec(api_status,status,status_thrift,StatusPort),

% api child spec 
% sample external application for testing
	 child_spec(api, api, alerts_thrift, APIPort),

	 {event_mgr, { event_mgr, start_link, [] },
	  permanent, 2000, worker, [event_mgr] }
       ]
% end child specs
     }
    }.

%% ===================================================================
%% Internal Functions
%% ===================================================================

child_spec(Name, Handler, Service, Port) ->
	 { Name, 
	   { thrift_socket_server, 
	     start,
	     [ [ { handler, Handler },
		 { service, Service },
		 { port, Port },
		 {socket_opts, [{recv_timeout, 60000*60}]},
		 { name, Name }
	       ]
	     ]
	   },
	   permanent,
	   2000,
	   worker,
	   [ thrift_socket_server ]
	 }.
    


	     
       
