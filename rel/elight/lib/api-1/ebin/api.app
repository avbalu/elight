%% app generated at {2015,1,28} {8,24,16}
{application,api,
             [{description,"The world interfaces with elight using this application"},
              {vsn,"1"},
              {id,[]},
              {modules,[alerts_thrift,api,api_app,api_sup,api_types,
                        availability_thrift,config,config_thrift,event_mgr,
                        opConfig_thrift,responseTime_thrift,status,
                        status_thrift,systemAlerts_thrift]},
              {registered,[]},
              {applications,[kernel,stdlib,thrift,configdb,controller]},
              {included_applications,[]},
              {env,[{config_tcp_port,9998},
                    {status_tcp_port,9999},
                    {api_tcp_port,9997}]},
              {maxT,infinity},
              {maxP,infinity},
              {mod,{api_app,[]}}]}.

