{application,api,
             [{registered,[]},
              {description,"The world interfaces with elight using this application"},
              {vsn,"1"},
              {applications,[kernel,stdlib,thrift,configdb,controller]},
              {mod,{api_app,[]}},
              {env,[]},
              {modules,[alerts_thrift,api,api_app,api_sup,api_types,
                        availability_thrift,config,config_tests,config_thrift,
                        event_mgr,opConfig_thrift,responseTime_thrift,status,
                        status_tests,status_thrift,systemAlerts_thrift]}]}.