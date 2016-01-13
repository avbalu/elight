{application,configdb,
             [{registered,[]},
              {description,"manages configuration dbs"},
              {vsn,"1"},
              {applications,[kernel,stdlib]},
              {env,[{mnesia_wait_time,5000}]},
              {mod,{configdb_app,[]}},
              {modules,[configdb,configdb_app,configdb_sup,configdb_tests]}]}.
