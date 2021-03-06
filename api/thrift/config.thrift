/*
TODO : Default handling
*/


include "types.thrift"

/* There are three APIs for each configurable object

get Operation 

get_ reads the config object from the specified Db
All objects support get


restore_default opetaion

restores default config.
oneway
takes no arguments
returns nothing
All objects support restore_default

add operation

All aggrgate objects support add
add_  adds a set of objects to the already configured set.
Returns Result
If ANY object being added is already in the configured set,
then NO object is added and Exception DuplicateEntry is thrown.
Exception types.InvalidOperation.Exception InvalidInput is thrown if input validation fails


del operation

All aggrgate objects support del
del_ deletes a set of objects from the already configured set.
Returns Result
If ANY object being deleted is not in the configured set,
then NO object is deleted and Exception NoEntry is thrown.

set operation
All Scalar objects support set
Returns Result
throws InvalidInput




*/


typedef i32 Duration // in seconds

exception InvalidInput {
	  1: string    error
}

exception DuplicateEntry {
	  1: string    error
}

exception NoEntry {
	  1: string    error
}




enum DbType {

	RUNNING,
	STARTUP,
	DEFAULT
}


/* Rate Object 
validation count > 0 && t > 0
*/
struct Rate {
	1: i32 count,
	2: Duration t
}

service Config {

/* for test */

	string ping(),

/* commit RUNNING_CONFIG to STARTUP_CONFIG */

	oneway void commit(),

/* A session manager is the the front end 
that receives the network traffic to be monitored.
Any element in set that is equal to or less than zero, will fail the input validation
 */

	set<types.HostId> get_session_mgrs (1: DbType whichDb),

	Result add_session_mgrs(1: set<types.HostId> sessionMgrs)
	 	throws (1:InvalidInput error, 2: DuplicateEntry duplicate),

	Result del_session_mgrs(1:set<types.HostId> SessionMgrs)
		throws (1:NoEntry error),

/*
 default value is empty set 
*/
	oneway void restore_default_session_mgrs(),

/**********************************************/

/* Server Monitoring Scope 
 Which Networks to monitor
For a session to be monitored
 the Server should be within the Network scope
Adding a network with both ipaddress and mask as 0, will Result in monitoring everything.

*/

	set<types.Network> get_monitoring_scope(1: DbType whichDb),	

	Result add_monitoring_scope (1: set<types.Network> scope)
	 	throws (1:InvalidInput error, 2: DuplicateEntry duplicate),

	Result del_monitoring_scope (1: set<types.Network> scope)
		throws (1:NoEntry error)

/* 
default - all private ip addresses will be monitored 
*/

	oneway void restore_default_monitoring_scope(),


/*************************************************************/
/*************************************************************/

/* Following is for configuring objects that affect operational db aspects
*/

/* Host inactive time out
After this period the Host will be removed from the operational Db
*/
	i32 get_host_inactive_timeout(1: DbType whichDb),


/*
 throws exception if t is negative 
*/
	Result set_host_inactive_timeout(1: Duration t)
		throws (1:InvalidInput error)

/*
default is infinity
*/
	oneway void restore_default_host_inactive_timeout(),

/*************************************************************/

/* 
performance data retention period
After this period old performance data will be removed from operational database
*/

	i32 get_perf_retention_period(1: DbType whichDb),


/*
 throws exception if t is negative 
*/
	Result set_perf_retention_period(1: Duration t)
		throws (1:InvalidInput error)

/*
default is one day
*/
	oneway void restore_default_perf_retention_period(),

/*************************************************************/

/*
If a service does not respond to MAX_R application requests in MAX_T time
it is considered non responsive */

	i32 get_service_no_response_rate(1: Rate r)


/*
 throws exception if t is negative 
*/
	Result set_service_no_response_rate(1: Duration t)
		throws (1:InvalidInput error)

/*
default: consider a serviec non responsive on first response time out
*/
	oneway void restore_default_service_no_response_rate(),



}


	


	





