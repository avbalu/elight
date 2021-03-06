

typedef binary IpAddress
typedef IpAddress HostId // IP address 

enum ServiceProtocol {
	ALL = 23,
	TCP = 6,
	UDP = 17
}

/* session end reason */
enum EndCause {
	TIMEOUT,
	CLIENT_INITIATED_FIN,
	SERVER_INITIATED_FIN,
	CLIENT_RST,
	SERVER_RST
}
enum ErrorCode {
	NORMAL,
	NO_SYN_ACK,
	NO_ACK_SYN_ACK,
	NO_CLIENT_REQUEST,
	NO_SERVER_RESPONSE
}

struct ServiceId {
	1: i32 port, 
	2: ServiceProtocol proto
}

struct TimeStamp {
	1: i32 seconds = 0, // unix time
	2: i32 micro = 0
}

// Alert Level
enum  Level {

// User

USER,

// Just an FYI
INFORMATIONAL,

// Could become a problem
WARNING,

// problem
SEVERE,

// catastrophe
CRITICAL

}

struct Network {
	1: IpAddress ipAddress,
	2: byte	mask
}

struct Duration {
       1:i64 seconds = 0,
       2:i32 micro = 0
}

exception InvalidInput {
	1: binary object
}

exception DuplicateEntry {
	1: binary object
}

exception NoEntry {
	1: binary object
}

enum Result {

	SUCCESS = 0,
	FAILURE = 255
}

enum DbType {

	RUNNING,
	STARTUP,
	DEFAULT
}


/* Rate Object 
validation count > 0 && t > 0
 t in  micro seconds
*/
struct Rate {
	1: i32 count,
	2: i32 seconds
}

typedef i32 Latency // in microseconds

struct EndStatus {
	1: byte endCause,
	2: byte errorCode
}

struct TrafficStats {
	10: i64 rxBytes = 0,
	11: i64 rxFrames = 0,
	12: i64 txBytes = 0,
	13: i64 txFrames = 0,
	14: i64 syn_retrans = 0,
	15: i64 retrans = 0
}      

struct ResponseTimeStats { 
	4: Latency responseTimeSum = 0, 
	5: Latency peakResponseTime = 0,
	6: TimeStamp peakTime
}


struct ClientProfile {
       1: ResponseTimeStats rto,
       2: TrafficStats trSt
}


struct ServiceProfile {
//	1: string name = "", // e.g http. 
	2: TimeStamp activeSince, 
	3: TimeStamp lastActive, 
	4: i64 nTransactions = 0,
	5: i64 nConnections = 0,

	6: map<EndStatus, i32> count,
	7: Duration totalSessionLength, 
	8: Duration maxSessionLength, // 
/* Traffic stats 
Cumulative since activeSince time
Includes only transaction traffic
Specificall excldues TCP control traffic
*/
	9: TrafficStats trSt,

/* transaction response time offered stats */
        10: ResponseTimeStats rto,

/* TCP connecion response time stats 
t_syn_ack - t_syn
*/	11:  ResponseTimeStats cto,
   	
}	


struct HostProfile {
        1: IpAddress key,
	2: TimeStamp activeSince, 
	3: TimeStamp lastActive, 

/*
 A host may be a client to many services
for each of which it has some data
cps = client profiles
 */
//	4: map<ServiceId,ClientProfile> cps,

/*
A host may offer several services
each of which has some data
sps = Service Profiles
*/
	5: map<ServiceId,ServiceProfile> sps
}


typedef list<HostId> Group


/* Slowest in terms of response time (excludes no response cases)
file represents session capture file
GUI should highlight the 'file'  a link that can be clicked on. 
TODO: This capture file may have been truncated if it is really large 
*/
struct Slowest {  
	1: ServiceId service_id,
	2: HostId server, 
	3: HostId client,
	4: Latency response,
	5: TimeStamp when_,
	6: i32 responseSize, // in bytes
	7: string file 
}

/* nClients refers to the number of clients accessing server for _service
Only clients that are represented as hosts in elight data base are considered */
struct Clientele {
	1: HostId server,
	2: ServiceId eservice,
	3: i32	nClients 
}

struct Point {
	1:i32 x,
	2:i32 y
}


/* leastConenctedTo is calculated after ignoring internet facing services */
struct DataCenter {
	1: i32 nHosts,
	2: i32 nGroups, // connected components
	3: HostId slowServer,  // server with highest average reponse time
	4: Slowest eservice,
	5: HostId mostConnectedTo,
	6: HostId leastConnectedTo,
	7: HostId mostAccomplished, // largest transaction count
	8: HostId leastAccomplished,
	9: HostId highestConnectionRate,
	10:HostId lowestConnectionRate,
	11:HostId higestTransactionRate,
	12:HostId lowestTransactionRate
}

struct TransactionData {
       1 : TimeStamp tStart, // Transactionstart time i.e t_request
       2 : Latency latency, // t_response - t-request
       3 : i32 rxBytes,
       4 : i32 rxFrames,
       5 : i32 txBytes,
       6 : i32 txFrames,
       7 : i32 retransmissions, // request retransmissions
}



struct PerformanceData {
       1 : TimeStamp tStart, //  t_syn
       2 : Latency   ConnectionLatency, // t_syn_ack - t_syn
       3 : i32 	     retransmissions, // retransmissions during connection setup
       4 : list<TransactionData> transactions
}

enum Op { LESS, EQ, MORE }

struct Compare {
	1 : Op op,
	2 : i32 port
}

struct Range {
	1 : i32 startItem,
	2 : i32 endItem
}

union PortOrRange {
	1 : Compare port;
	2 : Range range;
}

/* Filter Action */
/* min -> minimum number of occurrences below which action 
should not be triggered even if the filer matches 
If errorCode is set to no_syn_ack, then additional validation is done
to verify that if the server has started to responding to syn.
If yes, no action is taken 

duration is in micros seconds
duration = 0 means that duration does not matter
*/

struct Action {
        1 : optional i32 debounce = 0, // micro seconds
        2 : optional i32 min = 0,
 	3 : optional i32 logSession = 0, // max log number
	4 : optional i32 notify = 0, // max notify number
	5 : optional i32 alertLevel = Level.USER
}


/* Filter Condition
/* all the latencies in Condition relate to the server 
*/
struct Condition {
	1 : optional Network client,
	2 : optional Network server,
	3 : optional ServiceProtocol proto,
	4 : optional PortOrRange portOrRange
	41: optional i32 latency,
	42: optional i32 connectionLatency,
	5 : optional i32 avgConnectionLatency,
	6 : optional i32 maxConnectionLatency,
	7 : optional i32 avglatency,
	8 : optional i32 maxLatency,
	9 : optional ErrorCode error
}

struct Filter {
	1: required i32 id, // Key
	2: required Condition condition, // match condiion
	4: required Action action,
	5: optional string comment = ""
}

/* debounce_end = startTime + action.debounce 
*/
struct FilterStats {
       1 : required i32 id, // key
       2 : required i32 matched = 0,
       3 : required TimeStamp startTime,
       4 : optional i32 debounced_matches = 0,
       5 : optional TimeStamp debounce_end,
       6 : optional i32 debounce_count = 0,
       7 : required i32 logged = 0,
       8 : required i32 notified = 0
}

struct FilterAlert {1: HostId server,
		    2: ServiceId serviceId,
		    3: HostId client,
		    4: Filter filter }

/* ------------------------Config Services ------------------------*/
service Config {

/* test 
Should return "pong"
*/
	string ping(),

/* config_object : service_down_threshold
If the server does not respond to syn for server_down_threshold times 
consecutively, then the server will be considered down.
*/

	void 	set_service_down_threshold (1: byte n)
	throws (1:InvalidInput error),

	byte 	get_service_down_threshold(1: DbType whichDb),

/* config_object : filters */
/* Filter is used to define a set of conditions that when matched against a traffic, triggers a set of actions */

	list<Filter> get_filters (1: DbType whichDb),
	
	 void  add_filters(1: DbType Db, 2: list<Filter> filter)
	 	throws (1:InvalidInput error, 2: DuplicateEntry duplicate),

	 void del_filters(1: DbType Db, 2: list<i32> id)
		throws (1:NoEntry error),

/* A session manager is the the front end 
that receives the network traffic to be monitored.
Any element in set that is equal to or less than zero, will fail the input validation
 */

	set<HostId> get_session_mgrs (1: DbType whichDb),

	Result add_session_mgrs(1: set<HostId> sessionMgrs)
	 	throws (1:InvalidInput error, 2: DuplicateEntry duplicate),

	Result del_session_mgrs(1:set<HostId> SessionMgrs)
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

	set<Network> get_monitoring_scope(1: DbType whichDb),	

	Result add_monitoring_scope (1: set<Network> scope)
	 	throws (1:InvalidInput error, 2: DuplicateEntry duplicate),

	Result del_monitoring_scope (1: set<Network> scope)
		throws (1:NoEntry error)

/* 
default - all private ip addresses will be monitored 
*/

	oneway void restore_default_monitoring_scope(),


}

/*************************************************************/
/*************************************************************/

/* Following is for configuring objects that affect operational db aspects
*/

service OpConfig {

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


/* filter */
	list <Filter> get_filters(),
	oneway void set_filter(1:Filter filter),
	oneway void delete_filter(1:i32 filterId),

}


/*--------------- Status ------------------------------------*/
service Status {

/* test */	
	string ping(),

/* Host */
	HostProfile hostProfile(1:HostId host)
		throws (1:NoEntry error1, 2:InvalidInput error2),

	list<HostProfile> hostProfiles(),

	list<HostId> hosts(),

	list<HostId> clientsUsing(1:HostId server),

	list<HostId> serversUsedBy(1:HostId client),

	list<Group> groups(),

/* Service */
	ServiceProfile serviceProfileByClient(3:HostId client, 1:HostId server, 2:ServiceId eservice) throws (1:NoEntry error1, 2:InvalidInput error2),

/* Performance Time Series Data */
	list<PerformanceData>  servicePerformance(1:HostId server, 2:ServiceId eservice),
	list<PerformanceData>  servicePerformanceByClient(1:HostId Client, 2:HostId server, 3:ServiceId eservice),


/*service SystemStatus */
	DataCenter overview()
}

/*----------------------Alerts ------------------------------*/
// First field always indicates Alert Level
service Alerts {

/* test */	
	string ping(),


// When a filter has triggered
	oneway void filter_triggered(1: i32 serverity,
				     2: FilterAlert alert
				     ),

// When a session mgr is down or unreacheable 
	oneway void session_mgr_down ( 1: Level severity =
					Level.SEVERE,
					2: HostId sessionMgr ),

// When a session mgr comes up
	oneway void session_mgr_up ( 1: Level severity =
					Level.INFORMATIONAL,
					2: HostId sessionMgr ),

// When a new host is activated in the data center

	oneway void new_host(
		1:Level serevrity = Level.INFORMATIONAL,
		2:HostId host), 

// When a new service is activated in server
	oneway void new_service(1:Level serevrity 
					= Level.INFORMATIONAL,	
				2:HostId server, 
				3:ServiceId _service),

// When service is not responding to application request
	oneway void no_response(1:Level severity,
				2:HostId server, 
				3:ServiceId _service),

// When server is not responding to connection request for service
	oneway void service_inactive(1:Level severity,
					2:HostId server, 
					3:ServiceId _service),

// Host is quiet for a long time (Configurable)
// This, 
// in the absence of the the alarms service_inactive and no_response, 
// could mean Host may be been decommsioned or unused or simply down for a long time.
	oneway void host_inactive(1:Level serevrity,
					2:HostId host)




}


// Following alerts are sent
// when a threshold value configured in the rule is exceeded
// for particular latency type such as peak_response_time_offered
service ResponseTime {

	oneway void peak_response_time_offered(1:HostId Client, 
					2:HostId server,
					3:ServiceId _service, 
					4:TimeStamp _when,
					5:string rule),

	oneway void average_response_time_offered(1:HostId server,
					2:ServiceId _service, 
					3:TimeStamp _when,
					4:string rule),

	oneway void peak_response_time_experienced(1:HostId Client, 
					2:HostId server,
					3:ServiceId _service, 
					4:TimeStamp _when,
					5:string rule),

	oneway void average_response_time_experienced(1:HostId Client,
					2:ServiceId _service, 
					3:TimeStamp _when,
					4:string rule)

}



