/* Copyright (C) 2014 - All Rights Reserved
 * Proprietary and confidential
 * Written by Adoor Balasubramanian <elight.balu@gmail.com> October 2014
 */

/* This file defines the interface definition 
for accessing information provided by elight */

/* Terminology 

Host - Client or Server

nClients - number of clients accessing server for serviceId
Only clients that are represented as hosts in elight data base are considered 

*/

/* Caveats 
sevice  and when are keywords in thrift.
So we use serviceId and _when instead when we really want to use the word service
*/

/* Query Conventions
Default values are ONLY used in query and means "all objects" 
Default values MUST NOT be used in containers (map, list, set)
A field that does not have default value MUST be filled in query
The filed with name key MUST be filled in query

Partially filled objects can be used in query
For e.g. getserviceId (X)
where X = 
*/

/* Data Types */
include "types.thrift"



/* Latency is the mesure of response time. 
In case of retransmissions, response time is measured as T2 - T1 where
T1 = Instant when most recent reqest was sent before T2
T2 = Instant when most recent response was received
*/

typedef i32 Latency // in microseconds


struct ClientProfile {
	2: types.TimeStamp activeSince = 0, 
	3: types.TimeStamp lastActive = 0, 
	5: Latency averageResponseExperienced = 0, 
	6: Latency peakResponseExperienced = 0
	7: i32 nServers,
	8: i32 nTransactions = 0,
	9: i32 nConnections = 0
}

struct ServiceProfile {
	1: string name = "", // e.g http. 
	2: types.TimeStamp activeSince = 0, 
	3: types.TimeStamp lastActive = 0, 
	4: Latency averageResponseOffered = 0, 
	5: Latency peakResponseOffered = 0,
	7: i32 nClients = 0,
	8: i32 nTransactions = 0,
	9: i32 nConnections = 0
}	


struct Host {
	1: types.HostId key, 
	2: map<types.ServiceId,ServiceProfile> serviceId
}


struct Service {
	1: types.ServiceId key,
	2: set<Host> hosts 
}

typedef list<i32> Group


/* Slowest in terms ofresponse time (excludes no response cases)
file represents session capture file
GUI should highlight the 'file'  a link that can be clicked on. 
TODO: This capture file may have been truncated if it is really large 
*/
struct Slowest {  
	1: types.ServiceId serviceId,
	2: types.HostId server, 
	3: types.HostId client,
	4: Latency response,
	5: types.TimeStamp instant,
	6: i32 responseSize, // in bytes
	7: string file 
}

/* nClients refers to the number of clients accessing server for serviceId
Only clients that are represented as hosts in elight data base are considered */
struct Clientele {
	1: types.HostId server,
	2: types.ServiceId serviceId,
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
	3: types.HostId slowServer,  // server with highest average reponse time
	4: Slowest serviceId,
	5: types.HostId mostConnectedTo,
	6: types.HostId leastConnectedTo,
	7: types.HostId mostAccomplished, // largest transaction count
	8: types.HostId leastAccomplished,
	9: types.HostId highestConnectionRate,
	10:types.HostId lowestConnectionRate,
	11:types.HostId higestTransactionRate,
	12:types.HostId lowestTransactionRate
}

service status {

/*-------------- test ----------------------------*/

	string ping(),

/*------------------Client Status -------------------------*/
	i32  client_count(1:ClientProfile profile), 
	map<types.HostId, ClientProfile>   clients(1:ClientProfile profile)
	list<Point>  client_perf(1:types.HostId client, 2:types.ServiceId serviceId)

/*----------------------Server Status ----------------*/

	i32  server_count(1:ServiceProfile profile),
	map<types.ServiceId, ServiceProfile>  servers(1:ServiceProfile profile),
	list<Point>  server_perf(1:types.HostId server, 2:types.ServiceId serviceId),
	list<Point>  perfByClient(1:types.HostId Client, 2:types.HostId server, 3:types.ServiceId serviceId)

/*-------------------------System Status ---------------------*/

	DataCenter overview()
}
	


	
