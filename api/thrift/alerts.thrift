/* Copyright (C) 2014 - All Rights Reserved
 * Proprietary and confidential property of the Author
 * Adoor Balasubramanian <elight.balu@gmail.com> October 2014
 */


/* This file provides the api that elight will use to send alerts */

/* all apis are one way and have void return type*/

include "types.thrift"



// First field always indicates Alert types.Level
service System {

// When a session mgr is down or unreacheable 
	oneway void session_mgr_down ( 1: types.Level severity =
					types.Alert.SEVERE,
					2: types.HostId sessionMgr ),

// When a session mgr comes up
	oneway void session_mgr_up ( 1: types.Level severity =
					types.Alert.INFORMATIONAL,
					2: types.HostId sessionMgr ),
}


// First field always indicates Alert types.Level
service Availability { 

// When a new host is activated in the data center

	oneway void new_host(
		1:types.Level serevrity = types.Level.INFORMATIONAL,
		2:types.HostId host), 

// When a new service is activated in server
	oneway void new_service(1:types.Level serevrity 
					= types.Level.INFORMATIONAL,	
				2:types.HostId server, 
				3:types.ServiceId _service),

// When service is not responding to application request
	oneway void no_response(1:types.Level serevrity,
				2:types.HostId server, 
				3:types.ServiceId _service),

// When server is not responding to connection request for service
	oneway void service_inactive(1:types.Level serevrity,
					2:types.HostId server, 
					3:types.ServiceId _service),

// Host is quiet for a long time (Configurable)
// This, 
// in the absence of the the alarms service_inactive and no_response, 
// could mean Host may be been decommsioned or unused or simply down for a long time.
	oneway void host_inactive(1:types.Level serevrity,
					2:types.HostId host)




}


// Following alerts are sent
// when a threshold value configured in the rule is exceeded
// for particular latency type such as peak_response_time_offered
service ResponseTime {

	oneway void peak_response_time_offered(1:types.HostId Client, 
					2:types.HostId server,
					3:types.ServiceId _service, 
					4:types.TimeStamp _when,
					5:string rule),

	oneway void average_response_time_offered(1:types.HostId server,
					2:types.ServiceId _service, 
					3:types.TimeStamp _when,
					4:string rule),

	oneway void peak_response_time_experienced(1:types.HostId Client, 
					2:types.HostId server,
					3:types.ServiceId _service, 
					4:types.TimeStamp _when,
					5:string rule),

	oneway void average_response_time_experienced(1:types.HostId Client,
					2:types.ServiceId _service, 
					3:types.TimeStamp _when,
					4:string rule)

}





	
