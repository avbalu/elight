
typedef i32 IpAddress
typedef i32 HostId // IP address 

enum ServiceProtocol {
	ALL,
	TCP,
	UDP
}


struct ServiceId {
	1: i32 port, 
	2: ServiceProtocol proto
}

typedef i32 TimeStamp // unix time

// Alert Level
enum  Level {

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

