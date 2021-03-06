/*
%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2014, adoor balasubramanian
%%% @doc
%%% Monitors one ethernet interface
%%% Keep this code minimal just  to handle the I/O
%%% @end
%%% Created : 27 Dec 2014 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------*/

#include <stdio.h>
#include <pcap.h>
#include <errno.h>
#include <stdlib.h>
typedef   union {
    short i;
    u_char buf[2];
} U;

void write_fill(const u_char *buf, short size) {
  int writtenSoFar = 0;

  while (writtenSoFar < size) {
    int written =  write(4, buf+writtenSoFar, size-writtenSoFar) ;
    if (written == -1 && errno != EINTR) {
      fprintf(stderr, "monitor:write_fill : write failed with errno %d\n",
	      errno);
      exit(EXIT_FAILURE);
    }
    writtenSoFar += written;
  }
}
void callback(u_char *user, const struct pcap_pkthdr *h,
		  const u_char *bytes) {
  U u;
  u.i = htons(sizeof(struct pcap_pkthdr)+ h->caplen);
  write_fill(u.buf, 2);
  write_fill((u_char *)h,sizeof(struct pcap_pkthdr));
  write_fill(bytes, h->caplen);
}

    
int main(int argc, char *argv[])
{
  pcap_t *p;		/* Session handle */
  char *dev, *type;		/* Device to sniff on */
  char errbuf[PCAP_ERRBUF_SIZE];	/* Error string */
  int result;
  printf("%d\n", sizeof(long int));
  printf("%d\n", sizeof(struct pcap_pkthdr));
  if (argc != 3) {
    fprintf(stderr, "Usage : monitor <type> <device>\n");
    return 2;
  }
  
  type = argv[1];
  dev = argv[2];
  p = strcmp(type,"interface") == 0 ? 
    pcap_open_live(dev, BUFSIZ, 1, 1000, errbuf):  
    pcap_open_offline(dev, errbuf);

  if (p == NULL) {
    fprintf(stderr, "Couldn't open device %s: %s\n", dev, errbuf);
    pcap_perror(p, "official info: ");
    return(2);
  }
  result = pcap_loop(p, -1, callback, NULL);
  if (result  == -1){
    fprintf(stderr, "monitor:main : pcap_loop failed with errno %d\n",
	    errno);
    pcap_perror(p, "official info: ");
    return 2;
  }
  fprintf(stderr, "monitor:main : pcap_loop returned ! \n");
  sleep(6);
  return(result);
}
