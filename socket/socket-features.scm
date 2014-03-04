(use feature-test)

#> #include "socket.h" <#

(declaration-prefix HAVE_)
(registration-prefix "")

(define-foreign-features
  AF_UNIX

  SO_USELOOPBACK SO_REUSEPORT SO_TIMESTAMP SO_EXCLUSIVEADDRUSE

  TCP_MAXSEG TCP_NOPUSH TCP_NOOPT TCP_KEEPALIVE
  
  IP_MTU IP_MTU_DISCOVER
  IP_PKTINFO IP_RECVERR IP_RECVTOS IP_RECVTTL IP_ROUTER_ALERT 
  IP_RECVOPTS IP_RECVRETOPTS IP_RETOPTS IP_RECVDSTADDR
  
  IPV6_V6ONLY IPV6_ADDRFORM IPV6_MTU
  IPV6_MTU_DISCOVER IPV6_MULTICAST_HOPS IPV6_MULTICAST_IF IPV6_MULTICAST_LOOP IPV6_PKTINFO 
  IPV6_RTHDR IPV6_AUTHHDR IPV6_DSTOPTS IPV6_HOPOPTS IPV6_FLOWINFO IPV6_HOPLIMIT
  IPV6_RECVERR IPV6_ROUTER_ALERT IPV6_UNICAST_HOPS IPV6_NEXTHOP
  IPV6_PORT_RANGE IPV6_JOIN_GROUP IPV6_LEAVE_GROUP IPV6_CHECKSUM

  IPPROTO_IPV6
  )
