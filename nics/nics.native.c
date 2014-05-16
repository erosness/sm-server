/*
* GNU/Linux C Program to return a list of devices and their IP Addresses
* Mark Loiseau - 2012
* http://blog.markloiseau.com/2012/02/get-network-interfaces-in-c/
*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <linux/netdevice.h>
#include <arpa/inet.h>

#define DEBUG(x) // x // <-- uncomment for debug output

#define MAXINTERFACES 20

// this c->scheme API has been carefully been designed so that no
// scheme-object is kept stored from within C between callbacks to
// scheme. this is important because the GCC might move scheme-objects
// around. don't change this API unless you know what you are doing.
C_word nic_add(C_word lst, char* name, char* addr);

C_word nics(C_word lst)
{
    int sock;
    struct ifconf ifconf;
    struct ifreq ifreq[MAXINTERFACES];
    int interfaces;
    int i;

    // Create a socket or return an error.
    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0)
      return -1;

    ifconf.ifc_buf = (char *) ifreq;
    ifconf.ifc_len = sizeof ifreq;

    if (ioctl(sock, SIOCGIFCONF, &ifconf) == -1)
      return -2;

    interfaces = ifconf.ifc_len / sizeof(ifreq[0]);

    DEBUG(printf("nics: interfaces %d \n", interfaces);)

    for (i = 0; i < interfaces; i++) {
        char ip[INET_ADDRSTRLEN];
        struct sockaddr_in *address = (struct sockaddr_in *) &ifreq[i].ifr_addr;

        // convert ip from blob to string
        if (!inet_ntop(AF_INET, &address->sin_addr, ip, sizeof(ip)))
          return -100;

        DEBUG(printf("nics:  %s\t%s\n", ifreq[i].ifr_name, ip);)
        lst = nic_add(lst, ifreq[i].ifr_name, ip);
    }

    close(sock);
    return lst;
}
