
# listen for cube-server and client search multicasts. will show both
# requests and hearbeats (not search response, it's not a multicast).

# find the network interface on which we want to listen to (e.g. wlan0)
iface=`ip r|awk '/default via/ {print $5}'`
echo "listening on $iface"

socat -u UDP-RECVFROM:5055,reuseaddr,ip-add-membership=239.255.255.250:$iface,ip-pktinfo,fork \
    SYSTEM:"echo \`date +%FT%T\` \$SOCAT_PEERADDR \`cat\`"
