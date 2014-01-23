
# listen for cube-server multicasts (search requests and sometimes search
# responses)

socat -u UDP-RECVFROM:5055,reuseaddr,ip-add-membership=239.255.255.250:10.0.0.32,ip-pktinfo,fork \
    SYSTEM:"echo \`date +%FT%T\` \$SOCAT_PEERADDR \`cat\`"
