#!/bin/bash

# this will probably be useful in debugging
# - heartbeats / discovery
# - state-changes (multicasts from cube-server)

port=$1
if [[ "$port" == "" ]] ; then
	port=5055
fi
ngrep -d wlan0 port $port
