#!/bin/bash

# this will probably be useful in debugging
# - heartbeats / discovery
# - state-changes (multicasts from cube-server)

ngrep -d wlan0 port 5055
