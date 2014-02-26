#!/bin/bash

host='localhost:5055'
track='{"turi" : "tr://wimp/tid/18771487","title" : "Bah (landing)","artist" : "Valby Vokalgruppe","cover" : "http://images.osl.wimpmusic.com/im/im?w=100&h=100&albumid=18771477"}'
case "$1" in

    "play")
        curl -X POST -d "$track" $host/pq/play
        ;;
    "get")
        curl $host/pq
        ;;
    "clear")
        curl $host/pq/clear
        ;;
    "add")
        curl -X POST -d "$track" $host/pq/add
        ;;
    "del")
        shift
        curl -X POST -d "{\"id\": \"$1\"}" $host/pq/del
        ;;
    *) echo "Unknown command"
        ;;
esac
