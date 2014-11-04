#!/bin/sh

# hack to make sure network and avahi is available when we start
# TODO: fix properly
sleep 10

cd data
cube-server -n maestro 5055 
