#!/bin/bash

export CR=`pwd`/chicken-eggs-repository
rm -r $CR || true
mkdir $CR

. scripts/switch-chicken-repo.sh $CR

make ci="chicken-install -p $CR" ciflags=
