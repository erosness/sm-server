#!/bin/sh
#
# use this script to create a sandbox for chicken



if [ $# -ne 1 ]; then
    echo "usage: '. $0 <abs path to repository>'"
    return
fi

eggsdir=$1
if [ ! -d $eggsdir ]; then
    echo "$1 is not a directory, please create it"
    return
fi

binversion=`csi -p '(##sys#fudge 42)'`
mkdir -p $eggsdir/lib/chicken/$binversion

if [ ! -e "$eggsdir/lib/chicken/$binversion/srfi-13.import.so" ]; then
    chicken-install -i $eggsdir/lib/chicken/$binversion
fi

export CHICKEN_C_INCLUDE_PATH=$eggsdir/include/chicken
export CHICKEN_INCLUDE_PATH=$eggsdir/share/chicken
export CHICKEN_REPOSITORY=$eggsdir/lib/chicken/$binversion
export CHICKEN_INSTALL_PREFIX=$eggsdir
alias chicken-install='chicken-install -p $eggsdir'
