#!/usr/bin/env bash

SCRIPT_SOURCE="${BASH_SOURCE[0]}"
OLD_DIR=$(pwd)

# resolve $SCRIPT_SOURCE until the file is no longer a symlink
while [ -h "$SCRIPT_SOURCE" ]; do
    SCRIPT_DIR="$( cd -P "$( dirname "$SCRIPT_SOURCE" )" && pwd )"
    PVR_BASH_SOURCE="$(readlink "$SCRIPT_SOURCE")"
    [[ $SCRIPT_SOURCE != /* ]] && SCRIPT_SOURCE="$SCRIPT_DIR/$SCRIPT_SOURCE"
done

SCRIPT_DIR="$( cd -P "$( dirname "$SCRIPT_SOURCE" )" && pwd )"

cd $SCRIPT_DIR

git submodule update --init --recursive

echo Prima di proseguire accertarsi che la versione di AWK installata sia GAWK
echo altrimenti interrompere

$SCRIPT_DIR/install.sh $@

cd $OLD_DIR
