#!/usr/bin/env bash
#
MATHBIN=lake-packages/mathlib3port/Mathbin
MATHLIB=lake-packages/mathlib/Mathlib

PARAMS=$@
PARAMS_NAME=`echo $PARAMS | sed 's/ /./g'`
OUT_DIR=data/extracted.$PARAMS_NAME
SCRIPT_DIR=`dirname $0`
mkdir $OUT_DIR

# Download proof sources.
if [ ! -d data/proof_sources ]; then
    mkdir data/proof_sources
    cd data/proof_sources
    git clone https://github.com/ramonfmir/mathport.git
    cd mathport
    git checkout 40d0195
    cd ..
    mv mathport/Outputs/src/mathbin/Mathbin .
    cp -r Mathbin Mathlib
    rm -rf mathport
    cd ../..
fi

extract() {
    SCRIPT_DIR=$1
    OUT_DIR=$2
    PARAMS=$3
    module=$4
    module=`echo $module | sed 's/.lean$//g; s/\//./g'`
    module=`echo $module | sed 's/.*Mathbin/Mathbin/g'`
    module=`echo $module | sed 's/.*Mathlib/Mathlib/g'`
    echo "Extracting from $module"
    $SCRIPT_DIR/extract-from-module.sh $module $OUT_DIR $PARAMS
}
export -f extract

find $MATHBIN -name '*.lean' ! -name "All.lean" | \
    parallel "extract $SCRIPT_DIR $OUT_DIR $PARAMS {}"
find $MATHLIB -name '*.lean' ! -name "All.lean" | \
    parallel "extract $SCRIPT_DIR $OUT_DIR $PARAMS {}"

