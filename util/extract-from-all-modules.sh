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

find $MATHBIN -name '*.lean' ! -name "All.lean" | while read f; do
    module=`echo $f | sed 's/.*Mathbin/Mathbin/g; s/.lean$//g; s/\//./g'`
    echo "Extracting from $module"
    $SCRIPT_DIR/extract-from-module.sh $module $OUT_DIR $PARAMS
done

find $MATHLIB -name '*.lean' ! -name "All.lean" | while read f; do
    module=`echo $f | sed 's/.*Mathlib/Mathlib/g; s/.lean$//g; s/\//./g'`
    echo "Extracting from $module"
    $SCRIPT_DIR/extract-from-module.sh $module $OUT_DIR $PARAMS
done
