#!/usr/bin/env bash

PARAMS=$@
PARAMS_NAME=`echo $PARAMS | sed 's/ /./g'`
OUT_DIR=data/extracted.$PARAMS_NAME
SCRIPT_DIR=`dirname $0`
MATHLIB=lake-packages/mathlib/Mathlib

mkdir $OUT_DIR

find $MATHLIB -name '*.lean' ! -name "All.lean" | while read f; do
    echo $f
    module=`echo $f | sed 's/.*Mathlib/Mathlib/g; s/.lean$//g; s/\//./g'`
    echo "Extracting from $module"
    $SCRIPT_DIR/extract-from-module.sh $module $OUT_DIR $PARAMS
done
