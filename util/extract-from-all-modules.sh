#!/usr/bin/env bash
#
MATHLIB=lake-packages/mathlib/Mathlib

PARAMS=$@
PARAMS_NAME=`echo $PARAMS | sed 's/ /./g'`
OUT_DIR=data/extracted.$PARAMS_NAME
SCRIPT_DIR=`dirname $0`
mkdir $OUT_DIR

# Make proof sources.
if [ ! -d data/proof_sources ]; then
    $SCRIPT_DIR/make-all-proof-sources.sh
fi

find $MATHLIB -name '*.lean' ! -name "Mathlib.lean" | while read f; do
    module=`echo $f | sed 's/.*Mathlib/Mathlib/g; s/.lean$//g; s/\//./g'`
    echo "Extracting from $module"
    $SCRIPT_DIR/extract-from-module.sh $module $OUT_DIR $PARAMS
done
