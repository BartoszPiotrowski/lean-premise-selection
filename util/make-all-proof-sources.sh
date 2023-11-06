#!/usr/bin/env bash
#
MATHLIB=lake-packages/mathlib/Mathlib

SCRIPT_DIR=`dirname $0`

find $MATHLIB -name '*.lean' ! -name "Mathlib.lean" | while read f; do
    module=`echo $f | sed 's/.*Mathlib/Mathlib/g; s/.lean$//g; s/\//./g'`
    echo "Making proof sources for $module"
    $SCRIPT_DIR/make-proof-sources.sh $module
done
