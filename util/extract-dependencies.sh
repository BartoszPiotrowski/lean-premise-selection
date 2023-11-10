#!/bin/bash
MATHLIB=lake-packages/mathlib/Mathlib
find $MATHLIB -name '*.lean' ! -name "Mathlib.lean" | while read f; do
    {
    module=`echo $f | sed 's/.*Mathlib/Mathlib/g; s/.lean$//g; s/\//./g'`
    deps=`grep '^import Math' $f | sed 's/import //g'`
    echo $module:$deps
    } &
done
