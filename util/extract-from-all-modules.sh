#!/usr/bin/env bash
#
PARAMS=$@
OUT_DIR=data/extracted.$PARAMS_NAME
SCRIPT_DIR=`dirname $0`

mkdir $OUT_DIR

while read f; do
    module=`echo $f | sed 's/.*Mathlib/Mathlib/g; s/.lean$//g; s/\//./g'`
    echo "Extracting from $module"
    $SCRIPT_DIR/extract-from-module.sh $module $OUT_DIR $PARAMS
done < "$SCRIPT_DIR/all_modules.txt";
