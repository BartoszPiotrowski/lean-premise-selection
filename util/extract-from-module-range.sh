#!/usr/bin/env bash
#
FROM=$1
TO=$2
OUT_DIR=data/extracted.$PARAMS_NAME
SCRIPT_DIR=`dirname $0`

if [ ! -d $OUT_DIR ]; then
    mkdir $OUT_DIR
fi
I=0

# NOTE: parameters are fixed to `+source +n +b` here.
while read f; do
    if [ $I -ge $FROM ] && [ $I -le $TO ]; then
        module=`echo $f | sed 's/.*Mathlib/Mathlib/g; s/.lean$//g; s/\//./g'`
        echo "Extracting from $module"
        $SCRIPT_DIR/extract-from-module.sh $module $OUT_DIR +source +n +b
    fi;
    I=$((I+1))
done < "$SCRIPT_DIR/all_modules.txt";
