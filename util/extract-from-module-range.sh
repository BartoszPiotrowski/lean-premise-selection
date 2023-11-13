#!/usr/bin/env bash
#
FROM=$1
TO=$2
PARAMS="+source +n +b"
PARAMS_NAME=`echo $PARAMS | sed 's/ /./g'`
OUT_DIR=data/extracted.$PARAMS_NAME
SCRIPT_DIR=`dirname $0`

if [ ! -d $OUT_DIR ]; then
    mkdir $OUT_DIR
fi

MODULES_TO_PROCESS=()
I=0

# NOTE: parameters are fixed to `+source +n +b` here.
while read f; do
    if [ $I -ge $FROM ] && [ $I -le $TO ]; then
        module=`echo $f | sed 's/.*Mathlib/Mathlib/g; s/.lean$//g; s/\//./g'`
        MODULES_TO_PROCESS+=("$module")
        echo "Extracting from $module"

        if [ ${#MODULES_TO_PROCESS[@]} -eq 4 ]; then
            (
                $SCRIPT_DIR/make-proof-sources.sh "${MODULES_TO_PROCESS[0]}" &
                $SCRIPT_DIR/make-proof-sources.sh "${MODULES_TO_PROCESS[1]}" &
                $SCRIPT_DIR/make-proof-sources.sh "${MODULES_TO_PROCESS[2]}" &
                $SCRIPT_DIR/make-proof-sources.sh "${MODULES_TO_PROCESS[3]}" &
                wait
            )
        fi 

        if [ ${#MODULES_TO_PROCESS[@]} -eq 4 ]; then
            (
                timeout 3m $SCRIPT_DIR/extract-from-module.sh "${MODULES_TO_PROCESS[0]}" $OUT_DIR $PARAMS &
                timeout 3m $SCRIPT_DIR/extract-from-module.sh "${MODULES_TO_PROCESS[1]}" $OUT_DIR $PARAMS &
                timeout 3m $SCRIPT_DIR/extract-from-module.sh "${MODULES_TO_PROCESS[2]}" $OUT_DIR $PARAMS &
                timeout 3m $SCRIPT_DIR/extract-from-module.sh "${MODULES_TO_PROCESS[3]}" $OUT_DIR $PARAMS &
                wait
            )
            MODULES_TO_PROCESS=()
        fi 
    fi;
    I=$((I+1))
done < "$SCRIPT_DIR/all_modules.txt";

# Run the remaining modules (if any) outside the loop
if [ ${#MODULES_TO_PROCESS[@]} -gt 0 ]; then
    for module in "${MODULES_TO_PROCESS[@]}"; do
        echo "Extracting from $module"
        $SCRIPT_DIR/extract-from-module.sh "$module" $OUT_DIR $PARAMS
    done
fi
