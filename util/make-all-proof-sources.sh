#!/usr/bin/env bash
#
MATHLIB=lake-packages/mathlib/Mathlib

SCRIPT_DIR=`dirname $0`

MODULES_TO_PROCESS=()

find $MATHLIB -name '*.lean' ! -name "Mathlib.lean" | while read f; do
    module=`echo $f | sed 's/.*Mathlib/Mathlib/g; s/.lean$//g; s/\//./g'`
    MODULES_TO_PROCESS+=("$module")

    if [ ${#MODULES_TO_PROCESS[@]} -eq 8 ]; then
        (
            for module in "${MODULES_TO_PROCESS[@]}"; do
                $SCRIPT_DIR/make-proof-sources.sh "$module" &
            done
            wait
        )
        MODULES_TO_PROCESS=()
    fi
done

# Run the remaining modules (if any) outside the loop
if [ ${#MODULES_TO_PROCESS[@]} -gt 0 ]; then
    (
        for module in "${MODULES_TO_PROCESS[@]}"; do
            $SCRIPT_DIR/make-proof-sources.sh "$module" &
        done
        wait
    )
fi
