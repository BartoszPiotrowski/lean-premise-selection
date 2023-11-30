#!/usr/bin/env bash
#
SCRIPT_DIR=`dirname $0`

MODULES_TO_PROCESS=()

while read f; do
    module=`echo $f | sed 's/.*Mathlib/Mathlib/g; s/\//./g'`
    MODULES_TO_PROCESS+=("$module")

    if [ ${#MODULES_TO_PROCESS[@]} -eq 4 ]; then
        (
            $SCRIPT_DIR/make-proof-sources.sh "${MODULES_TO_PROCESS[0]}" &
            $SCRIPT_DIR/make-proof-sources.sh "${MODULES_TO_PROCESS[1]}" &
            $SCRIPT_DIR/make-proof-sources.sh "${MODULES_TO_PROCESS[2]}" &
            $SCRIPT_DIR/make-proof-sources.sh "${MODULES_TO_PROCESS[3]}" &
            wait
        )
        MODULES_TO_PROCESS=()
    fi
done < "$SCRIPT_DIR/all_modules.txt";

# Run the remaining modules (if any) outside the loop
if [ ${#MODULES_TO_PROCESS[@]} -gt 0 ]; then
    for module in "${MODULES_TO_PROCESS[@]}"; do
        $SCRIPT_DIR/make-proof-sources.sh "$module"
    done
fi
