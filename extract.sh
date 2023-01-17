#!/bin/bash
export LEAN_PATH=./build/lib
export LEAN_PATH=$LEAN_PATH:./lake-packages/mathlib3port/build/lib
export LEAN_PATH=$LEAN_PATH:./lake-packages/mathlib/build/lib
export LEAN_PATH=$LEAN_PATH:./lake-packages/lean3port/build/lib
export LEAN_PATH=$LEAN_PATH:./lake-packages/std/build/lib
export LEAN_PATH=$LEAN_PATH:./lake-packages/Qq/build/lib 
export LEAN_PATH=$LEAN_PATH:./lake-packages/Aesop/build/lib 
COUNT=0
for f in $(ls data/imports)
do 
    IMPORT=data/imports/$f
    LABELS=data/output/$f.labels
    FEATURES=data/output/$f.features 
    LOGS=data/logs/$f.logs
    echo $IMPORT
    lean --run PremiseSelection/ExtractorRunner.lean $LABELS $FEATURES $IMPORT max-depth=255 min-depth=0 +user +n &> $LOGS &
    COUNT=$((COUNT+1))
    if [ $COUNT -eq 12 ]; then
        wait
        COUNT=0
    fi
done
echo "Done"