#!/bin/bash

# Download proof sources.

if [ ! -d data/proof_sources ]; then
    mkdir data/proof_sources
    cd data/proof_sources 
    git clone https://github.com/ramonfmir/mathport.git
    mv mathport/Outputs/src/mathbin/Mathbin .
    cp -r Mathbin Mathlib
    rm -rf mathport 
    cd ../..
fi

# Split imports.

if [ ! -d data/imports ]; then
    mkdir data/imports
    cd data/imports
    split -l 40 ../all_imports import
    cd ../..
fi

# Run the extractor in parallel.

if [ ! -d data/output ]; then
    mkdir data/output
fi

if [ ! -d data/logs ]; then
    mkdir data/logs
fi

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
    lean --run --memory=4096 --timeout=100000000000 PremiseSelection/ExtractorRunner.lean $LABELS $FEATURES $IMPORT max-depth=255 min-depth=0 $@ &> $LOGS &
    COUNT=$((COUNT+1))
    if [ $COUNT -eq 12 ]; then
        wait
        COUNT=0
    fi
done

# Combine the results.

cat data/output/*.labels > data/output.labels
cat data/output/*.features > data/output.features
