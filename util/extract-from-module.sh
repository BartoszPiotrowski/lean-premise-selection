#!/usr/bin/env bash

MODULE=$1
OUT_DIR=`realpath $2`
PARAMS=${@:3}
LABELS=$OUT_DIR/$MODULE.labels
FEATURES=$OUT_DIR/$MODULE.features
LEAN_EXTRACTOR=PremiseSelection/ExtractorRunner.lean

export LEAN_PATH=build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/mathlib3port/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/mathlib/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/lean3port/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/std/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/Qq/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/aesop/build/lib

lean --run --memory=4096 --timeout=100000000000 \
    $LEAN_EXTRACTOR \
    $LABELS \
    $FEATURES \
    <(echo $MODULE) \
    $PARAMS
