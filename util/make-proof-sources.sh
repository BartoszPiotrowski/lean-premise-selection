#!/usr/bin/env bash

MODULE=$1
LEAN_PROOF_SOURCE_MAKER=PremiseSelection/ProofSourceMaker.lean

export LEAN_PATH=build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/mathlib/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/std/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/Qq/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/aesop/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/proofwidgets/build/lib

lean --run --memory=16384 --timeout=1000000000000 \
    $LEAN_PROOF_SOURCE_MAKER \
    $MODULE
