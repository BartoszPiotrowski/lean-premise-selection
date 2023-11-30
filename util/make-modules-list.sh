#!/usr/bin/env bash
#
MODULES_LIST=lake-packages/mathlib/Mathlib.lean
SCRIPT_DIR=`dirname $0`

> $SCRIPT_DIR/all_modules.txt
cp $MODULES_LIST $SCRIPT_DIR/all_modules.txt
