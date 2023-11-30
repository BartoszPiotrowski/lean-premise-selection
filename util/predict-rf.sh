#!/usr/bin/env bash

set -o pipefail
DIR=`pwd`
TEST_FEATURES=$1
TEST_LABELS=$2
FOREST=$3
LOG=${0%.*}.`basename $TEST_FEATURES`.`basename $FOREST`.$RANDOM.log
PREDS=${LOG%.*}.preds

RF_DIR=../lean-premise-selection
PREDICT_BINARY=$RF_DIR/build/bin/Predict
echo "Log being written to $LOG" &&
echo "Evaluating on $TEST_FEATURES" &&
{
    time ./$PREDICT_BINARY \
    $FOREST \
    $TEST_FEATURES \
    $PREDS
} 2>&1 | tee -a $LOG &&
echo
./util/cover.py $TEST_LABELS $PREDS --n 0 | tee -a $LOG &&
./util/cover.py $TEST_LABELS $PREDS --n 10 | tee -a $LOG
