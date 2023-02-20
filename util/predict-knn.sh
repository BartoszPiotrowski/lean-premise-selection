#!/bin/env bash
set -o pipefail
DIR=`pwd`
DATA=$1
RF_DIR=../lean-premise-selection
KNN_BINARY=$RF_DIR/build/bin/KnnPredict &&
cd $RF_DIR && lake build && cd $DIR &&
LOG=${0%.*}.`basename $DATA`.$RANDOM.log &&
TRAIN=$DATA.train &&
TEST=$DATA.test &&
PREDS=${LOG%.*}.preds &&
echo "Log being written to $LOG" &&
echo "Training on $TRAIN, evaluating $TEST" &&
{
    time ./$KNN_BINARY \
    $TRAIN.features \
    $TRAIN.labels \
    $TEST.features \
    $TEST.labels \
    $PREDS \
    100 # n. neighbours
} 2>&1 | tee -a $LOG &&
./util/cover.py $TEST.labels $PREDS --n 0 | tee -a $LOG &&
./util/cover.py $TEST.labels $PREDS --n 10 | tee -a $LOG
