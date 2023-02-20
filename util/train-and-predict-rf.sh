#!/bin/env bash
set -o pipefail
DIR=`pwd`
DATA=$1
TRAIN=$DATA.train
TEST=$DATA.test
LOG=${0%.*}.`basename $DATA`.$RANDOM.log
FOREST=${LOG%.*}.forest
PREDS=${LOG%.*}.preds

# rf's hyper-parameters
N_TREES=300
PASSES=3
PART=0.3
INIT_THRESHOLD=2.0
OPTIM_LEVEL=1.0

RF_DIR=../lean-premise-selection
TRAIN_BINARY=$RF_DIR/build/bin/Train
PREDICT_BINARY=$RF_DIR/build/bin/Predict
cd $RF_DIR && lake build && cd $DIR &&
echo "Log being written to $LOG" &&
echo "Training on $TRAIN" &&
{
    time ./$TRAIN_BINARY \
    $TRAIN.features \
    $TRAIN.labels \
    $FOREST \
    $N_TREES \
    $PASSES \
    $PART \
    $INIT_THRESHOLD \
    $OPTIM_LEVEL
} 2>&1 | tee -a $LOG &&
echo
echo "Evaluating on $TEST" &&
{
    time ./$PREDICT_BINARY \
    $FOREST \
    $TEST.features \
    $PREDS
} 2>&1 | tee -a $LOG &&
echo
./util/cover.py $TEST.labels $PREDS --n 0 | tee -a $LOG &&
./util/cover.py $TEST.labels $PREDS --n 10 | tee -a $LOG
