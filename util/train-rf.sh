#!/usr/bin/env bash

set -o pipefail
DIR=`pwd`
DATA=$1
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
DATA_BINARY=$RF_DIR/build/bin/Train
PREDICT_BINARY=$RF_DIR/build/bin/Predict
cd $RF_DIR && lake build && cd $DIR &&
echo "Log being written to $LOG" &&
echo "Training on $DATA" &&
{
    time ./$DATA_BINARY \
    $DATA.features \
    $DATA.labels \
    $FOREST \
    $N_TREES \
    $PASSES \
    $PART \
    $INIT_THRESHOLD \
    $OPTIM_LEVEL
} 2>&1 | tee -a $LOG
