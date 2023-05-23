#!/usr/bin/env bash

MAIN_DATA_DIR=$1
AUX_DATA_DIR=$2
TEST_MODULES=data/sinks
TEST=$MAIN_DATA_DIR.augmented.test
TRAIN=$MAIN_DATA_DIR.augmented.train
rm $TEST.features $TEST.labels 2> /dev/null
rm $TRAIN.features $TRAIN.labels 2> /dev/null
cat $TEST_MODULES | while read m; do
    if [ -f $MAIN_DATA_DIR/$m.features ]; then
        cat $MAIN_DATA_DIR/$m.features >> $TEST.features
        cat $MAIN_DATA_DIR/$m.labels | cut -d: -f2 $f | sed 's/^ //g' >> $TEST.labels
    fi
done
ALL=`mktemp`
TRAIN_MODULES=`mktemp`
ls -1 $MAIN_DATA_DIR | grep '\.features$' | sed 's/\.features$//g' | sort > $ALL
comm -23 $ALL <(cat $TEST_MODULES | sort) > $TRAIN_MODULES
cat $TRAIN_MODULES | while read m; do
    cat $MAIN_DATA_DIR/$m.features >> $TRAIN.features
    cat $MAIN_DATA_DIR/$m.labels | cut -d: -f2 $f | sed 's/^ //g' >> $TRAIN.labels
    if [ -f $AUX_DATA_DIR/$m.features ]; then
        cat $AUX_DATA_DIR/$m.features >> $TRAIN.features
        cat $AUX_DATA_DIR/$m.labels >> $TRAIN.labels
    fi
done
