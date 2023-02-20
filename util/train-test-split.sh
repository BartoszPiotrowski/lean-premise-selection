DATA_DIR=$1
TEST_MODULES=data/test_modules
TEST=$DATA_DIR.test
TRAIN=$DATA_DIR.train
rm $TEST.features $TEST.labels 2> /dev/null
rm $TRAIN.features $TRAIN.labels 2> /dev/null
cat $TEST_MODULES | while read m; do
    if [ -f $DATA_DIR/$m.features ]; then
        cat $DATA_DIR/$m.features >> $TEST.features
        cat $DATA_DIR/$m.labels | cut -d: -f2 $f | sed 's/^ //g' >> $TEST.labels
    fi
done
ALL=`mktemp`
TRAIN_MODULES=`mktemp`
ls -1 $DATA_DIR | grep '\.features$' | sed 's/\.features$//g' | sort > $ALL
comm -23 $ALL <(cat $TEST_MODULES | sort) > $TRAIN_MODULES
cat $TRAIN_MODULES | while read m; do
    cat $DATA_DIR/$m.features >> $TRAIN.features
    cat $DATA_DIR/$m.labels | cut -d: -f2 $f | sed 's/^ //g' >> $TRAIN.labels
done
