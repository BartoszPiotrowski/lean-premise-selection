BASE_DATA=$1
AUG_DATA=$2

NEW_DATA=$BASE_DATA.augmented-3-1

cp $BASE_DATA.test.features $NEW_DATA.test.features
cp $BASE_DATA.test.labels $NEW_DATA.test.labels

cat \
    $BASE_DATA.train.features \
    $BASE_DATA.train.features \
    $BASE_DATA.train.features \
    $AUG_DATA.train.features \
        > $NEW_DATA.train.features

cat \
    $BASE_DATA.train.labels \
    $BASE_DATA.train.labels \
    $BASE_DATA.train.labels \
    $AUG_DATA.train.labels \
        > $NEW_DATA.train.labels
