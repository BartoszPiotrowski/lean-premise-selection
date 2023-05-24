BASE_DATA=$1
AUG_DATA=$2

NEW_DATA=$BASE_DATA.augmented-1-3

cp $BASE_DATA.test.features $NEW_DATA.test.features
cp $BASE_DATA.test.labels $NEW_DATA.test.labels

cat \
    $AUG_DATA.train.features \
    $BASE_DATA.train.features \
    $BASE_DATA.train.features \
    $BASE_DATA.train.features \
        > $NEW_DATA.train.features

cat \
    $AUG_DATA.train.labels \
    $BASE_DATA.train.labels \
    $BASE_DATA.train.labels \
    $BASE_DATA.train.labels \
        > $NEW_DATA.train.labels
