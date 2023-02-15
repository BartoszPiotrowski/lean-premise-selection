export LEAN_PATH=build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/mathlib3port/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/mathlib/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/lean3port/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/std/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/Qq/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/aesop/build/lib

PARAMS="max-depth=255 min-depth=0 $@"
MODULES=`mktemp`
echo Mathbin.Algebra.Group.Basic >  $MODULES
echo Mathlib.Algebra.Group.Basic >> $MODULES

lean --run PremiseSelection/ExtractorRunner.lean \
    data/test.labels \
    data/test.features \
    $MODULES \
    $PARAMS
