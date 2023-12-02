MODULES=`realpath $1`
NAMES=`mktemp`
LEAN_EXTRACTOR=Scripts/ExtractNames.lean
export LEAN_PATH=build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/mathlib3port/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/mathlib/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/lean3port/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/std/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/Qq/build/lib
export LEAN_PATH=$LEAN_PATH:lake-packages/aesop/build/lib
lean --run $LEAN_EXTRACTOR $MODULES $NAMES
cat $NAMES | grep -v '\._' | grep -v '«'
