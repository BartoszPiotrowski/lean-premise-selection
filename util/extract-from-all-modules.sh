PARAMS=$@
PARAMS_NAME=`echo $PARAMS | sed 's/ /./g'`
OUT_DIR=data/extracted.$PARAMS_NAME
SCRIPT_DIR=`dirname $0`
mkdir $OUT_DIR

# Download proof sources.
if [ ! -d data/proof_sources ]; then
    mkdir data/proof_sources
    cd data/proof_sources
    git clone https://github.com/ramonfmir/mathport.git
    mv mathport/Outputs/src/mathbin/Mathbin .
    cp -r Mathbin Mathlib
    rm -rf mathport
    cd ../..
fi

cat data/all_modules_debug | while read m; do
#cat data/all_modules | while read m; do
    $SCRIPT_DIR/extract-from-module.sh $m $OUT_DIR $PARAMS
done
