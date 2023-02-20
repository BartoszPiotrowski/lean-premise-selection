PARAMS=$@
PARAMS_NAME=`echo $PARAMS | sed 's/ /./g'`
OUT_DIR=data/extracted.$PARAMS_NAME
SCRIPT_DIR=`dirname $0`
mkdir $OUT_DIR
cat data/all_modules_debug | while read m; do
#cat data/all_modules | while read m; do
    $SCRIPT_DIR/extract-from-module.sh $m $OUT_DIR $PARAMS
done
