MATHLIB=lake-packages/mathlib/Mathlib
find $MATHLIB -name '*.lean' ! -name "Mathlib.lean" | while read f; do
    { module=`echo $f | sed 's/.*Mathlib/Mathlib/g; s/.lean$//g; s/\//./g'`
    echo $f
    deps=`grep '^import Math' $f | sed 's/import //g' | xargs` 
    #NOTE(RFM): paste -s -d ' '` didin't work for me.
    echo $module:$deps
    } &
done
