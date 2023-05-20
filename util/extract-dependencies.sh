MATHBIN=lake-packages/mathlib3port/Mathbin
find $MATHBIN -name '*.lean' ! -name "All.lean" | while read f; do
    { module=`echo $f | sed 's/.*Mathbin/Mathbin/g; s/.lean$//g; s/\//./g'`
    deps=`grep '^import Math' $f | sed 's/import //g' | paste -s -d' '`
    echo $module:$deps
    } &
done
