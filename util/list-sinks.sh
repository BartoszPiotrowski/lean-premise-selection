DEPS=data/deps
SINKS=data/sinks

./util/extract-dependencies.sh > $DEPS
echo "Dependencies between modules saved to $DEPS"

A=`mktemp`
B=`mktemp`
C=`mktemp`

cut -d: -f1 $DEPS | sort -u > $A
cut -d: -f2 $DEPS | sort -u > $B
sed 's/ /\n/g' $B | sort -u > $C
comm -23 $A $C > $SINKS
cat $SINKS | sed 's/Mathlib/Mathlib/g' >> $SINKS
echo "Sink modules saved to $SINKS"
