ALL_MODULES=$1
MATH_MODULES=data/mathlib.math-modules
cat $ALL_MODULES \
    | grep -v Mathlib.Deprecated \
    | grep -v Mathlib.Meta \
    | grep -v Mathlib.Tactic \
    | grep -v Mathlib.Testing \
> $MATH_MODULES
wc -l $ALL_MODULES
wc -l $MATH_MODULES
