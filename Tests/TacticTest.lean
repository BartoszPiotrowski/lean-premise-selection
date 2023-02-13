import Mathlib
import Mathlib.Algebra.Group.Defs
import PremiseSelection.Tactic
import PremiseSelection.Widget

open PremiseSelection


variable {M : Type u} [RightCancelMonoid M] {a b : M}

example : b = a * b ↔ a = 1 := by {
  --print_smt_features
  suggest_premises
  sorry
}
--eq_comm.trans mul_left_eq_self

variable [CommSemigroup G]

#check mul_comm
example : ∀ a b c : G, a * (b * c) = b * (a * c) := by
{
  --print_smt_features
  intros a b c
  suggest_premises
  sorry
}
--left_comm Mul.mul mul_comm mul_assoc

example (a b c : Nat) (h : a < 4) : 0 + a = a := by {
  --print_smt_features
  suggest_premises
  simp
}
