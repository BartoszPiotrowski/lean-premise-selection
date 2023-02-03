import Mathlib
import Mathlib.Algebra.Group.Defs
import PremiseSelection.Tactic

open PremiseSelection


variable {M : Type u} [RightCancelMonoid M] {a b : M}

example : b = a * b ↔ a = 1 := by {
  --print_smt_features
  suggest_premises
  sorry
}
--eq_comm.trans mul_left_eq_self

variable [CommSemigroup G]

example : ∀ a b c : G, a * (b * c) = b * (a * c) := by
{
  --print_smt_features
  suggest_premises
  sorry
}
--left_comm Mul.mul mul_comm mul_assoc

example (a b c : Nat) (h : a < 4) : a + 0 = a := by {
  --print_smt_features
  suggest_premises
  apply rfl
}
