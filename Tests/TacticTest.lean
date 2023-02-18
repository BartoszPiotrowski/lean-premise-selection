import Mathlib
import Mathlib.Algebra.Group.Defs
import PremiseSelection.Tactic
import PremiseSelection.Widget

open PremiseSelection

variable {M : Type u} [RightCancelMonoid M] {a b : M}

example : b = a * b ↔ a = 1 := by {
  rw [eq_comm]
  suggest_premises
}


variable [CommSemigroup G]

example : ∀ a b c : G, a * (b * c) = b * (a * c) := by
{
  intros a b c
  suggest_premises
  apply mul_left_comm
}

example (a b c : Nat) (h : a < 4) : 0 + a = a := by {
  suggest_premises
  apply zero_add
}
