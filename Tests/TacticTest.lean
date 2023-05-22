import Mathlib
import Mathlib.Algebra.Group.Defs
import PremiseSelection.Tactic

open PremiseSelection

variable {M : Type u} [RightCancelMonoid M] {a b : M}
variable [CommSemigroup G]

example : b = a * b ↔ a = 1 := by {
  rw [eq_comm]
  suggest_premises
  sorry
}

example : ∀ a b c : G, a * (b * c) = b * (a * c) := by
{
  intros a b c
  suggest_premises
  sorry
}

example (a : Nat) : 0 + a = a := by {
  suggest_premises
  sorry
}
