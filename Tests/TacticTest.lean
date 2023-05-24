import Mathlib
import Mathlib.Algebra.Group.Defs
import Mathlib.Data.Real.Sqrt
import PremiseSelection.Tactic
import PremiseSelection.Widget

open PremiseSelection

/- 
NOTE: Usually, `suggest_premises` won't be left in the proof script, it will be 
replaced by the suggestion the user clicks on. We leave it here so that we 
can easily inspect the suggestions.
-/

variable {M : Type u} [RightCancelMonoid M] {a b : M}
variable [CommSemigroup G]

section Simple

example : b = a * b ↔ a = 1 := by {
  suggest_premises -- suggests rw [eq_comm]
  rw [eq_comm]
  suggest_premises -- suggests apply mul_left_eq_self
  apply mul_left_eq_self
}

example : ∀ a b c : G, a * (b * c) = b * (a * c) := by
{
  intros a b c
  suggest_premises -- suggests apply mul_left_comm 
  apply mul_left_comm
}

example (a : Nat) : 0 + a = a := by {
  suggest_premises -- suggests apply zero_add
  apply zero_add 
}

end Simple

section LibrarySearch

example (a b : ℕ) : a + b = b + a := by {
  -- library_search (works)
  suggest_premises -- suggests apply add_comm
  apply add_comm
}

example (n : ℕ) (m : ℤ) : 2^(n + 1) * m = 2 * 2^n * m := by {
  -- library_search (declaration uses sorry)
  suggest_premises -- suggests rw [pow_succ]
  rw [pow_succ]
}

example (a b c : ℕ) : a * b = c ↔ c = b * a := by {
  -- library_search (declaration uses sorry) 
  suggest_premises -- suggests rw [eq_comm]
  rw [eq_comm]
  suggest_premises -- suggests rw [mul_comm]
  rw [mul_comm]
}

end LibrarySearch

section Harder 

open Real

example (x y : ℝ) (hx : 0 ≤ x) : sqrt (x / y) = sqrt x / sqrt y := by {
  -- suggest_premises
  sorry
}

end Harder 
