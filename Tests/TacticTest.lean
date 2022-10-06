import PremiseSelection.Tactic
open PremiseSelection

example (a b c : Nat) (h : a < 4) : a + 0 = a := by {
  print_smt_features
  suggest_premises
  apply rfl
}

example (a b c : Nat) : a + 0 = a := by {
  suggest_premises
  apply rfl
}
