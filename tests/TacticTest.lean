import LeanRandomForest.Tactic

example (a b c : Nat) (h : a < 4) : a + 0 = a := by {
  print_smt_features
  suggest_premises
  apply rfl
}

example (a b c : Nat) : a + 0 = a := by {
  suggest_premises_with_scores
  apply rfl
}

