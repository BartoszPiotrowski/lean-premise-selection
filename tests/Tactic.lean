import LeanRandomForest.Tactic

example (a b c : Nat) : a + 0 = a := by {
  suggest_premises
  apply rfl
}

example (a b c : Nat) : a + 0 = a := by {
  suggest_premises_with_scores
  apply rfl
}

