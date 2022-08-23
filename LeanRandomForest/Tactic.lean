import Lean
import LeanRandomForest.Forest
import LeanRandomForest.StatementFeatures

open Lean Meta Elab Tactic Term

def trained_forest := loadFromFile "data/forest1"

elab "suggest_premises" : tactic => do
  let g ← getMainGoal
  let m := MessageData.ofGoal g
  let m ← addMessageContext m
  let m ← m.toString
  let e := unlabeled m.splitOn
  let p := ranking (← trained_forest) e
  for i in p do
    IO.println i

elab "print_smt_features" : tactic => do
  let t ← getMainTarget
  let features ← getStatementFeatures t
  for (⟨n1, n2⟩, count) in features.bigramCounts do
    dbg_trace (s!"{n1}/{n2}", count)

elab "suggest_premises_with_scores" : tactic => do
  let g ← getMainGoal
  let m := MessageData.ofGoal g
  let m ← addMessageContext m
  let m ← m.toString
  let e := unlabeled m.splitOn
  let p := rankingWithScores (← trained_forest) e
  for i in p do
    IO.println i
