import Lean
import LeanRandomForest.Forest

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

elab "suggest_premises_with_scores" : tactic => do
  let g ← getMainGoal
  let m := MessageData.ofGoal g
  let m ← addMessageContext m
  let m ← m.toString
  let e := unlabeled m.splitOn
  let p := score (← trained_forest) e
  for i in p do
    IO.println i
