import Lean
import PremiseSelection.Forest
import PremiseSelection.StatementFeatures
import PremiseSelection.Widget

namespace PremiseSelection

open Lean Meta Elab Tactic Term

def trainedForest := loadFromFile "data/forest.n+b.user.user-filter.mathlib4"

syntax (name := suggestPremises) "suggest_premises" : tactic

def getFeatures := do
  let t ← getMainTarget
  let hyps_features ← withMainContext (do
    let ctx ← getLCtx
    let mut features : StatementFeatures := ∅
    for h in ctx do
      let p ← inferType h.type
      if p.isProp then
        let fs ← getStatementFeatures h.type
        features := features ++ fs
    return features
  )
  let target_features ← getStatementFeatures t
  let mut result : Array String := #[]
  for (n, _) in target_features.nameCounts do
    result := result.push s!"T:{n}"
  for (n, _) in hyps_features.nameCounts do
    result := result.push s!"H:{n}"
  for (n, _) in target_features.bigramCounts do
    result := result.push s!"T:{n}"
  for (n, _) in hyps_features.bigramCounts do
    result := result.push s!"H:{n}"
  for (n, _) in target_features.trigramCounts do
    result := result.push s!"T:{n}"
  for (n, _) in hyps_features.trigramCounts do
    result := result.push s!"H:{n}"
  return result.data

@[tactic suggestPremises]
def suggestPremisesTactic : Tactic := fun stx => do
  let features ← getFeatures
  let e := unlabeled features
  let p := rankingWithScores (← trainedForest) e
  let p : List Item := p.map (fun (name, score) => {name := name.toName, score})
  saveWidget stx p.toArray[:10]
  return ()

elab "print_smt_features" : tactic => do
  let t ← getMainTarget
  let hyps_features ← withMainContext (do
    let ctx ← getLCtx
    let mut features : StatementFeatures := ∅
    for h in ctx do
      let p ← inferType h.type
      if p.isProp then
        let fs ← getStatementFeatures h.type
        features := features ++ fs
    return features
  )
  let target_features ← getStatementFeatures t
  let features := hyps_features ++ target_features
  for (⟨n1, n2⟩, count) in features.bigramCounts do
    dbg_trace (s!"{n1}/{n2}", count)


end PremiseSelection
