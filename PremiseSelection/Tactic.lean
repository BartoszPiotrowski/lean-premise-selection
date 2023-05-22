import Lean
import PremiseSelection.Forest
import PremiseSelection.StatementFeatures

namespace PremiseSelection

open Lean Meta Elab Tactic Term

def trainedForest := loadFromFile "data/forest.source.nb.small"

syntax (name := suggestPremises) "suggest_premises" : tactic

def getGoalFeatures : TacticM (List String) := do
  let target ← getMainTarget
  let hyps ← withMainContext <| do
    let mut hyps := []
    let ctx ← getLCtx
    for h in ctx do
      let hyp ← inferType h.type
      hyps := hyps ++ [hyp]
    return hyps

  let targetFeatures ← getStatementFeatures target
  let hypsFeatures ← getArgsFeatures hyps

  let features := Array.data <| targetFeatures.toTFeatures ++
    hypsFeatures.concatMap StatementFeatures.toHFeatures
  return features

def blacklist := [
  "iff.trans",
  "iff.mp", "iff.mpr",
  "eq.trans",
  "eq.symm",
  "rfl",
  "or.elim"
]

def scoreThreshold := 1




end PremiseSelection
