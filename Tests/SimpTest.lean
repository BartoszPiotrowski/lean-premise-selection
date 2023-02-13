import Lean 
import Lean.Elab.Term
import Lean.Elab.Tactic.Basic
import PremiseSelection.Tactic

import Mathlib.Algebra.Group.Basic
-- import Mathlib.Algebra.Group.Commutator
-- import Mathlib.Algebra.Group.Commute
-- import Mathlib.Algebra.Group.Conj
-- import Mathlib.Algebra.Group.Defs
-- import Mathlib.Algebra.Group.Ext
-- import Mathlib.Algebra.Group.InjSurj
-- import Mathlib.Algebra.Group.Opposite
-- import Mathlib.Algebra.Group.OrderSynonym
-- import Mathlib.Algebra.Group.Pi
-- import Mathlib.Algebra.Group.Prod
-- import Mathlib.Algebra.Group.Semiconj
-- import Mathlib.Algebra.Group.TypeTags
-- import Mathlib.Algebra.Group.ULift
-- import Mathlib.Algebra.Group.Units
-- import Mathlib.Algebra.Group.WithOne.Basic
-- import Mathlib.Algebra.Group.WithOne.Defs
-- import Mathlib.Algebra.Group.WithOne.Units

open Lean Elab Meta Tactic

namespace PremiseSelection

syntax (name := suggestSimp) "suggest_simp" : tactic

def SimpTheoremsArray.addConst 
  (thmsArray : SimpTheoremsArray) (declName : Name) : MetaM SimpTheoremsArray :=
  if thmsArray.isEmpty then
    let thms : SimpTheorems := {}
    return #[ (← thms.addConst declName) ]
  else
    thmsArray.modifyM 0 fun thms => thms.addConst declName

@[tactic suggestSimp]
unsafe def suggestSimpTactic : Tactic := fun _ => do
  let features ← getGoalFeatures
  let e := unlabeled features
  let ps := Array.mk (rankingWithScores (← trainedForest) e)
  let fps := ps[:10].toArray
  let ns : Array Name := fps.map (fun (name, _) => name.toName)

  let mut simpCtx ← Lean.Meta.Simp.Context.mkDefault

  for n in ns do
    let cinfo? := (← getEnv).find? n
    if let some (ConstantInfo.thmInfo _) := cinfo? then 
      let simpTheorems ← SimpTheoremsArray.addConst simpCtx.simpTheorems n
      simpCtx := { simpCtx with simpTheorems }

  -- let mut lemmasStx : Syntax.TSepArray `Lean.Parser.Tactic.simpLemma "," := ⟨#[]⟩
  -- for n in ns do
  --   let stx ← `($(mkIdent n))
  --   lemmasStx := lemmasStx.push stx
  -- evalTactic (← `(tactic| simp [$lemmasStx,*]))
  
  if let (none, usedSimps) ← simpGoal (← getMainGoal) simpCtx then
    setGoals []

end PremiseSelection

def runTactic (tactic : TacticM Unit) (goal : MVarId) (mctx : MetavarContext) 
  : MetaM (List MVarId) := do
  let (_, tacticState) ←
    tactic
    |>.run { elaborator := .anonymous }
    |>.run { goals := [goal] }
    |>.run'
    |>.run' {} { mctx := mctx }
  return tacticState.goals

def simpTest : MetaM Unit := do
  let env ← getEnv
  let moduleNamesArray := env.header.moduleNames
  let moduleDataArray := env.header.moduleData

  for (moduleName, moduleData) in Array.zip moduleNamesArray moduleDataArray do
    let moduleNameComponents := moduleName.components
    if moduleNameComponents.length < 3 then 
      continue
    if moduleNameComponents[0]! != `Mathlib then
      continue
    if moduleNameComponents[1]! != `Algebra then
      continue
    if moduleNameComponents[2]! != `Group then
      continue
    
    let mut totalCount := 0
    let mut nontrivialCount := 0
    let mut solvedCount := 0
    for cinfo in moduleData.constants do
      if let ConstantInfo.thmInfo { name := n, type := ty, .. } := cinfo then 
        totalCount := totalCount + 1

        let goal ← mkFreshMVarId
        let mctx := (← getMCtx).addExprMVarDecl goal .anonymous (← getLCtx) (← getLocalInstances) ty
        
        let justSimp := Tactic.evalTactic (← `(tactic| try { simp }))
        let justSimpGoals ← runTactic justSimp goal mctx
        
        if justSimpGoals.length == 0 then
          continue

        nontrivialCount := nontrivialCount + 1

        let suggestSimp := Tactic.evalTactic (← `(tactic| try { suggest_simp }))
        let suggestSimpGoals ← runTactic suggestSimp goal mctx

        if suggestSimpGoals.length == 0 then
          solvedCount := solvedCount + 1

        dbg_trace s!"{n} {justSimpGoals.length} {suggestSimpGoals.length}"
        
    dbg_trace s!"{moduleName}: {solvedCount} / {nontrivialCount} / {totalCount}"

#eval simpTest
