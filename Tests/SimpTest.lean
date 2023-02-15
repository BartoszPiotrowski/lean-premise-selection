import Lean 
import Lean.Elab.Term
import Lean.Elab.Tactic.Basic
import PremiseSelection.Tactic

import Mathlib.Algebra.Group.Basic
import Mathlib.Algebra.Group.Commutator
import Mathlib.Algebra.Group.Commute
-- import Mathlib.Algebra.Group.Conj
import Mathlib.Algebra.Group.Defs
-- import Mathlib.Algebra.Group.Ext
import Mathlib.Algebra.Group.InjSurj
-- import Mathlib.Algebra.Group.Opposite
import Mathlib.Algebra.Group.OrderSynonym
-- import Mathlib.Algebra.Group.Pi
-- import Mathlib.Algebra.Group.Prod
import Mathlib.Algebra.Group.Semiconj
-- import Mathlib.Algebra.Group.TypeTags
-- import Mathlib.Algebra.Group.ULift
import Mathlib.Algebra.Group.Units
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

def simpTacticM : TacticM Nat := do 
  let n ← Tactic.tryTactic? <| do
    let mut simpCtx : Simp.Context ← Lean.Meta.Simp.Context.mkDefault
    let (result, usedSimps) ← simpGoal (← getMainGoal) simpCtx 
    if result == none then
      replaceMainGoal []
    
    return usedSimps.size

  return n.getD 0

def suggestSimpTacticM : TacticM Nat := do
  let n ← Tactic.tryTactic? <| do
    let features ← getGoalFeatures
    let e := unlabeled features
    let ps := Array.mk (rankingWithScores (← trainedForest) e)
    let fps := ps[:5].toArray
    let ns : Array Name := fps.map (fun (name, _) => name.toName)

    -- OLD APPROACH USING SIMP CTX.
    let mut simpCtx : Simp.Context := {} -- ← Lean.Meta.Simp.Context.mkDefault

    for n in ns do
      let cinfo? := (← getEnv).find? n
      if let some (ConstantInfo.thmInfo _) := cinfo? then 
        let simpTheorems ← SimpTheoremsArray.addConst simpCtx.simpTheorems n
        simpCtx := { simpCtx with simpTheorems }
    
    let (result, usedSimps) ← simpGoal (← getMainGoal) simpCtx 
    if result == none then
      replaceMainGoal []
    
    return usedSimps.size
  
  return n.getD 0
  
  -- NEW APPROACH USING SIMP TACTIC.
  -- let mut is : Array Ident := #[]

  -- for n in ns do
  --   let cinfo? := (← getEnv).find? n
  --   if let some (ConstantInfo.thmInfo _) := cinfo? then 
  --     is := is.push (mkIdent n)

  -- if is.size == 1 then 
  --   evalTactic (← `(tactic| try { intros ; simp only [*, $(is[0]!):term] }))
  -- else if is.size == 2 then 
  --   evalTactic (← `(tactic| try { intros ; simp only [*, $(is[0]!):term, $(is[1]!):term] }))
  -- else if is.size == 3 then 
  --   evalTactic (← `(tactic| try { intros ; simp only [*, $(is[0]!):term, $(is[1]!):term, $(is[2]!):term] }))
  -- else if is.size == 4 then 
  --   evalTactic (← `(tactic| try { intros ; simp only [*, $(is[0]!):term, $(is[1]!):term, $(is[2]!):term, $(is[3]!):term] }))
  -- else if is.size == 5 then 
  --   evalTactic (← `(tactic| try { intros ; simp only [*, $(is[0]!):term, $(is[1]!):term, $(is[2]!):term, $(is[3]!):term, $(is[4]!):term] }))

@[tactic suggestSimp]
unsafe def suggestSimpTactic : Tactic := fun _ => do let _ ← suggestSimpTacticM
  
def runTactic' (tactic : TacticM α) (goal : MVarId) (mctx : MetavarContext) 
  : MetaM (α × List MVarId) := do
  let (e, tacticState) ←
    tactic
    |>.run { elaborator := .anonymous }
    |>.run { goals := [goal] }
    |>.run'
    |> withLctx goal
    |>.run' {} { mctx := mctx }
  return (e, tacticState.goals)

end PremiseSelection

#check Tactic.tryTactic

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
    
    let mut total := 0
    let mut solvedBySimp := 0
    let mut solvedBySuggestSimp := 0
    let mut totalUsedSimp := 0
    let mut totalUsedSuggestSimp := 0

    dbg_trace s!"-- {moduleName} --"

    for cinfo in moduleData.constants do
      if let ConstantInfo.thmInfo { name := n, type := ty, .. } := cinfo then 
        total := total + 1

        let goal ← mkFreshMVarId
        let mctx := ({} : MetavarContext).addExprMVarDecl goal .anonymous {} #[] ty
        
        let justSimp := PremiseSelection.simpTacticM
        let (usedSimp, justSimpGoals) ← 
          PremiseSelection.runTactic' justSimp goal mctx
        
        totalUsedSimp := totalUsedSimp + usedSimp

        if justSimpGoals.length == 0 then
          solvedBySimp := solvedBySimp + 1
          dbg_trace s!"Successful simp: {n}"

        let suggestSimp := PremiseSelection.suggestSimpTacticM
        let (usedSuggestSimp, suggestSimpGoals) ← 
          PremiseSelection.runTactic' suggestSimp goal mctx

        totalUsedSuggestSimp := totalUsedSuggestSimp + usedSuggestSimp

        if suggestSimpGoals.length == 0 then
          solvedBySuggestSimp := solvedBySuggestSimp + 1
          dbg_trace s!"Successful suggest_simp: {n}"

    dbg_trace s!"Total theorems         : {total}"
    dbg_trace s!"Solved by simp         : {solvedBySimp}"
    dbg_trace s!"Solved by suggest_simp : {solvedBySuggestSimp}"
    dbg_trace s!"Used by simp           : {totalUsedSimp}"
    dbg_trace s!"Used by suggest_simp   : {totalUsedSuggestSimp}"

#eval simpTest
