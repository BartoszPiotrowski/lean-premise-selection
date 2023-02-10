import Lean 
import Lean.Elab.Term
import Lean.Elab.Tactic.Basic
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

open Lean Lean.Elab
 
def run : MetaM Unit := do
  let env ← getEnv
  let moduleNamesArray := env.header.moduleNames
  let moduleDataArray := env.header.moduleData

  for (moduleName, moduleData) in Array.zip moduleNamesArray moduleDataArray do
    if moduleName.getRoot != `Mathlib then 
      continue
    
    for cinfo in moduleData.constants do
      if let ConstantInfo.thmInfo { name := n, type := ty, value := v, .. } := cinfo then 
        let mvarId ← mkFreshMVarId
        Lean.MVarId.assign mvarId ty
        
        -- let goals ← Term.TermElabM.run' <| do
        --   Tactic.setGoals [mvarId] 
        --   Tactic.run mvarId 
        --   |>.Tactic.evalTactic (← `(tactic| simp))
        
        -- dbg_trace s!"{n} {goals.length}"

#eval run
