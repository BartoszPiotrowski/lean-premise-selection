import Lean
import Mathlib.Control.Writer
import PremiseSelection.StatementFeatures 

namespace PremiseSelection

open Lean Lean.Elab Lean.Elab.Term Lean.Elab.Command Lean.Meta

-- TODO: Testing. Move to StatementFeatures with correct representation.
instance : ToJson StatementFeatures where 
  toJson features := Id.run <| do
    -- NOTE: Ignoring count for now.
    let mut jsonFeatures : Array Json := #[]
    for (⟨n1, n2⟩, _) in features.bigramCounts do
      jsonFeatures := jsonFeatures.push s!"{n1}/{n2}"
    return Json.arr jsonFeatures

structure PremisesData where 
  theoremName        : Name 
  theoremFeatures    : StatementFeatures
  argumentsFeatures  : List StatementFeatures
  premises           : List Name 

instance : ToJson PremisesData where 
  toJson data := Id.run <| do
    Json.mkObj [
      ("theoremName", toJson data.theoremName),
      ("theoremFeatures", toJson data.theoremFeatures),
      ("argumentsFeatures", toJson data.argumentsFeatures),
      ("premises", toJson data.premises.eraseDup)
    ]

instance : ToString PremisesData where 
  toString tp := Json.pretty (toJson tp)

private def getTheoremFromName (n : Name) : MetaM (List Name) := do 
  -- NOTE: Option 1. Get all consts.
  --pure [n]
  -- NOTE: Option 2. Get all theorems.
  --if let ConstantInfo.thmInfo _ := (← getEnv).find? n then pure [n] else pure []
  -- NOTE: Option 3. Get all consts that are Props.
  if let some cinfo := (← getEnv).find? n then
    if (← inferType cinfo.type).isProp then pure [n] else pure []
  else pure []

private def getTheoremFromExpr (e : Expr) : MetaM (List Name) := do
  if let .const n _ := e then getTheoremFromName n else pure []

private def visitPremise (e : Expr) : WriterT (List Name) MetaM Unit := do
  getTheoremFromExpr e >>= tell

def extractPremises (e : Expr) : MetaM (List Name) := do 
  let ((), premises) ← WriterT.run <| forEachExpr visitPremise e
  pure premises

/- Given a `ConstantInfo` that holds theorem data, it finds the premises used in
the proof and constructs an object of type `PremisesData` with all-/
def extractPremisesFromConstantInfo : ConstantInfo → MetaM (Option PremisesData)
  | ConstantInfo.thmInfo { name := n, type := ty, value := v, .. } => do
    forallTelescope ty $ fun args thm => do
      let thmFeats ← getStatementFeatures thm
      let mut argsFeats := []
      for arg in args do
        let argType ← inferType arg 
        if (← inferType argType).isProp then
          let argFeats ← getStatementFeatures argType 
          if ¬ argFeats.bigramCounts.isEmpty then 
            argsFeats := argsFeats ++ [argFeats]
      pure $ PremisesData.mk n thmFeats argsFeats (← extractPremises v)
  | _ => pure none

/- Same as `extractPremisesFromConstantInfo` but take an idenitfier and gets 
its information from the environment. -/
def extractPremisesFromId (id : Name) : MetaM (Option PremisesData) := do
  match (← getEnv).find? id with 
  | some cinfo => extractPremisesFromConstantInfo cinfo
  | none => pure none

section Commands 

/- Extract and print premises from a single theorem. -/
def extractPremisesThm (stx : Syntax) : MetaM Json := do
  let ns ← resolveGlobalConst stx
  let mut output : Array Json := #[]
  for n in ns do 
    if let some data ← extractPremisesFromId n then
      output := output.push (toJson data)
      dbg_trace s!"{data}"
  return if output.size == 1 then output[0]! else Json.arr output

syntax (name := extract_premises_thm) "extract_premises_thm " term : command

@[commandElab «extract_premises_thm»]
def elabExtractPremisesThm : CommandElab
  | `(extract_premises_thm $id:ident) => 
    liftTermElabM <| liftM <| do let _ ← extractPremisesThm id
  | _ => throwUnsupportedSyntax

/- Extract and print premises from all the theorems in the context. -/
def extractPremisesCtx : MetaM Json := do 
    let cs := (← getEnv).constants.toList
    let mut output : Array Json := #[]
    for (_, cinfo) in cs do 
      if let some data ← extractPremisesFromConstantInfo cinfo then
        output := output.push (toJson data)
        dbg_trace s!"{data}"
    return Json.arr output

syntax (name := extract_premises_ctx) "extract_premises_ctx" : command

@[commandElab «extract_premises_ctx»]
def elabExtractPremisesCtx : CommandElab
  | `(extract_premises_ctx) => 
    liftTermElabM <| liftM <| do let _ ← extractPremisesCtx
  | _ => throwUnsupportedSyntax

/- Extract and print premises from all the theorems in the imports. -/
def extractPremisesImports : MetaM Json := do 
  let env ← getEnv
  let imports := env.imports.map (·.module)
  let moduleNames := env.header.moduleNames
  let moduleData := env.header.moduleData

  let mut output : Array Json := #[] 
  for (n, d) in Array.zip moduleNames moduleData do
    -- Ignore Init, Mathbin and PremiseSelection.
    let userImport := n != `Init ∧ n != `Mathib ∧ n != `PremiseSelection
    if imports.contains n ∧ userImport then 
      dbg_trace s!"Module {n}"
      let mut theoremsData : Array Json := #[]
      for cinfo in d.constants do 
        if let some data ← extractPremisesFromConstantInfo cinfo then 
          theoremsData := theoremsData.push (toJson data)
          dbg_trace s!"{data}"
      let moduleData := 
        Json.mkObj [("module", toJson n), ("theorems", Json.arr theoremsData)]
      output := output.push moduleData
    
  return Json.arr output

syntax (name := extract_premises_imports) "extract_premises_imports" : command

@[commandElab «extract_premises_imports»]
def elabExtractPremisesImports : CommandElab
  | `(extract_premises_imports) => 
    liftTermElabM <| liftM <| do let _ ← extractPremisesImports
  | _ => throwUnsupportedSyntax

end Commands 

end PremiseSelection
