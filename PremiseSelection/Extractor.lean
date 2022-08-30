import Lean
import Mathlib.Control.Writer
import PremiseSelection.StatementFeatures 

namespace PremiseSelection

open Lean Lean.Elab Lean.Elab.Term Lean.Elab.Command Lean.Meta

-- TODO: Testing. Move to StatementFeatures with correct representation.
instance : ToString StatementFeatures where 
  toString features := Id.run <| do
    let mut output : List String := []
    for (⟨n1, n2⟩, _) in features.bigramCounts do
      output := output.append [s!"{n1}/{n2}"]
    return " + ".intercalate output

structure PremisesData where 
  theoremName        : Name 
  theoremFeatures    : StatementFeatures
  argumentsFeatures  : List StatementFeatures
  premises           : List Name 

instance : ToJson PremisesData where 
  toJson data := Id.run <| do
    -- NOTE: Ignoring count for now.
    let nameJson : Json := toString data.theoremName
    let thmFeatJson : Json := toString data.theoremFeatures
    let argsFeatJson := 
      Json.arr $ (Array.mk data.argumentsFeatures).map (Json.str ∘ toString)
    let premisesJson : Json := 
      Json.arr $ (Array.mk data.premises.eraseDup).map (Json.str ∘ toString)
    Json.mkObj [
      ("theoremName", nameJson),
      ("theoremFeature", thmFeatJson),
      ("argumentsFeatuters", argsFeatJson),
      ("premises", premisesJson)
    ]

instance : ToString PremisesData where 
  toString tp := Json.pretty (toJson tp)

private def getTheoremFromName (n : Name) : MetaM (List Name) := do 
  pure [n]
  --if let ConstantInfo.thmInfo _ := (← getEnv).find? n then pure [n] else pure []

private def getTheoremFromExpr (e : Expr) : MetaM (List Name) := do
  if let .const n _ := e then getTheoremFromName n else pure []

private def visitPremise (e : Expr) : WriterT (List Name) MetaM Unit  := do
  tell <| ← getTheoremFromExpr e
  return ()

def extractPremises (e : Expr) : MetaM (List Name) := do 
  let ((), premises) ← WriterT.run <| forEachExpr visitPremise e
  pure premises

/- Given a `ConstantInfo` that holds theorem data, it finds the premises used in
the proof and constructs an object of type `PremisesData` with all-/
def extractPremisesFromConstantInfo : ConstantInfo → MetaM (Option PremisesData)
  | ConstantInfo.thmInfo { name := n, type := ty, value := v, .. } => do
    let (args, _, thm) ← forallMetaTelescope ty 
    let thmFeats ← getStatementFeatures thm
    let mut argsFeats := []
    for arg in args do
      let arg ← inferType arg 
      let argType ← inferType arg 
      dbg_trace s!"{arg} +++ {argType}"
      if argType.isProp then
        let argFeats ← getStatementFeatures arg 
        argsFeats := argsFeats ++ [argFeats]
    pure $ PremisesData.mk n thmFeats argsFeats (← extractPremises v)
  | _ => pure none

/- Same as `extractPremisesFromConstantInfo` but take an idenitfier and gets 
its information from the environment. -/
def extractPremisesFromId (id : Name) : MetaM (Option PremisesData) := do
  match (← getEnv).find? id with 
  | some cinfo => extractPremisesFromConstantInfo cinfo
  | none => pure none

/- Same as `extractPremisesFromId` but takes syntax and first finds all the 
identifiers. -/
def extractPremisesFromSyntax (stx : Syntax) : MetaM Unit := do
  let ns ← resolveGlobalConst stx
  for n in ns do 
    match ← extractPremisesFromId n with 
    | some tp => dbg_trace s!"{tp}"
    | none => continue

section Commands 

syntax (name := extract_premises_thm) "extract_premises_thm " term : command

@[commandElab «extract_premises_thm»]
def elabExtractPremisesThm : CommandElab
  | `(extract_premises_thm $id:ident) => 
    liftTermElabM <| liftM <| extractPremisesFromSyntax id
  | _ => throwUnsupportedSyntax

/- Extract premises from all the theorems in the context. -/
def extractPremisesCtx : MetaM Unit := do 
    let cs := (← getEnv).constants.toList
    for (_, cinfo) in cs do 
      match ← extractPremisesFromConstantInfo cinfo with 
      | some tp => dbg_trace s!"{tp}"
      | none => continue

syntax (name := extract_premises_ctx) "extract_premises_ctx" : command

@[commandElab «extract_premises_ctx»]
def elabExtractPremisesCtx : CommandElab
  | `(extract_premises_ctx) => 
    liftTermElabM <| liftM <| extractPremisesCtx
  | _ => throwUnsupportedSyntax

/- Extract premises from all the theorems in the imports. -/
def extractPremisesImports : MetaM Unit := do 
  let env ← getEnv
  let imports := env.imports.map (·.module)
  let moduleNames := env.header.moduleNames
  let moduleData := env.header.moduleData
  for (n, d) in Array.zip moduleNames moduleData do
    if imports.contains n ∧ n != `Init ∧ n != `Extractor then 
      dbg_trace s!"Module {n}"
      for cinfo in d.constants do 
        match ← extractPremisesFromConstantInfo cinfo with 
        | some tp => dbg_trace s!"{tp}"
        | none => continue 

syntax (name := extract_premises_imports) "extract_premises_imports" : command

@[commandElab «extract_premises_imports»]
def elabExtractPremisesImports : CommandElab
  | `(extract_premises_imports) => 
    liftTermElabM <| liftM <| extractPremisesImports
  | _ => throwUnsupportedSyntax

end Commands 

end PremiseSelection
