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

/-- Holds the name, features and premises of a single theorem. -/
structure TheoremPremises where 
  name              : Name 
  features          : StatementFeatures
  argumentsFeatures : List StatementFeatures
  premises          : List Name 

instance : ToJson TheoremPremises where 
  toJson data := 
    Json.mkObj [
      ("name",              toJson data.name),
      ("features",          toJson data.features),
      ("argumentsFeatures", toJson data.argumentsFeatures),
      ("premises",          toJson data.premises.eraseDup)
    ]

instance : ToString TheoremPremises where 
  toString := Json.pretty ∘ toJson

/-- Holds the premise data for each theorem in a module. -/
structure ModulePremises where 
  name     : Name 
  theorems : Array TheoremPremises 

instance : ToJson ModulePremises where 
  toJson data := 
    Json.mkObj [
      ("name",     toJson data.name),
      ("theorems", toJson data.theorems)
    ]

instance : ToString ModulePremises where 
  toString := Json.pretty ∘ toJson

/-- Given a name `n`, if it qualifies as a premise, it returns `[n]`, otherwise 
it returns the empty list. -/
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

private def extractPremises (e : Expr) : MetaM (List Name) := do 
  let ((), premises) ← WriterT.run <| forEachExpr visitPremise e
  pure premises

/-- Given a `ConstantInfo` that holds theorem data, it finds the premises used 
in the proof and constructs an object of type `PremisesData` with all. -/
private def extractPremisesFromConstantInfo : ConstantInfo → MetaM (Option TheoremPremises)
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
      pure $ TheoremPremises.mk n thmFeats argsFeats (← extractPremises v)
  | _ => pure none

/-- Same as `extractPremisesFromConstantInfo` but take an idenitfier and gets 
its information from the environment. -/
def extractPremisesFromId (id : Name) : MetaM (Option TheoremPremises) := do
  if let some cinfo := (← getEnv).find? id then 
    extractPremisesFromConstantInfo cinfo
  else pure none

/-- Extract and print premises from a single theorem. -/
def extractPremisesFromThm (stx : Syntax) : MetaM (Array TheoremPremises) := do
  let mut thmData : Array TheoremPremises := #[]
  for n in ← resolveGlobalConst stx do 
    if let some data ← extractPremisesFromId n then
      thmData := thmData.push data

  dbg_trace s!"{thmData}"
  return thmData 

def extractPremisesFromThmJson (stx : Syntax) : MetaM Json := 
  toJson <$> extractPremisesFromThm stx

/-- Extract and print premises from all the theorems in the context. -/
def extractPremisesFromCtx : MetaM (Array TheoremPremises) := do 
    let mut ctxData : Array TheoremPremises := #[]
    for (_, cinfo) in (← getEnv).constants.toList do 
      if let some data ← extractPremisesFromConstantInfo cinfo then
        ctxData := ctxData.push data

    dbg_trace s!"{ctxData}"
    return ctxData

def extractPremisesFromCtxJson : MetaM Json := 
  toJson <$> extractPremisesFromCtx

/-- Extract and print premises from all the theorems in the imports. -/
def extractPremisesFromImports : MetaM (Array ModulePremises) := do 
  let env ← getEnv
  let imports := env.imports.map (·.module)
  let moduleNamesArray := env.header.moduleNames
  let moduleDataArray := env.header.moduleData

  let mut modulePremisesArray : Array ModulePremises := #[] 
  for (name, moduleData) in Array.zip moduleNamesArray moduleDataArray do
    -- NOTE: Ignore Init, Mathbin and PremiseSelection.
    let isUserImport := name != `Init ∧ name != `Mathbin
    let isNotPremiseSelection := name.getRoot != `PremiseSelection
    if imports.contains name ∧ isUserImport ∧ isNotPremiseSelection then 
      let mut theorems : Array TheoremPremises := #[]
      for cinfo in moduleData.constants do 
        if let some data ← extractPremisesFromConstantInfo cinfo then 
          theorems := theorems.push data
      let modulePremisesData := ModulePremises.mk name theorems
      modulePremisesArray := modulePremisesArray.push modulePremisesData

  dbg_trace s!"{modulePremisesArray}"
  return modulePremisesArray

def extractPremisesFromImportsJson : MetaM Json := 
  toJson <$> extractPremisesFromImports

/-- Call `extractPremisesFromImports`, then look at the source files and filter 
the theorems to only obtain the ones typed by the user. -/
def extractUserPremisesFromImports : MetaM (Array ModulePremises) := do 
  let mut moduleUserPremisesArray : Array ModulePremises := #[]
  for modulePremisesData in ← extractPremisesFromImports do 
    let name := modulePremisesData.name
    let userText ← userTextFromImport name
    let mut frontIter := userText.mkIterator
    let mut backIter := userText.mkIterator

    let mut theorems : Array TheoremPremises := #[]
    for theoremPremises in modulePremisesData.theorems do 
      let theoremDef := "theorem " ++ toString theoremPremises.name
      backIter := backIter.forward theoremDef.length
      -- Find beginning of theorem definition.
      while (frontIter.extract backIter) != theoremDef ∧ backIter.hasNext do 
        frontIter := frontIter.next
        backIter := backIter.next
      -- Keep going until next theorem or end of file.
      while (frontIter.extract backIter) != "theorem" ∧ backIter.hasNext do 
        backIter := backIter.next
      -- Extract block with the rpoof of the theorem.
      let block := frontIter.extract backIter
      dbg_trace s!"{theoremDef} +++ {block}"
      -- NOTE: THIS DOES NOT WORK, e.g. @[to_additive] stuff....
      -- Reset iterators for next search.
      backIter := backIter.prevn 7 
      frontIter := backIter 

      break

    let moduleUserPremises := ModulePremises.mk name theorems
    moduleUserPremisesArray := moduleUserPremisesArray.push moduleUserPremises

  dbg_trace s!"{moduleUserPremisesArray}"
  return moduleUserPremisesArray
  where 
    userTextFromImport (mod : Name) : MetaM String := do 
      let mathbinPath : System.FilePath := 
        "." / "lean_packages" / "mathlib3port"
      if let some path ← SearchPath.findWithExt [mathbinPath] "lean" mod then 
        IO.FS.readFile path
      else return ""

-- NOTE: The commands are only used for testing.
section Commands 

syntax (name := extract_premises_from_thm) "extract_premises_from_thm " term : command

@[commandElab «extract_premises_from_thm»]
def elabExtractPremisesFromThm : CommandElab
  | `(extract_premises_from_thm $id:ident) => 
    liftTermElabM <| liftM <| do let _ ← extractPremisesFromThm id
  | _ => throwUnsupportedSyntax

syntax (name := extract_premises_from_ctx) "extract_premises_from_ctx" : command

@[commandElab «extract_premises_from_ctx»]
def elabExtractPremisesFromCtx : CommandElab
  | `(extract_premises_from_ctx) => 
    liftTermElabM <| liftM <| do let _ ← extractPremisesFromCtx
  | _ => throwUnsupportedSyntax

syntax (name := extract_premises_from_imports) "extract_premises_from_imports" : command

@[commandElab «extract_premises_from_imports»]
def elabExtractPremisesFromImports : CommandElab
  | `(extract_premises_from_imports) => 
    liftTermElabM <| liftM <| do let _ ← extractPremisesFromImports
  | _ => throwUnsupportedSyntax

syntax (name := extract_user_premises_from_imports) "extract_user_premises_from_imports" : command

@[commandElab «extract_user_premises_from_imports»]
def elabExtractUserPremisesFromImports : CommandElab
  | `(extract_user_premises_from_imports) => 
    liftTermElabM <| liftM <| do let _ ← extractUserPremisesFromImports
  | _ => throwUnsupportedSyntax


end Commands 

end PremiseSelection
