import Lean
import Mathlib.Control.Writer
import PremiseSelection.StatementFeatures 

namespace PremiseSelection

open Lean Lean.Elab Lean.Elab.Term Lean.Elab.Command Lean.Meta System

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
  -- NOTE: Get all consts whose type is of type Prop.
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
private def extractPremisesFromConstantInfo 
  : ConstantInfo → MetaM (Option TheoremPremises)
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

def extractPremisesFromCtxAndSave (f : System.FilePath) : MetaM Unit := do 
  let content ← Json.pretty <$> extractPremisesFromCtxJson 
  IO.FS.writeFile f content

/-- Extract and print premises from all the theorems in the imports. -/
def extractPremisesFromImports (allMathbin : Bool) (recursive : Bool) 
  : MetaM (Array ModulePremises) := do 
  let env ← getEnv
  let imports := env.imports.map (·.module)
  let moduleNamesArray := env.header.moduleNames
  let moduleDataArray := env.header.moduleData

  let mut modulePremisesArray : Array ModulePremises := #[] 
  for (name, moduleData) in Array.zip moduleNamesArray moduleDataArray do
    -- TODO: Recurse through modules if allMathbin.
    let isMathbinImport := 
      name.getRoot == `Mathbin ∨ (name == `Mathbin && allMathbin)
    if imports.contains name ∧ isMathbinImport then 
      let mut theorems : Array TheoremPremises := #[]
      for cinfo in moduleData.constants do 
        if let some data ← extractPremisesFromConstantInfo cinfo then 
          theorems := theorems.push data
      let modulePremisesData := ModulePremises.mk name theorems
      modulePremisesArray := modulePremisesArray.push modulePremisesData

  dbg_trace s!"{modulePremisesArray}"
  return modulePremisesArray

def extractPremisesFromImportsJson : MetaM Json := 
  toJson <$>  
  extractPremisesFromImports (allMathbin := false) (recursive := false)

def extractPremisesFromImportsAndSave (f : FilePath) : MetaM Unit := do 
  let content ← Json.pretty <$> extractPremisesFromImportsJson 
  IO.FS.writeFile f content

def extractPremisesFromAllImportsJson : MetaM Json := 
  toJson <$> 
  extractPremisesFromImports (allMathbin := true) (recursive := false)

def extractPremisesFromAllImportsAndSave (f : FilePath) : MetaM Unit := do 
  let content ← Json.pretty <$> extractPremisesFromAllImportsJson 
  IO.FS.writeFile f content

/-- Call `extractPremisesFromImports`, then look at the source files and filter 
the theorems to only obtain the ones typed by the user. -/
def extractUserPremisesFromImports (allMathbin : Bool) (recursive : Bool) 
  : MetaM (Array ModulePremises) := do 
  let mut moduleUserPremisesArray : Array ModulePremises := #[]
  for modulePremisesData in ← extractPremisesFromImports allMathbin recursive do 
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

section Commands 

elab "extract_premises_from_thm " id:term : command =>
  liftTermElabM <| do let _ ← extractPremisesFromThm id

elab "extract_premises_from_ctx" : command =>
  liftTermElabM <| do let _ ← extractPremisesFromCtx

-- TODO: Unify commands with options.

elab "extract_premises_from_imports" : command =>
  liftTermElabM <| do 
    let _ ← extractPremisesFromImports 
      (allMathbin := false) (recursive := false)

elab "extract_premises_from_all_imports" : command =>
  liftTermElabM <| do 
    let _ ← extractPremisesFromImports 
      (allMathbin := true) (recursive := false)

elab "extract_user_premises_from_imports" : command =>
  liftTermElabM <| do 
    let _ ← extractUserPremisesFromImports 
      (allMathbin := false) (recursive := false)

elab "extract_user_premises_from_all_imports" : command =>
  liftTermElabM <| do 
    let _ ← extractUserPremisesFromImports 
      (allMathbin := true) (recursive := false)

end Commands 

end PremiseSelection
