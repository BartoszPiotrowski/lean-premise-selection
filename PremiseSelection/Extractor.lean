import Lean
import Mathlib.Control.Writer
import PremiseSelection.StatementFeatures 
import PremiseSelection.ProofSource 

namespace PremiseSelection

open Lean Lean.Elab Lean.Elab.Term Lean.Elab.Command Lean.Meta System

/-- Format used for training. All the features (arguments and theorems are
put together in a list tageged with `T` for theorem or `H` for hypotheses). -/
class ToInputFormat (α) where 
  features : α → String 
  labels   : α → String

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

instance : ToInputFormat TheoremPremises where 
  features tp := Id.run <| do
    let mut result : Array String := #[]
    for (⟨n1, n2⟩, _) in tp.features.bigramCounts do
      result := result.push s!"T:{n1}/{n2}"
    for arg in tp.argumentsFeatures do 
      for (⟨n1, n2⟩, _) in arg.bigramCounts do
        result := result.push s!"H:{n1}/{n2}"
    return " ".intercalate result.data
  labels tp := " ".intercalate (tp.premises.map toString)

/-- Holds the premise data for each theorem in a module. -/
structure ModulePremises where 
  module   : Name 
  theorems : Array TheoremPremises 

instance : ToJson ModulePremises where 
  toJson data := 
    Json.mkObj [
      ("module",   toJson data.module),
      ("theorems", toJson data.theorems)
    ]

instance : ToString ModulePremises where 
  toString := Json.pretty ∘ toJson

instance : ToInputFormat ModulePremises where 
  features mp := "\n".intercalate (mp.theorems.map ToInputFormat.features).data
  labels   mp := "\n".intercalate (mp.theorems.map ToInputFormat.labels).data

section Core

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
          if ! argFeats.bigramCounts.isEmpty then 
            argsFeats := argsFeats ++ [argFeats]
      pure $ TheoremPremises.mk n thmFeats argsFeats (← extractPremises v)
  | _ => pure none

end Core 

section Helpers

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

  return thmData 

def extractPremisesFromThmJson (stx : Syntax) : MetaM Json := 
  toJson <$> extractPremisesFromThm stx

/-- Extract and print premises from all the theorems in the context. -/
def extractPremisesFromCtx : MetaM (Array TheoremPremises) := do 
    let mut ctxData : Array TheoremPremises := #[]
    for (_, cinfo) in (← getEnv).constants.toList do 
      if let some data ← extractPremisesFromConstantInfo cinfo then
        ctxData := ctxData.push data

    return ctxData

def extractPremisesFromCtxJson : MetaM Json := 
  toJson <$> extractPremisesFromCtx

def extractPremisesFromCtxAndSave (f : System.FilePath) : MetaM Unit := do 
  let content ← Json.pretty <$> extractPremisesFromCtxJson 
  IO.FS.writeFile f content

end Helpers 

section FromImports

/-- Extract and print premises from all the theorems in the imports. -/
-- TODO: Save while visiting.
def extractPremisesFromImports (recursive : Bool) 
  : MetaM (Array ModulePremises) := do 
  let env ← getEnv
  let imports := env.imports.map (·.module)
  let moduleNamesArray := env.header.moduleNames
  let moduleDataArray := env.header.moduleData

  let mut modulePremisesArray : Array ModulePremises := #[] 
  for (name, moduleData) in Array.zip moduleNamesArray moduleDataArray do
    let isMathbinImport := name.getRoot == `Mathbin || name == `Mathbin
    if (recursive || imports.contains name) && isMathbinImport then 
      let mut theorems : Array TheoremPremises := #[]
      for cinfo in moduleData.constants do 
        if let some data ← extractPremisesFromConstantInfo cinfo then 
          theorems := theorems.push data
      let modulePremisesData := ModulePremises.mk name theorems
      modulePremisesArray := modulePremisesArray.push modulePremisesData

  return modulePremisesArray

def extractPremisesFromImportsJson : MetaM Json := 
  toJson <$>  
  extractPremisesFromImports (recursive := false)

def extractPremisesFromImportsAndSave (f : FilePath) : MetaM Unit := do 
  let content ← Json.pretty <$> extractPremisesFromImportsJson 
  IO.FS.writeFile f content

def extractPremisesFromAllImportsJson : MetaM Json := 
  toJson <$> 
  extractPremisesFromImports (recursive := false)

def extractPremisesFromAllImportsAndSave (f : FilePath) : MetaM Unit := do 
  let content ← Json.pretty <$> extractPremisesFromAllImportsJson 
  IO.FS.writeFile f content

/-- Call `extractPremisesFromImports`, then look at the source files and filter 
the theorems to only obtain the ones typed by the user. -/
def extractUserPremisesFromImports (recursive : Bool) 
  : MetaM (Array ModulePremises) := do 
  let mut moduleUserPremisesArray : Array ModulePremises := #[]
  for modulePremisesData in ← extractPremisesFromImports recursive do 
    let module := modulePremisesData.module
    if let some modulePath ← pathFromMathbinImport module then 
      let mut theorems : Array TheoremPremises := #[]
      for theoremPremises in modulePremisesData.theorems do 
        if let some source ← proofSource theoremPremises.name modulePath then
          let filtered := filterUserPremises theoremPremises.premises source
          dbg_trace s!"User premises for {theoremPremises.name} : {filtered}"
        else 
          dbg_trace s!"Could not find proof source for {theoremPremises.name}."
          continue

      let moduleUserPremises := ModulePremises.mk module theorems
      moduleUserPremisesArray := moduleUserPremisesArray.push moduleUserPremises
    else 
      dbg_trace s!"Could not find path for {module}, not filtering premises."
      moduleUserPremisesArray := moduleUserPremisesArray.push modulePremisesData

  return moduleUserPremisesArray

end FromImports

section Commands 

private def runAndPrint [ToJson α] (f : MetaM α) : CommandElabM Unit :=
  liftTermElabM <| do dbg_trace s!"{Json.pretty <| toJson <| ← f}"

elab "extract_premises_from_thm " id:term : command =>
  runAndPrint <| extractPremisesFromThm id

elab "extract_premises_from_ctx" : command =>
  runAndPrint <| extractPremisesFromCtx

elab "extract_premises_from_imports" : command =>
  runAndPrint <| extractPremisesFromImports (recursive := false)

elab "extract_premises_from_all_imports" : command =>
  runAndPrint <| extractPremisesFromImports (recursive := true)

elab "extract_user_premises_from_imports" : command =>
  runAndPrint <| extractUserPremisesFromImports (recursive := false)

elab "extract_user_premises_from_all_imports" : command =>
  runAndPrint <| extractUserPremisesFromImports (recursive := true)

end Commands 

end PremiseSelection
