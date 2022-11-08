import Lean
import Mathlib.Control.Writer
import PremiseSelection.StatementFeatures 
import PremiseSelection.ProofSource 

namespace PremiseSelection

open Lean Lean.Elab Lean.Elab.Term Lean.Elab.Command Lean.Meta System

/-- Format used for training. All the features (arguments and theorem) should be
put together in a sequence tageged with `T` for theorem or `H` for hypotheses. 
Premises are simply concatenated. -/
class ToFeaturesLabels (α) where 
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

instance : ToFeaturesLabels TheoremPremises where 
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

instance : ToFeaturesLabels ModulePremises where 
  features mp := 
    "\n".intercalate (mp.theorems.map ToFeaturesLabels.features).data
  labels   mp := 
    "\n".intercalate (mp.theorems.map ToFeaturesLabels.labels).data

section CoreExtractor

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
    forallTelescope ty <| fun args thm => do
      let thmFeats ← getStatementFeatures thm
      let mut argsFeats := []
      for arg in args do
        let argType ← inferType arg 
        if (← inferType argType).isProp then
          let argFeats ← getStatementFeatures argType 
          if ! argFeats.bigramCounts.isEmpty then 
            argsFeats := argsFeats ++ [argFeats]
      pure <| TheoremPremises.mk n thmFeats argsFeats (← extractPremises v)
  | _ => pure none

end CoreExtractor 

section Variants

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

/-- Extract and print premises from all the theorems in the context. -/
def extractPremisesFromCtx : MetaM (Array TheoremPremises) := do 
    let mut ctxData : Array TheoremPremises := #[]
    for (_, cinfo) in (← getEnv).constants.toList do 
      if let some data ← extractPremisesFromConstantInfo cinfo then
        ctxData := ctxData.push data
    return ctxData

end Variants 

section FromImports

/-- -/
private def extractPremisesFromModule 
  {ω : Type} [EmptyCollection ω] [Append ω] (insert : TheoremPremises → ω)
  (moduleName : Name) (moduleData : ModuleData) (user : Bool := false)
  : WriterT ω MetaM Unit := do
  let mut filter : Name → List Name → MetaM (List Name) := fun _ => pure
  if user then 
    if let some modulePath ← pathFromMathbinImport moduleName then 
      -- If user premises and path found, then create a filter looking at proof 
      -- source. If no proof source is found, no filter is applied.
      filter := fun thmName premises => do
        if let some source ← proofSource thmName modulePath then
          return filterUserPremises premises source
        else return premises
  -- Go through all theorems in the module, filter premises and write.
  for cinfo in moduleData.constants do 
    if let some data ← extractPremisesFromConstantInfo cinfo then 
      let filteredPremises ← filter data.name data.premises
      let filteredData := { data with premises := filteredPremises }
      tell (insert filteredData)
  pure ()

open IO IO.FS

instance : EmptyCollection (IO Unit) := ⟨pure ()⟩

instance : Append (IO Unit) := ⟨fun f g => f *> g⟩

/-- -/
def extractPremisesFromModuleToFiles 
  (moduleName : Name) (moduleData : ModuleData) (user : Bool := false)
  (labelsPath featuresPath : FilePath) 
  : MetaM Unit := do 
  let labelsHandle ← Handle.mk labelsPath Mode.write false
  let featuresHandle ← Handle.mk featuresPath Mode.write false

  let insert : TheoremPremises → IO Unit := fun data => do
    labelsHandle.putStrLn (ToFeaturesLabels.labels data)
    featuresHandle.putStrLn (ToFeaturesLabels.features data)

  let (_, resultHandler) ← 
    WriterT.run <| extractPremisesFromModule insert moduleName moduleData user
  resultHandler

/-- -/
def extractPremisesFromModuleToStructure
  (moduleName : Name) (moduleData : ModuleData) (user : Bool := false) 
  : MetaM ModulePremises := do 
  let insert : TheoremPremises → ModulePremises := fun data => 
    { module := moduleName, theorems := #[data] }
  
  have : EmptyCollection ModulePremises := 
    ⟨{ module := moduleName, theorems := #[] }⟩
  have : Append ModulePremises := 
    ⟨fun m1 m2 => 
      { module := moduleName, theorems := m1.theorems ++ m2.theorems }⟩

  let (_, result) ← 
    WriterT.run <| extractPremisesFromModule insert moduleName moduleData user
  pure result

/-- Extract and print premises from all the theorems in the imports. -/
private def extractPremisesFromImports 
  {ω : Type} [EmptyCollection ω] [Append ω] (insert : Name → TheoremPremises → ω)
  (recursive : Bool) (user : Bool := false)
  : MetaM ω := do 
  let env ← getEnv
  let imports := env.imports.map (·.module)
  let moduleNamesArray := env.header.moduleNames
  let moduleDataArray := env.header.moduleData

  dbg_trace "Extracting premises from imports"
  
  -- Write for every module, to avoid having to keep all the data in memory.
  let mut res : ω := {}
  for (moduleName, moduleData) in Array.zip moduleNamesArray moduleDataArray do
    let isMathbinImport := 
      moduleName.getRoot == `Mathbin || moduleName == `Mathbin
    if (recursive || imports.contains moduleName) && isMathbinImport then 
      dbg_trace s!"Extracting premises from {moduleName}."
      let extractFn := 
        extractPremisesFromModule (insert moduleName) moduleName moduleData user
      let (_, moduleRes) ← WriterT.run <| extractFn
      res := res ++ moduleRes
          
  pure res

/-- -/
def extractPremisesFromImportsToFiles 
  (recursive : Bool) (user : Bool := false) (labelsPath featuresPath : FilePath) 
  : MetaM Unit := do 
  dbg_trace "Extracting premises from imports to files."

  let labelsHandle ← Handle.mk labelsPath Mode.write false
  let featuresHandle ← Handle.mk featuresPath Mode.write false

  let insert : Name → TheoremPremises → IO Unit := fun _ tp => do
    labelsHandle.putStrLn (ToFeaturesLabels.labels tp)
    featuresHandle.putStrLn (ToFeaturesLabels.features tp)
  
  let evalIO ← extractPremisesFromImports insert recursive user
  evalIO

/-- -/
def extractPremisesFromImportsToStructure 
  (recursive : Bool) (user : Bool := false)
  : MetaM (Array ModulePremises) := do 
  let insert : Name → TheoremPremises → Array ModulePremises := 
    fun moduleName tp => #[ModulePremises.mk moduleName #[tp]]
  -- TODO: Append here won't merge two ModulePremises with the same module name.
  extractPremisesFromImports insert recursive user

end FromImports

section Json

def extractPremisesFromCtxJson : MetaM Json := 
  toJson <$> extractPremisesFromCtx

def extractPremisesFromThmJson (stx : Syntax) : MetaM Json := 
  toJson <$> extractPremisesFromThm stx

def extractPremisesFromImportsJson : MetaM Json := 
  toJson <$>  
  extractPremisesFromImportsToStructure (recursive := false)

def extractPremisesFromAllImportsJson : MetaM Json := 
  toJson <$> 
  extractPremisesFromImportsToStructure (recursive := true)

def extractUserPremisesFromImportsJson : MetaM Json := 
  toJson <$>  
  extractPremisesFromImportsToStructure (recursive := false) (user := true)

def extractUserPremisesFromAllImportsJson : MetaM Json := 
  toJson <$> 
  extractPremisesFromImportsToStructure (recursive := true) (user := true)

end Json

section Commands 

private def runAndPrint [ToJson α] (f : MetaM α) : CommandElabM Unit :=
  liftTermElabM <| do dbg_trace s!"{Json.pretty <| toJson <| ← f}"

elab "extract_premises_from_thm " id:term : command =>
  runAndPrint <| extractPremisesFromThm id

elab "extract_premises_from_ctx" : command =>
  runAndPrint <| extractPremisesFromCtx

elab "extract_premises_from_imports" : command =>
  runAndPrint <| extractPremisesFromImportsJson

elab "extract_premises_from_all_imports" : command =>
  runAndPrint <| extractPremisesFromAllImportsJson

elab "extract_user_premises_from_imports" : command =>
  runAndPrint <| extractUserPremisesFromImportsJson

elab "extract_user_premises_from_all_imports" : command =>
  runAndPrint <| extractUserPremisesFromAllImportsJson

syntax (name := extract_to_files) 
  "extract_to_files l:" str " f:" str : command

@[commandElab «extract_to_files»]
unsafe def elabExtractToFiles : CommandElab
| `(extract_to_files l:$lp f:$fp) => liftTermElabM <| do
  let labelsPath ← evalTerm String (mkConst `String) lp.raw
  let featuresPath ← evalTerm String (mkConst `String) fp.raw
  extractPremisesFromImportsToFiles (recursive := true) (user := true) labelsPath featuresPath
| _ => throwUnsupportedSyntax

end Commands 

end PremiseSelection
