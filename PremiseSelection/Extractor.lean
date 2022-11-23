import Lean
import Mathlib.Control.Writer
import PremiseSelection.StatementFeatures 
import PremiseSelection.ProofSource 

namespace PremiseSelection

open Lean Lean.Elab Lean.Elab.Term Lean.Elab.Command Lean.Meta System

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

/-- Used to choose the feature format: nameCounts and/or bigramCounts and/or 
subexpressions -/
structure FeatureFormat where 
  n : Bool 
  b : Bool
  s : Bool

/-- Features used for training. All the features (arguments and theorem) should 
be put together in a sequence tageged with `T` for theorem or `H` for 
hypotheses.  -/
def getFeatures (tp : TheoremPremises) (format : FeatureFormat) : String := 
  Id.run <| do
    let mut result : Array String := #[]
    if format.n then
      for (n, _) in tp.features.nameCounts do
        result := result.push s!"T:{n}"
      for arg in tp.argumentsFeatures do 
        for (n, _) in arg.nameCounts do
          result := result.push s!"H:{n}"
    if format.b then 
      for (⟨n1, n2⟩, _) in tp.features.bigramCounts do
        result := result.push s!"T:{n1}/{n2}"
      for arg in tp.argumentsFeatures do 
        for (⟨n1, n2⟩, _) in arg.bigramCounts do
          result := result.push s!"H:{n1}/{n2}"
    if format.s then 
      for (n, _) in tp.features.subexpressions do
        result := result.push s!"T:{n}"
      for arg in tp.argumentsFeatures do 
        for (n, _) in arg.subexpressions do
          result := result.push s!"H:{n}"
    return " ".intercalate result.data

/-- Premises are simply concatenated. -/
def getLabels (tp : TheoremPremises) : String :=
  " ".intercalate (tp.premises.map toString)

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
      -- Heuristic to avoid long executions for deep theorems.
      if v.approxDepth < 128 then 
        pure <| TheoremPremises.mk n thmFeats argsFeats (← extractPremises v)
      else 
        pure none
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

open IO IO.FS

/-- Given a way to insert `TheoremPremises`, this function goes through all
the theorems in a module, extracts the premises filtering them appropriately 
and inserts the resulting data. -/
private def extractPremisesFromModule 
  (insert : TheoremPremises → IO Unit)
  (moduleName : Name) (moduleData : ModuleData) (user : Bool := false)
  : MetaM Unit := do
  dbg_trace s!"Extracting premises from {moduleName}."
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
    -- Ignore non-user definitions.
    let nameStr := toString cinfo.name
    if nameStr.contains '!' || 
       nameStr.contains '«' || 
       "_eqn_".isSubstrOf nameStr ||
       "_proof_".isSubstrOf nameStr || 
       "_match_".isSubstrOf nameStr then 
      continue
    if let some data ← extractPremisesFromConstantInfo cinfo then 
      let filteredPremises ← filter data.name data.premises
      let filteredData := { data with premises := filteredPremises }
      insert filteredData
  pure ()

/-- Call `extractPremisesFromModule` with an insertion mechanism that writes
to the specified files for labels and features. -/
def extractPremisesFromModuleToFiles 
  (moduleName : Name) (moduleData : ModuleData) 
  (user : Bool := false) (ff : FeatureFormat)
  (labelsPath featuresPath : FilePath) 
  : MetaM Unit := do 
  let labelsHandle ← Handle.mk labelsPath Mode.append false
  let featuresHandle ← Handle.mk featuresPath Mode.append false

  let insert : TheoremPremises → IO Unit := fun data => do
    labelsHandle.putStrLn (getLabels data)
    featuresHandle.putStrLn (getFeatures data ff)

  extractPremisesFromModule insert moduleName moduleData user

/-- Looks through all the meaningful imports and applies 
`extractPremisesFromModuleToFiles` to each of them. -/
def extractPremisesFromImportsToFiles 
  (user : Bool := false) (format : FeatureFormat) 
  (labelsPath featuresPath : FilePath) 
  : MetaM Unit := do 
  dbg_trace "Extracting premises from imports to {labelsPath}, {featuresPath}."

  let env ← getEnv
  let imports := env.imports.map (·.module)
  let moduleNamesArray := env.header.moduleNames
  let moduleDataArray := env.header.moduleData

  let mut count := 0
  for (moduleName, moduleData) in Array.zip moduleNamesArray moduleDataArray do
    let isMathbinImport := 
      moduleName.getRoot == `Mathbin || moduleName == `Mathbin
    if imports.contains moduleName && isMathbinImport then 
      count := count + 1
      extractPremisesFromModuleToFiles 
        moduleName moduleData user format labelsPath featuresPath
      dbg_trace s!"count = {count}."
          
  pure ()

end FromImports

section Json

def extractPremisesFromCtxJson : MetaM Json := 
  toJson <$> extractPremisesFromCtx

def extractPremisesFromThmJson (stx : Syntax) : MetaM Json := 
  toJson <$> extractPremisesFromThm stx

end Json

section Commands 

private def runAndPrint [ToJson α] (f : MetaM α) : CommandElabM Unit :=
  liftTermElabM <| do dbg_trace s!"{Json.pretty <| toJson <| ← f}"

elab "extract_premises_from_thm " id:term : command =>
  runAndPrint <| extractPremisesFromThm id

elab "extract_premises_from_ctx" : command =>
  runAndPrint <| extractPremisesFromCtx

syntax (name := extract_premises_to_files) 
  "extract_premises_to_files l:" str " f:" str : command

@[commandElab «extract_premises_to_files»]
unsafe def elabExtractPremisesToFiles : CommandElab
| `(extract_premises_to_files l:$lp f:$fp) => liftTermElabM <| do
  let labelsPath ← evalTerm String (mkConst `String) lp.raw
  let featuresPath ← evalTerm String (mkConst `String) fp.raw
  let user := true 
  let format : FeatureFormat := { n := false, b := true, s:= false }
  extractPremisesFromImportsToFiles user format labelsPath featuresPath
| _ => throwUnsupportedSyntax

end Commands 

end PremiseSelection
