import Mathbin
import PremiseSelection.Extractor 

attribute [-instance] coeDecidableEq

open Lean Lean.Meta PremiseSelection

unsafe def main (args : List String) : IO Unit := do
  let labelsPath   := args.get! 0
  let featuresPath := args.get! 1
  -- TODO: This option should be removed, it is very inefficient.
  let recursive    := (args.drop 2).contains "+recursive"
  -- Add `--user` to the command to apply the user filter.
  let user         := (args.drop 2).contains "+user"
  -- If `--all` is set, then it will extract all of mathbin.
  let all          := (args.drop 2).contains "+all"

  let mut moduleNames := #[]
  if all then 
    let selectedModules := "data/modules"
    for moduleNameStr in ← IO.FS.lines selectedModules do 
      let moduleNameStr := moduleNameStr.trim
      if moduleNameStr.startsWith "Mathbin" then
        let decopmosedNameStr := (moduleNameStr.splitOn ".").map Name.mkSimple
        let moduleName := decopmosedNameStr.foldl Name.append Lean.Name.anonymous
        moduleNames := moduleNames.push moduleName
  else 
    -- TODO: Just for testing.
    moduleNames := #[`Mathbin.Algebra.Abs]

  withImportModules (moduleNames.data.map ({ module := · })) {} 0 fun env => do 
    let m := extractPremisesFromImportsToFiles false user labelsPath featuresPath
    let _ ← m.toIO { fileName := "", fileMap := default } { env := env }
    pure ()
