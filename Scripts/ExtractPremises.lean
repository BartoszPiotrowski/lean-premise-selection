import Lean
import PremiseSelection 

open Lean Lean.Meta 

open PremiseSelection

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
    let mathbinAll := "lean_packages/mathlib3port/Mathbin/All.lean"
    for moduleImportStr in ← IO.FS.lines mathbinAll do 
      let moduleImportStr := moduleImportStr.trim
      if moduleImportStr.startsWith "import" then
        let moduleNameStr := moduleImportStr.drop 7
        if moduleNameStr.startsWith "Mathbin" then
          let decopmosedNameStr := (moduleNameStr.splitOn ".").map Name.mkSimple
          let moduleName := decopmosedNameStr.foldl Name.append Name.anonymous
          moduleNames := moduleNames.push moduleName
  else 
    -- TODO: Just for testing.
    moduleNames := #[`Mathbin.Algebra.Abs]

  withImportModules (moduleNames.data.map ({ module := · })) {} 0 fun env => do 
    let m := extractPremisesFromImportsToFiles false user labelsPath featuresPath
    let _ ← m.toIO { fileName := "", fileMap := default } { env := env }
    pure ()
