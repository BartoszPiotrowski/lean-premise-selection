import Lean
import PremiseSelection 

open Lean Lean.Meta 

open PremiseSelection

set_option maxHeartbeats 100000000

unsafe def main (args : List String) : IO Unit := do
  let labelsPath   := args.get! 0
  let featuresPath := args.get! 1
  let recursive    := (args.drop 2).contains "--recursive"
  let user         := (args.drop 2).contains "--user"
  withImportModules [{ module := `Mathbin.Algebra.Group.Defs }] {} 0 fun env => do 
    let m := extractPremisesFromImportsToFiles recursive user labelsPath featuresPath
    let _ ← m.toIO { fileName := "", fileMap := default } { env := env }
    pure ()
