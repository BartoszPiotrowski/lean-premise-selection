import Lean
import PremiseSelection 

open Lean Lean.Meta 

open PremiseSelection

set_option maxHeartbeats 10000000000

unsafe def main (args : List String) : IO Unit := do
  let labelsPath   := args.get! 0
  let featuresPath := args.get! 1
  withImportModules [{ module := `Mathbin }] {} 0 fun env => do 
    let m := extractPremisesFromImportsToFiles true true labelsPath featuresPath
    let _ ← m.toIO { fileName := "", fileMap := default } { env := env }
    pure ()
