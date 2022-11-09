import Mathbin
import PremiseSelection.Extractor 

attribute [-instance] coeDecidableEq

open Lean Lean.Meta PremiseSelection

/-- Run this file to extract premises as follows: 
  
  `lean --run PremiseSelection/ExtractorRunner.lean data/test.labels data/test.features data/modules [+user]`

The first argument is the path to the labels file, the second argument is the 
path to the features file, the third argument is the path to the modules file (a
file consisting of a list of module names, one per line). Adding the option 
+user will try to filter user premises. 

This is quite a heavy task so you might
need to increase memory and time out, e.g. by adding 
  
  `--memory=4096 --timeout=10000000000000000`

after `lean`. -/
unsafe def main (args : List String) : IO Unit := do
  let labelsPath      := args.get! 0
  let featuresPath    := args.get! 1
  let selectedModules := args.get! 2

  -- Add `+user` to the command to apply the user filter.
  let user := (args.drop 3).contains "+user"

  let mut moduleNames := #[]
  for moduleNameStr in ← IO.FS.lines selectedModules do 
    let moduleNameStr := moduleNameStr.trim
    if moduleNameStr.startsWith "Mathbin" then
      let decopmosedNameStr := (moduleNameStr.splitOn ".").map Name.mkSimple
      let moduleName := decopmosedNameStr.foldl Name.append Lean.Name.anonymous
      moduleNames := moduleNames.push moduleName

  withImportModules (moduleNames.data.map ({ module := · })) {} 0 fun env => do 
    let m := extractPremisesFromImportsToFiles user labelsPath featuresPath
    let ctx : Core.Context := { 
      fileName      := "", 
      fileMap       := default, 
      maxHeartbeats := 100000000000000, 
      maxRecDepth   := 100000000000000 }
    let _ ← m.toIO ctx { env := env }
