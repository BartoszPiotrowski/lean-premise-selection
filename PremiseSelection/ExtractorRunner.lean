import Mathbin
import PremiseSelection.Extractor 

attribute [-instance] coeDecidableEq

open Lean Lean.Meta PremiseSelection

unsafe def main (args : List String) : IO Unit := do
  let labelsPath   := args.get! 0
  let featuresPath := args.get! 1
  -- Add `+user` to the command to apply the user filter.
  let user         := (args.drop 2).contains "+user"

  let mut moduleNames := #[]

  let selectedModules := "data/testmodules"
  for moduleNameStr in ← IO.FS.lines selectedModules do 
    let moduleNameStr := moduleNameStr.trim
    if moduleNameStr.startsWith "Mathbin" then
      let decopmosedNameStr := (moduleNameStr.splitOn ".").map Name.mkSimple
      let moduleName := decopmosedNameStr.foldl Name.append Lean.Name.anonymous
      moduleNames := moduleNames.push moduleName

  -- for i in List.range (moduleNames.size.div 100) do
  --   let chunk := moduleNames.extract (i.mul 100) ((i.mul 100).add 100)
  withImportModules (moduleNames.data.map ({ module := · })) {} 0 fun env => do 
    -- let labelsPath  := labelsPath ++ s!"_{i}"
    -- let featuresPath := featuresPath ++ s!"_{i}"
    let m := extractPremisesFromImportsToFiles user labelsPath featuresPath
    let ctx : Core.Context := { 
      fileName := "", 
      fileMap := default, 
      maxHeartbeats := 100000000000000, 
      maxRecDepth := 10000000000000 }
    let _ ← m.toIO ctx { env := env }
  
-- lake build && lean -j 4 --memory=4096 --timeout=10000000000000000 --run PremiseSelection/ExtractorRunner.lean data/test.labels data/test.features +all
-- stuck at Mathbin.Algebra.Category.Group.EpiMono.
-- print constants again
-- chunks dont necessarily help
-- doesnt seemt o be a time limit for the # error. aslo not dependent on file sizes.
-- make suire we're not reading proof source
-- why empty theorem features?
