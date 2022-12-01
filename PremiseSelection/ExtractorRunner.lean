import Mathbin
import PremiseSelection.Extractor 

attribute [-instance] coeDecidableEq

open Lean Lean.Meta PremiseSelection

/-- Run this file to extract premises as follows: 
  
  `lean --run PremiseSelection/ExtractorRunner.lean data/test.labels data/test.features data/modules [max-depth=n] [+user] [+n] [+b] [+s]`

The first argument is the path to the labels file, the second argument is the 
path to the features file, the third argument is the path to the modules file (a
file consisting of a list of module names, one per line). Adding the option 
`+user` will try to filter user premises. Options `+n`, `+b`, and `+s` will 
change the format of the features.

This is quite a heavy task so you might need to increase memory and time out, 
e.g. by adding 
  
  `--memory=4096 --timeout=10000000000000000`

after `lean`. 

Mathbin errors can usually be fixed by making sure that `LEAN_PATH` is correct:

  `export LEAN_PATH=build/lib:lean_packages/mathlib3port/build/lib:lean_packages/mathlib/build/lib:lean_packages/lean3port/build/lib`   
-/
unsafe def main (args : List String) : IO Unit := do
  let labelsPath      := args.get! 0
  let featuresPath    := args.get! 1
  let selectedModules := args.get! 2

  -- Change the max depth allowed for proofs (recommended ~ 100).
  let mut maxDepth : UInt32 := 255 
  if @LE.le Nat instLENat 4 args.length && (args.get! 3).startsWith "max-depth=" then
    let maxDepthStr := (args.get! 3).drop 10
    maxDepth := maxDepthStr.toNat!

  -- Add `+user` to the command to apply the user filter.
  let user := (args.drop 3).contains "+user"

  -- Flags for features: 
  -- * `+n` = nameCounts.
  -- * `+b` = biagramCounts.
  -- * `+s` = subexpressions.
  let n := (args.drop 3).contains "+n"
  let b := (args.drop 3).contains "+b"
  let s := (args.drop 3).contains "+s"
  let format := FeatureFormat.mk n b s

  let options : UserOptions := ⟨maxDepth, user, format⟩

  let mut moduleNames := #[]
  for moduleNameStr in ← IO.FS.lines selectedModules do 
    let moduleNameStr := moduleNameStr.trim
    if moduleNameStr.startsWith "Mathbin" then
      let decopmosedNameStr := (moduleNameStr.splitOn ".").map Name.mkSimple
      let moduleName := decopmosedNameStr.foldl Name.append Lean.Name.anonymous
      moduleNames := moduleNames.push moduleName

  withImportModules (moduleNames.data.map ({ module := · })) {} 0 fun env => do 
    let m := extractPremisesFromImportsToFiles labelsPath featuresPath options
    let ctx : Core.Context := { 
      fileName      := "", 
      fileMap       := default, 
      maxHeartbeats := 100000000000000, 
      maxRecDepth   := 100000000000000 }
    let _ ← m.toIO ctx { env := env }
