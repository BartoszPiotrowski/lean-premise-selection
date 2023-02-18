import PremiseSelection.Extractor

open Lean Lean.Meta PremiseSelection

/-- Run this file to extract premises as follows:

  `lean --run PremiseSelection/ExtractorRunner.lean
    data/test.labels data/test.features data/modules 
    [min-depth=x] [max-depth=y] [+all] [+source] [+math] [+n] [+b] [+t]`

The first argument is the path to the labels file, the second argument is the
path to the features file, the third argument is the path to the modules file (a
file consisting of a list of module names, one per line). Adding the options
`+all`, `+source` or `+math` will apply the respecive filters. Options `+n`, 
`+b`, and `+t` will change the format of the features.

This is quite a heavy task so you might need to increase memory and time out,
e.g. by adding

  `--memory=4096 --timeout=10000000000000000`

after `lean`.

Mathbin errors can usually be fixed by making sure that `LEAN_PATH` is correct:

  export LEAN_PATH=./build/lib
  export LEAN_PATH=$LEAN_PATH:./lake-packages/mathlib3port/build/lib
  export LEAN_PATH=$LEAN_PATH:./lake-packages/mathlib/build/lib
  export LEAN_PATH=$LEAN_PATH:./lake-packages/lean3port/build/lib
  export LEAN_PATH=$LEAN_PATH:./lake-packages/std/build/lib
  export LEAN_PATH=$LEAN_PATH:./lake-packages/Qq/build/lib
  export LEAN_PATH=$LEAN_PATH:./lake-packages/Aesop/build/lib
-/
unsafe def main (args : List String) : IO Unit := do
  let labelsPath      := args.get! 0
  let featuresPath    := args.get! 1
  let selectedModules := args.get! 2

  -- Change the min and max depth allowed for proofs (max recommended ~ 100).
  let mut minDepth : UInt32 := 0
  let mut maxDepth : UInt32 := 255
  for pos in [3, 4] do
    for key in ["min", "max"] do
      let fullKey := key ++ "-depth="
      if pos + 1 <= args.length && (args.get! pos).startsWith fullKey then
        let depthStr := (args.get! pos).drop fullKey.length
        let n := depthStr.toNat!
        if h : n < UInt32.size then
          if key == "min" then minDepth := ⟨n, h⟩ else maxDepth := ⟨n, h⟩

  -- Add `+all` to the command to apply the no-aux filter.
  let noAux := (args.drop 3).contains "+all"

  -- Add `+source` to the command to apply the source filter.
  let source := (args.drop 3).contains "+source"
  
  -- Add `+math` to the command to apply the math filter.
  let math := (args.drop 3).contains "+math"
    
  if source && math then
    panic "Cannot use both user and math filters."

  -- Flags for features:
  -- * `+n` = nameCounts.
  -- * `+b` = biagramCounts.
  -- * `+t` = trigramCounts.
  let n := (args.drop 3).contains "+n"
  let b := (args.drop 3).contains "+b"
  let t := (args.drop 3).contains "+t"
  if !n && !b && !t then
    panic "No features selected. Add `+n`, `+b` or `+t` to the command."

  let format := FeatureFormat.mk n b t

  let options : UserOptions := ⟨minDepth, maxDepth, noAux, source, math, format⟩

  let mut moduleNames := #[]
  for moduleNameStr in ← IO.FS.lines selectedModules do
    let moduleNameStr := moduleNameStr.trim
    if moduleNameStr.startsWith "Math" then
      let decopmosedNameStr := (moduleNameStr.splitOn ".").map Name.mkSimple
      let moduleName := decopmosedNameStr.foldl Name.append Lean.Name.anonymous
      moduleNames := moduleNames.push moduleName

  withImportModules (moduleNames.data.map ({ module := · })) {} 0 fun env => do
    let m := extractPremisesFromImportsToFiles labelsPath featuresPath options
    let ctx : Core.Context := {
      fileName      := "",
      fileMap       := default,
      maxHeartbeats := 10000000000,
      maxRecDepth   := 10000000000 }
    let _ ← m.toIO ctx { env := env }
