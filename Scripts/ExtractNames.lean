import Lean
open Lean Meta System

open IO IO.FS


def extractNamesFromImportsToFiles (namesPath : FilePath) : MetaM Unit := do
  IO.FS.writeFile namesPath ""
  let env ← getEnv
  let imports := env.imports.map (·.module)
  let moduleNamesArray := env.header.moduleNames
  let moduleDataArray := env.header.moduleData
  for (moduleName, moduleData) in Array.zip moduleNamesArray moduleDataArray do
    let isMathImport :=
      moduleName.getRoot == `Mathbin || moduleName.getRoot == `Mathlib
    if imports.contains moduleName && isMathImport then
      let namesHandle ← Handle.mk namesPath Mode.append false
      let insert : String → IO Unit := fun name => do
        namesHandle.putStrLn name
      for cinfo in moduleData.constants do
        insert cinfo.toConstantVal.name.toString
  pure ()

unsafe def main (args : List String) : IO Unit := do
  let selectedModules := args.get! 0
  let namesPath       := args.get! 1

  let mut moduleNames := #[]
  for moduleNameStr in ← IO.FS.lines selectedModules do
    let moduleNameStr := moduleNameStr.trim
    if moduleNameStr.startsWith "Math" then
      let decopmosedNameStr := (moduleNameStr.splitOn ".").map Name.mkSimple
      let moduleName := decopmosedNameStr.foldl Name.append Lean.Name.anonymous
      moduleNames := moduleNames.push moduleName

  withImportModules (moduleNames.data.map ({ module := · })) {} 0 fun env => do
    let m := extractNamesFromImportsToFiles namesPath
    let ctx : Core.Context := {
      fileName      := "",
      fileMap       := default,
      maxHeartbeats := 10000000000,
      maxRecDepth   := 10000000000 }
    let _ ← m.toIO ctx { env := env }
