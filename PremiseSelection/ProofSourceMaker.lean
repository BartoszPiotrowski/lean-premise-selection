import Lean
import PremiseSelection.ProofSource

open Lean Meta Elab System

def splitUsingConstants (path : FilePath) (content : String) (constants : Array ConstantInfo) :
  MetaM (HashMap Name String) := do
  let mut m := HashMap.empty
  -- let thmNames : Array String := constants.filterMap <| fun cinfo =>
  --   match cinfo with
  --   | ConstantInfo.thmInfo val => some (toString val.name.componentsRev.head!)
  --   | _ => none
  -- let mut mutContent := content
  -- let mut thmNamesAndDelims := #[]
  -- for thmName in thmNames do
  --   match mutContent.findSubstr? thmName.toSubstring with
  --   | some substr =>
  --       let idx := substr.stopPos.byteIdx
  --       mutContent := mutContent.drop idx
  --       thmNamesAndDelims := thmNamesAndDelims.push (thmName, idx)
  --   | _ => continue

  -- IO.println thmNamesAndDelims

  let opts := Options.empty

  let inputCtx := Parser.mkInputContext content path.toString
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  let (env, messages) ← processHeader header opts messages inputCtx

  let env := env.setMainModule (← moduleNameOfFileName path none)
  let commandState := Command.mkState env messages opts
  let s ← IO.processCommands inputCtx parserState commandState
  let env' := s.commandState.env
  let commands := s.commands.pop
  let trees := s.commandState.infoState.trees.toArray
  -- Split the file by cmdPos.
  -- detect if command is a theorem (command.theorem ?)
  -- get the name and associated proof.

  IO.println s!"{commands.size} commands"
  IO.println s!"{commands[7]!}"

  return m


def makeProofSource : MetaM Unit := do
  let env ← getEnv
  let moduleNamesArray := env.header.moduleNames
  let moduleDataArray := env.header.moduleData
  for (moduleName, moduleData) in moduleNamesArray.zip moduleDataArray do
    if let some path ← pathFromMathlibImport moduleName then
      let constants := moduleData.constants
      let content ← IO.FS.readFile path
      IO.println s!"Making proof sources for {moduleName} ({path})"
      let _m ← splitUsingConstants path content constants
      break

unsafe def main (args : List String) : IO Unit := do
  -- let outputPath := "data/proof_sources"
  -- let mut moduleNames ← IO.FS.lines "lake-packages/mathlib/Mathlib/Mathlib.lean"

  withImportModules #[`Mathlib] {} 0 fun env => do
    let m := makeProofSource
    let ctx : Core.Context := {
      fileName      := "",
      fileMap       := default,
      maxHeartbeats := 10000000000,
      maxRecDepth   := 10000000000 }
    let _ ← m.toIO ctx { env := env }
