import Lean
import PremiseSelection.ProofSource

open Lean Meta Elab System


partial def getNameFromCommand : Syntax → Option String
  | Syntax.missing => none
  | Syntax.atom _ _ => none
  | Syntax.ident _ _ _ _ => none
  | Syntax.node _ ``Lean.Parser.Command.declId args =>
      match args[0]! with
      | Syntax.ident _ _ val _ => some (toString val)
      | _ => none
  | Syntax.node _ _ args => args.foldl (init := none) fun acc? arg =>
      match acc? with
      | some s => some s
      | none => getNameFromCommand arg

def getNameAndSourceFromCommand (command : Syntax) :
  MetaM (Option (String × String)) := do
  match getNameFromCommand command with
  | some name =>
      try
        let source := toString <| ← PrettyPrinter.ppCommand ⟨command⟩
        return some (name, source)
      catch _ =>
        return none
  | _ => return none

def getNamesAndSourcesFromFile (path : FilePath) (content : String) :
  MetaM (Array (String × String)) := do
  let mut m := #[]

  let opts := Options.empty

  let inputCtx := Parser.mkInputContext content path.toString
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  let (env, messages) ← processHeader header opts messages inputCtx

  if messages.hasErrors then
    return m

  let env := env.setMainModule (← moduleNameOfFileName path none)
  let commandState := Command.mkState env messages opts
  let s ← IO.processCommands inputCtx parserState commandState
  let commands := s.commands.pop

  for command in commands do
    let nameAndSource? ← getNameAndSourceFromCommand command
    match nameAndSource? with
    | some (name, source) => m := m.push (name, source)
    | _ => continue

  return m

def makeProofSource : MetaM Unit := do
  let env ← getEnv
  let moduleNamesArray := env.header.moduleNames

  for moduleName in moduleNamesArray do
    if let some path ← pathFromMathlibImport moduleName then
      IO.println s!"Making proof sources for {moduleName} ({path})"

      -- Get names and sources from file.
      let content ← IO.FS.readFile path
      let proofSources ← getNamesAndSourcesFromFile path content
      let proofSourcesJson := Json.mkObj <| Array.data <|
        proofSources.map fun (name, source) => (name, Json.str source)

      -- Make proof source file.
      let psPathPre := path.withExtension "json"
      let psPathComponents :=
        [".", "data", "proof_sources"] ++ psPathPre.components.drop 2
      let psPath := mkFilePath psPathComponents
      if ← psPath.pathExists then
        IO.FS.removeFile psPath
      if let some parent := psPath.parent then
        IO.FS.createDirAll parent

      -- Write proof source file.
      IO.FS.writeFile psPath <| toString <| Json.pretty <| proofSourcesJson

unsafe def main : IO Unit := do
  withImportModules #[`Mathlib] {} 0 fun env => do
    let m := makeProofSource
    let ctx : Core.Context := {
      fileName      := "",
      fileMap       := default,
      maxHeartbeats := 10000000000,
      maxRecDepth   := 10000000000 }
    let _ ← m.toIO ctx { env := env }
