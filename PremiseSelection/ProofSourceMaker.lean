import Lean
import PremiseSelection.ProofSource

open Lean Meta Elab System

def getNameFromCommand (command : Syntax) : Option String :=
  getNameFromCommandAux 10 command
where
  getNameFromCommandAux : Nat → Syntax → Option String
    | 0, _ => none
    | _, Syntax.missing => none
    | _, Syntax.atom _ _ => none
    | _, Syntax.ident _ _ _ _ =>  none
    | _, Syntax.node _ ``Lean.Parser.Command.declId args =>
        match args[0]! with
        | Syntax.ident _ _ val _ => some (toString val)
        | _ => none
    | n + 1, Syntax.node _ _ args => args.foldl (init := none) fun acc? arg =>
        match acc? with
        | some s => some s
        | none => getNameFromCommandAux n arg

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

def makeProofSource (moduleName : Name) : MetaM Unit := do
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
      pure () -- IO.FS.removeFile psPath
    if let some parent := psPath.parent then
      IO.FS.createDirAll parent

    -- Write proof source file.
    IO.FS.writeFile psPath <| toString <| Json.pretty <| proofSourcesJson

unsafe def main (args : List String) : IO Unit := do
  if args.length != 1 then
    throw <| IO.userError "Usage: make_proof_sources <module_name>"

  let moduleNameRaw := args.get! 0
  let moduleNameComponents := moduleNameRaw.splitOn "."
  let mut moduleName := Name.anonymous
  for c in moduleNameComponents do
    moduleName := Name.mkStr moduleName c

  withImportModules #[moduleName] {} 0 fun env => do
    let ctx : Core.Context := {
      fileName      := "",
      fileMap       := default,
      maxHeartbeats := 10 ^ 100,
      maxRecDepth   := 10 ^ 100 }
    let _ ← (makeProofSource moduleName).toIO ctx { env := env }
