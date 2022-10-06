import Lean

open Lean Elab Tactic Widget Server

def Array.chooseM [Alternative m] [Monad m] (f : α → m β) (xs : Array α) : m (Array β) := do
  let mut acc := #[]
  for x in xs do
    if let some y ← optional <| f x then
      acc := acc.push y
  return acc


namespace PremiseSelection

@[widget]
def premiseSelectionWidget : UserWidgetDefinition := {
  name := "Premise Selection"
  javascript := include_str ".." / "widget" / "dist" / "index.js"
}

structure Item where
  name : Name
  score : Int
  deriving ToJson, FromJson, Inhabited

structure WidgetProps where
  items : Array Item
  deriving ToJson, FromJson, Inhabited


structure ItemData extends Item where
  expr? : Option CodeWithInfos
  deriving RpcEncodable, Inhabited

structure GetItemArgs where
  items : Array Item
  pos : Lean.Lsp.Position
  deriving ToJson, FromJson, Inhabited

instance : Alternative RequestM where
  failure _ := throw <| RequestError.internalError "failure"
  orElse a b c := OrElse.orElse (a c) (fun _ => b () c)

open Lean Server RequestM in
@[serverRpcMethod]
def getItems (args : GetItemArgs) : RequestM (RequestTask (Array ItemData)) := do
  let doc ← readDoc
  let pos := doc.meta.text.lspPosToUtf8Pos args.pos
  withWaitFindSnapAtPos args.pos fun snap => do
    let g :: _ := snap.infoTree.goalsAt? doc.meta.text pos
      | throw <| RequestError.internalError "no goals"
    let { ctxInfo := ci, tacticInfo := ti, useAfter := useAfter, .. } := g
    let ci := if useAfter then { ci with mctx := ti.mctxAfter } else { ci with mctx := ti.mctxBefore }
    let g :: _ := if useAfter then ti.goalsAfter else ti.goalsBefore
      | throw <| RequestError.internalError "no goals"
    ci.runMetaM {} <| do
      let some mvarDecl := (← getMCtx).findDecl? g
        | throwError "unknown goal {g.name}"
      let lctx := mvarDecl.lctx
      let lctx := lctx.sanitizeNames.run' { options := (← getOptions) }
      Meta.withLCtx lctx mvarDecl.localInstances do
        args.items.mapM (fun item => do
          let expr? ← optional do
            let e ← Lean.Meta.mkAppM item.name #[]
            let p ← ppExprTagged e
            return p
          return {item with expr?}
        )


def saveWidget (stx : Syntax) (xs : Array Item) : TacticM Unit := do
  let ps : WidgetProps := {items := xs}
  saveWidgetInfo `PremiseSelection.premiseSelectionWidget (toJson ps) stx
  return ()

end PremiseSelection