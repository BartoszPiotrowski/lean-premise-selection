import Lean

open Lean Elab Tactic Widget

namespace PremiseSelection

@[widget]
def premiseSelectionWidget : UserWidgetDefinition := {
  name := "Premise Selection"
  javascript := include_str ".." / "widget" / "dist" / "index.js"
}

structure Item where
  name : String
  score : Int
  deriving ToJson, FromJson, Inhabited

structure WidgetProps where
  items : Array Item
  deriving ToJson, FromJson

def saveWidget (stx : Syntax) (xs : Array Item) : TacticM Unit := do
  let ps : WidgetProps := {items := xs}
  saveWidgetInfo `PremiseSelection.premiseSelectionWidget (toJson ps) stx
  return ()

end PremiseSelection