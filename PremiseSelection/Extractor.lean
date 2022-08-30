import Lean
import Mathlib.Control.Writer
import PremiseSelection.StatementFeatures 

namespace PremiseSelection

open Lean Lean.Elab Lean.Elab.Term Lean.Elab.Command Lean.Meta

-- TODO: Move to StatementFeatures with correct representation.
instance : ToString StatementFeatures where 
  toString features := Id.run <| do
    let mut output : String := ""
    for (⟨n1, n2⟩, _) in features.bigramCounts do
      output := output.append s!"{n1}/{n2}\n"
    return output

structure PremisesData where 
  theoremName        : Name 
  theoremFeatures    : StatementFeatures
  argumentsFeatuters : List StatementFeatures
  premises           : List Name 

instance : ToJson PremisesData where 
  toJson data := Id.run <| do
    -- NOTE: Ignoring count for now.
    let nameJson : Json := toString data.theoremName
    let thmFeatJson : Json := toString data.theoremFeatures
    let argsFeatJson := 
      Json.arr $ (Array.mk data.argumentsFeatuters).map (Json.str ∘ toString)
    let premisesJson : Json := 
      Json.arr $ (Array.mk data.premises.eraseDup).map (Json.str ∘ toString)
    Json.mkObj [
      ("theoremName", nameJson),
      ("theoremFeatures", thmFeatJson),
      ("argumentsFeatuters", argsFeatJson),
      ("premises", premisesJson)
    ]

instance : ToString PremisesData where 
  toString tp := Json.pretty (toJson tp)

end PremiseSelection
