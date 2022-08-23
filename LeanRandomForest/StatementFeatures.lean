import Lean
import LeanRandomForest.Writer
import LeanRandomForest.Utils
open Lean
/-!

# Theorem feature extraction

Input: the goal state
Ouput: the theorem statement as an expr

 -/
open Std

def Std.RBMap.modify (k : κ) (fn : Option α → Option α) (r : RBMap κ α cmp) :=
  match fn <| r.find? k with
  | none => r.erase k
  | some v => r.insert k v

def Std.RBMap.mergeBy (fn : κ → α → α → α) (r1 r2 : RBMap κ α cmp) :  RBMap κ α cmp :=
  r2.fold (fun r1 k v2 => r1.modify k (fun | none => some v2 | some v1 => some (fn k v1 v2))) r1

def StatementFeatures := NameMap Nat
def StatementFeatures.write : Name → StatementFeatures
  | n => mkNameMap _ |>.insert n 1
instance : EmptyCollection StatementFeatures := ⟨mkNameMap _⟩
instance : Append StatementFeatures where
  append x y := x.mergeBy (fun _ => (·+·)) y
instance : ForIn M StatementFeatures (Name × Nat) :=
  show ForIn _ (NameMap _) _ by infer_instance

def visitFeature (e : Expr) : WriterT StatementFeatures MetaM Unit  :=
  match e with
  | Expr.const n _ => tell <| StatementFeatures.write n
  | _ => pure ()

def getStatementFeatures (e : Expr) : MetaM StatementFeatures := do
  let ((), features) ← WriterT.run <| forEachExpr visitFeature e
  return features