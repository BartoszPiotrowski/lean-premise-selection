import Lean
import Mathlib.Control.Writer
import PremiseSelection.Utils
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

namespace PremiseSelection

def Multiset (α : Type) [Ord α] := RBMap α Nat compare

variable {α : Type} [Ord α]

def Multiset.empty : Multiset α := mkRBMap _ _ _

instance : EmptyCollection  (Multiset α) :=  ⟨Multiset.empty⟩

instance : Append  (Multiset α) where
  append x y := x.mergeBy (fun _ => (·+·)) y

def Multiset.add : Multiset α → α → Multiset α
  | m, a => m.modify a (fun | none => some 1 | some v => some (v + 1))

def Multiset.singleton : α → Multiset α
  | a => Multiset.empty |>.add a

instance : Ord Name := ⟨Name.quickCmp⟩

structure Bigram where
  fst : Name
  snd : Name
  deriving Ord

structure StatementFeatures where
  /-- Just the constant's names and how frequently they arise. -/
  nameCounts : Multiset Name := ∅
  bigramCounts : Multiset Bigram := ∅
  subexpressions : Multiset String := ∅

instance : ForIn M (Multiset α) (α × Nat) :=
  show ForIn _ (RBMap _ _ _) _ by infer_instance

instance : ToJson StatementFeatures where 
  toJson features := Id.run <| do
    let mut jsonFeatures : Array Json := #[]
    for (⟨n1, n2⟩, _) in features.bigramCounts do
      jsonFeatures := jsonFeatures.push s!"{n1}/{n2}"
    return Json.arr jsonFeatures

instance : EmptyCollection StatementFeatures := ⟨{}⟩
instance : Append StatementFeatures where
  append x y := {
    nameCounts := x.nameCounts ++ y.nameCounts
    bigramCounts := x.bigramCounts ++ y.bigramCounts
    subexpressions := x.subexpressions ++ y.subexpressions
  }

def StatementFeatures.mkName : Name → StatementFeatures
  | n => {nameCounts := Multiset.singleton n}

def StatementFeatures.mkBigram : Name → Name → StatementFeatures
  | n1, n2 => {bigramCounts := Multiset.singleton ⟨n1, n2⟩}

def StatementFeatures.mkSubexpr : String → StatementFeatures
  | x => {subexpressions := Multiset.singleton x}

def immediateName (e : Expr) : Option Name :=
  if let .const n _ := e then
    some n
  else if let some n := e.natLit? then
    some <| toString n
  else
    none

def getHeadName? (e : Expr) : Option Name := do
  immediateName <| e.getAppFn

def visitFeature (e : Expr) : WriterT StatementFeatures MetaM Unit  := do
  let ppe ←  Lean.PrettyPrinter.ppExpr e
  tell <| StatementFeatures.mkSubexpr <| toString ppe
  if let some n := immediateName e then
    tell <| StatementFeatures.mkName n
  if e.isApp then
    e.withApp (fun f args => do
      if let some n1 := immediateName f then
        for arg in args do
          if let some n2 := getHeadName? arg then
            tell <| StatementFeatures.mkBigram n1 n2
    )
  return ()

def getStatementFeatures (e : Expr) : MetaM StatementFeatures := do
  let ((), features) ← WriterT.run <| forEachExpr visitFeature e
  return features

end PremiseSelection
