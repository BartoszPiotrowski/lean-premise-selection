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

def Std.RBMap.modify' (k : κ) (fn : Option α → Option α) (r : RBMap κ α cmp) :=
  match fn <| r.find? k with
  | none => r.erase k
  | some v => r.insert k v

def Std.RBMap.mergeBy (fn : κ → α → α → α) (r1 r2 : RBMap κ α cmp) :  RBMap κ α cmp :=
  r2.foldl (fun r1 k v2 => r1.modify' k (fun | none => some v2 | some v1 => some (fn k v1 v2))) r1

namespace PremiseSelection

def Multiset (α : Type) [Ord α] := Std.RBMap α Nat compare

variable {α : Type} [Ord α]

def Multiset.empty : Multiset α := mkRBMap _ _ _

instance : EmptyCollection  (Multiset α) :=  ⟨Multiset.empty⟩

instance : Append  (Multiset α) where
  append x y := x.mergeBy (fun _ => (·+·)) y

def Multiset.add : Multiset α → α → Multiset α
  | m, a => m.modify' a (fun | none => some 1 | some v => some (v + 1))

def Multiset.singleton : α → Multiset α
  | a => Multiset.empty |>.add a

instance : Ord Name := ⟨Name.quickCmp⟩

structure Bigram where
  fst : Name
  snd : Name
  deriving Ord

structure Trigram where
  fst : Name
  snd : Name
  trd : Name
  deriving Ord

instance : ToJson Bigram where
  toJson b := s!"{b.fst}/{b.snd}"

instance : ToString Bigram where
  toString b := s!"{b.fst}/{b.snd}"

instance : ToString Trigram where
  toString t := s!"{t.fst}/{t.snd}/{t.trd}"

instance : ToJson Trigram where
  toJson t := s!"{t.fst}/{t.snd}/{t.trd}"

structure StatementFeatures where
  /-- Just the constant's names and how frequently they arise. -/
  nameCounts : Multiset Name := ∅
  bigramCounts : Multiset Bigram := ∅
  trigramCounts : Multiset Trigram := ∅

instance : ForIn M (Multiset α) (α × Nat) :=
  show ForIn _ (Std.RBMap _ _ _) _ by infer_instance

def Multiset.toList (m : Multiset α) : List α :=
  m.foldl (fun l x _ => x :: l) []

def Multiset.toHFeatures [ToString α] (m : Multiset α) : Array String :=
  Array.mk <| m.toList.map (s!"H:{·}")

def Multiset.toTFeatures [ToString α] (m : Multiset α) : Array String :=
  Array.mk <| m.toList.map (s!"T:{·}")

instance [ToJson α] : ToJson (Multiset α) where
  toJson m := Json.arr (Array.mk (m.toList.map toJson))

instance : EmptyCollection StatementFeatures := ⟨{}⟩
instance : Append StatementFeatures where
  append x y := {
    nameCounts := x.nameCounts ++ y.nameCounts
    bigramCounts := x.bigramCounts ++ y.bigramCounts
    trigramCounts := x.trigramCounts ++ y.trigramCounts
  }

instance : ToJson StatementFeatures where
  toJson f := Json.mkObj [
    ("nameCounts", toJson f.nameCounts),
    ("bigramCounts", toJson f.bigramCounts),
    ("trigramCounts", toJson f.trigramCounts)
  ]

def StatementFeatures.mkName : Name → StatementFeatures
  | n => {nameCounts := Multiset.singleton n}

def StatementFeatures.mkBigram : Name → Name → StatementFeatures
  | n1, n2 => {bigramCounts := Multiset.singleton ⟨n1, n2⟩}

def StatementFeatures.mkTrigram : Name → Name → Name → StatementFeatures
  | n1, n2, n3 => {trigramCounts := Multiset.singleton ⟨n1, n2, n3⟩}

def StatementFeatures.toHFeatures (f : StatementFeatures) : Array String := 
  f.nameCounts.toHFeatures ++
  f.bigramCounts.toHFeatures ++
  f.trigramCounts.toHFeatures

def StatementFeatures.toTFeatures (f : StatementFeatures) : Array String :=
  f.nameCounts.toTFeatures ++
  f.bigramCounts.toTFeatures ++
  f.trigramCounts.toTFeatures

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
  --let ppe ← Lean.PrettyPrinter.ppExpr e
  if let some n := immediateName e then
    tell <| StatementFeatures.mkName n
  if e.isApp then
    e.withApp (fun f args => do
      if let some n1 := immediateName f then
        for arg in args do
          if let some n2 := getHeadName? arg then
            tell <| StatementFeatures.mkBigram n1 n2
            if arg.isApp then
              arg.withApp (fun f args => do
                if let some n2 := immediateName f then
                  for arg in args do
                    if let some n3 := getHeadName? arg then
                      tell <| StatementFeatures.mkTrigram n1 n2 n3
              )
        for p in args.toList.allPairs do
          if let some n2 := getHeadName? p.1 then
            if let some n3 := getHeadName? p.2 then
              tell <| StatementFeatures.mkTrigram n1 n2 n3
    )
  return ()

def getStatementFeatures (e : Expr) : MetaM StatementFeatures := do
  let ((), features) ← WriterT.run <| forEachExpr visitFeature e
  return features

open Lean.Meta

def getArgsFeatures (args : List Expr) : MetaM (Array StatementFeatures) := do 
  let mut argsFeats := #[]
  for arg in args do
    let argType ← inferType arg
    if (← inferType argType).isProp then
      let argFeats ← getStatementFeatures argType
      if ! argFeats.nameCounts.isEmpty then
        argsFeats := argsFeats ++ #[argFeats]
  return argsFeats

def getThmAndArgsFeatures (e : Expr) 
  : MetaM (StatementFeatures × Array StatementFeatures) := do
  forallTelescope e <| fun args thm => do
      let thmFeats ← getStatementFeatures thm
      let argsFeats ← getArgsFeatures args.data
      return (thmFeats, argsFeats)

end PremiseSelection
