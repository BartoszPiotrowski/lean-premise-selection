import PremiseSelection.Data

abbrev SplitRule := String

inductive Tree : Type
  | Node : SplitRule × Tree × Tree → Tree
  | Leaf : Label × Examples → Tree

instance : Inhabited Tree := { default := Tree.Leaf ([], []) }

open Std
open Tree
open Direction


def leaf (e : Example) :=
  Leaf (e.label, [e])

def makeNewNode (examples : List Example) : IO Tree := do
  --let rule ← randomRule examples
  let rule ← optimizedRule examples
  let (examplesL, examplesR) := split rule examples
  if examplesL.isEmpty || examplesR.isEmpty
  then return Leaf ((← examples.chooseRandom).label, examples)
  else return Node (rule,
    Leaf (unionOfLabels examplesL, examplesL),
    Leaf (unionOfLabels examplesR, examplesR))

--def initCond (m : Float) (examples : List Example) : Bool :=
--  let labels := labels examples
--  let labels := List.flattenUnordered labels
--  let impur := giniImpur labels
--  impur > m

def initCond (m : Float) (examples : List Example) : Bool :=
  let labels := examples.map (fun x => x.label)
  let unionSize := Float.ofNat (union labels).length
  let avgSize := average (labels.map (fun x => Float.ofNat (List.length x)))
  --let n := Float.ofNat labels.length
  (unionSize / avgSize) > m

def Tree.add (m : Float) (tree : Tree) (e : Example) : IO Tree := do
  let rec loop t := match t with
    | Node (fea, treeL, treeR) =>
      match (ruleOfFea fea) e with
      | Left  => do return Node (fea, ← loop treeL, treeR)
      | Right => do return Node (fea, treeL, ← loop treeR)
    | Leaf (label, examples) =>
      let examples := e :: examples
      if initCond m examples
      then makeNewNode examples
      else return Leaf (label, examples)
  loop tree

def Tree.classify (e : Example) tree :=
  let rec loop tree :=
    match tree with
    | Leaf (cls, _) => cls
    | Node (fea, treeL, treeR) =>
      match (ruleOfFea fea) e with
      | Left  => loop treeL
      | Right => loop treeR
  loop tree

def Tree.toString (tree : Tree) : String :=
  match tree with
  | Node (rule, treeL, treeR) =>
    let a := toString treeL
    let b := toString treeR
    s!"{rule}" ++ " " ++ a ++ " " ++ b
  | Leaf (label, _) => s!"☘{String.joinWith label "☘"}"

partial def fromStringAux (l : List String) :=
  let isLeaf (string : String) := string.front = '☘'
  match l with
  | h :: rest =>
    if isLeaf h then
      match h.splitOn "☘" with
      | [] => panic! "error while loading a tree"
      | _ :: t => (Leaf (t, []), rest)
    else
      let (leftTree, restL) := fromStringAux rest
      let (rightTree, restR) := fromStringAux restL
      (Node (h, leftTree, rightTree), restR)
  | [] => (Leaf ([], []), [])

def Tree.fromString (string : String) : Tree :=
  let (tree, rest) := fromStringAux (string.splitOn " ")
  assert! rest = []
  tree

def Tree.depth (t : Tree) : Nat :=
  match t with
  | Leaf _ => 0
  | Node (_, tLeft, tRight) => 1 + max (Tree.depth tLeft) (Tree.depth tRight)

def Tree.nNodes (t : Tree) : Nat :=
  match t with
  | Leaf _ => 1
  | Node (_, tLeft, tRight) => 1 + (Tree.nNodes tLeft) + (Tree.nNodes tRight)

def Tree.balance (t : Tree) : Float :=
  let d := Float.ofNat t.depth
  let n := Float.ofNat t.nNodes
  (Float.log2 n) / d

def Tree.sizesOfLeavesE (t : Tree) : List Nat :=
  let s := match t with
  | Leaf (_, es) => [es.length]
  | Node (_, tLeft, tRight) =>
    (Tree.sizesOfLeavesE tLeft) ++ (Tree.sizesOfLeavesE tRight)
  s.sort (fun x y => x > y)

def Tree.sizesOfLeavesL (t : Tree) : List Nat :=
  let s := match t with
  | Leaf (l, _) => [l.length]
  | Node (_, tLeft, tRight) =>
    (Tree.sizesOfLeavesL tLeft) ++ (Tree.sizesOfLeavesL tRight)
  s.sort (fun x y => x > y)

def Tree.sizesOfLeavesLdivE (t : Tree) : List Float :=
  let s := match t with
  | Leaf (l, es) => [(Float.ofNat l.length) / (Float.ofNat es.length)]
  | Node (_, tLeft, tRight) =>
    (Tree.sizesOfLeavesLdivE tLeft) ++ (Tree.sizesOfLeavesLdivE tRight)
  s.sort (fun x y => x > y)

def Tree.sizesOfLeavesL_E (t : Tree) :=
  let s := match t with
  | Leaf (l, es) => [(l.length, es.length)]
  | Node (_, tLeft, tRight) =>
    (Tree.sizesOfLeavesL_E tLeft) ++ (Tree.sizesOfLeavesL_E tRight)
  s.sort (fun (x, _) (y, _) => x > y)

def Tree.avgSizeOfLeavesE (t : Tree) :=
  average (t.sizesOfLeavesE.map Float.ofNat)

def Tree.maxSizeOfLeavesE (t : Tree) :=
  maxList (t.sizesOfLeavesE.map Float.ofNat)

def Tree.minSizeOfLeavesE (t : Tree) :=
  minList (t.sizesOfLeavesE.map Float.ofNat)

def Tree.avgSizeOfLeavesL (t : Tree) :=
  average (t.sizesOfLeavesL.map Float.ofNat)

def Tree.maxSizeOfLeavesL (t : Tree) :=
  maxList (t.sizesOfLeavesL.map Float.ofNat)

def Tree.minSizeOfLeavesL (t : Tree) :=
  minList (t.sizesOfLeavesL.map Float.ofNat)

def Tree.avgSizeOfLeavesLdivE (t : Tree) :=
  average t.sizesOfLeavesLdivE

def Tree.maxSizeOfLeavesLdivE (t : Tree) :=
  maxList t.sizesOfLeavesLdivE

def Tree.minSizeOfLeavesLdivE (t : Tree) :=
  minList t.sizesOfLeavesLdivE
