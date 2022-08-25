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
  let (examples_l, examples_r) := split rule examples
  if examples_l.isEmpty || examples_r.isEmpty
  then return Leaf ((← examples.chooseRandom).label, examples)
  else return Node (rule,
    Leaf (unionOfLabels examples_l, examples_l),
    Leaf (unionOfLabels examples_r, examples_r))

--def initCond (m : Float) (examples : List Example) : Bool :=
--  let labels := labels examples
--  let labels := List.flattenUnordered labels
--  let impur := giniImpur labels
--  impur > m

def initCond (m : Float) (examples : List Example) : Bool :=
  let labels := examples.map (fun x => x.label)
  let union_size := Float.ofNat (union labels).length
  let avg_size := average (labels.map (fun x => Float.ofNat (List.length x)))
  --let n := Float.ofNat labels.length
  (union_size / avg_size) > m

def Tree.add (m : Float) (tree : Tree) (e : Example) : IO Tree := do
  let rec loop t := match t with
    | Node (fea, tree_l, tree_r) =>
      match (ruleOfFea fea) e with
      | Left  => do return Node (fea, ← loop tree_l, tree_r)
      | Right => do return Node (fea, tree_l, ← loop tree_r)
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
    | Node (fea, tree_l, tree_r) =>
      match (ruleOfFea fea) e with
      | Left  => loop tree_l
      | Right => loop tree_r
  loop tree

def Tree.toString (tree : Tree) : String :=
  match tree with
  | Node (rule, tree_l, tree_r) =>
    let a := toString tree_l
    let b := toString tree_r
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
      let (left_tree, rest_l) := fromStringAux rest
      let (right_tree, rest_r) := fromStringAux rest_l
      (Node (h, left_tree, right_tree), rest_r)
  | [] => (Leaf ([], []), [])

def Tree.fromString (string : String) : Tree :=
  let (tree, rest) := fromStringAux (string.splitOn " ")
  assert! rest = []
  tree

def Tree.depth (t : Tree) : Nat :=
  match t with
  | Leaf _ => 0
  | Node (_, t_left, t_right) => 1 + max (Tree.depth t_left) (Tree.depth t_right)

def Tree.n_nodes (t : Tree) : Nat :=
  match t with
  | Leaf _ => 1
  | Node (_, t_left, t_right) => 1 + (Tree.n_nodes t_left) + (Tree.n_nodes t_right)

def Tree.balance (t : Tree) : Float :=
  let d := Float.ofNat t.depth
  let n := Float.ofNat t.n_nodes
  (Float.log2 n) / d

def Tree.sizesOfLeavesE (t : Tree) : List Nat :=
  let s := match t with
  | Leaf (_, es) => [es.length]
  | Node (_, t_left, t_right) =>
    (Tree.sizesOfLeavesE t_left) ++ (Tree.sizesOfLeavesE t_right)
  s.sort (fun x y => x > y)

def Tree.sizesOfLeavesL (t : Tree) : List Nat :=
  let s := match t with
  | Leaf (l, _) => [l.length]
  | Node (_, t_left, t_right) =>
    (Tree.sizesOfLeavesL t_left) ++ (Tree.sizesOfLeavesL t_right)
  s.sort (fun x y => x > y)

def Tree.sizesOfLeavesLdivE (t : Tree) : List Float :=
  let s := match t with
  | Leaf (l, es) => [(Float.ofNat l.length) / (Float.ofNat es.length)]
  | Node (_, t_left, t_right) =>
    (Tree.sizesOfLeavesLdivE t_left) ++ (Tree.sizesOfLeavesLdivE t_right)
  s.sort (fun x y => x > y)

def Tree.sizesOfLeavesL_E (t : Tree) :=
  let s := match t with
  | Leaf (l, es) => [(l.length, es.length)]
  | Node (_, t_left, t_right) =>
    (Tree.sizesOfLeavesL_E t_left) ++ (Tree.sizesOfLeavesL_E t_right)
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
