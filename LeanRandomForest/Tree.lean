import Std.Data.HashMap
import LeanRandomForest.Utils
import LeanRandomForest.Data

abbrev SplitRule := String

--instance : Inhabited LeafLabel := inferInstance
--instance : Inhabited Examples := sorry

inductive Tree : Type
  | Node : SplitRule × Tree × Tree → Tree
  | Leaf : Label × Examples → Tree
--deriving Inhabited

instance : Inhabited Tree := { default := Tree.Leaf ([], []) }

open Std
open Tree
open Direction


def leaf (e : Example) :=
  match e.label with
  | [] => Leaf ([], [e])
  | l => Leaf (l, [e])

def makeNewNode (label : Label) (examples : List Example) : IO Tree := do
  let rule ← giniRule examples
  let (examples_l, examples_r) := split rule examples
  if examples_l.isEmpty || examples_r.isEmpty
  then return Leaf (label, examples)
  else return Node (rule,
    --Leaf ([], examples_l), -- hide premises from the leftmost (the most "negative") leaf
    --Leaf (unionOfLabels examples_l, examples_l),
    --Leaf (unionOfLabels examples_r, examples_r))
    Leaf ((← examples_l.chooseRandom).label, examples_l),
    Leaf ((← examples_r.chooseRandom).label, examples_r))

def initCond (min_impur : Float) (examples : List Example) : Bool :=
  let labels := labels examples
  let labels := List.flatten_unordered labels
  let impur := giniImpur labels
  impur > min_impur

--def initCond' (_ : Float) (examples : List Example) : Bool :=
--  let labels := labels examples
--  let union_size := Float.ofNat (union labels).length
--  let avg_size := average (labels.map (fun x => Float.ofNat (List.length x)))
--  --let n := Float.ofNat labels.length
--  (union_size / avg_size) > 5

def Tree.add (min_impur : Float) (tree : Tree) (e : Example) : IO Tree := do
  let rec loop t := match t with
    | Node (fea, tree_l, tree_r) =>
      (match (ruleOfFea fea) e with
      | Left  => do return Node (fea, ← loop tree_l, tree_r)
      | Right => do return Node (fea, tree_l, ← loop tree_r))
    | Leaf (label, examples) =>
      let examples := e :: examples
      if initCond min_impur examples
      then makeNewNode label examples
      else return Leaf (label, examples)
  loop tree

def Tree.classify (e : Example) tree :=
  let rec loop tree :=
    match tree with
    | Leaf (cls, _) => cls
    | Node (fea, tree_l, tree_r) =>
      match (ruleOfFea fea) e with
      --| Left  => dbg_trace "left"; loop tree_l
      --| Right => dbg_trace "right"; loop tree_r
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
  --decreasing_by { sorry }

def Tree.fromString (string : String) : Tree :=
  let (tree, rest) := fromStringAux (string.splitOn " ")
  assert! rest = []
  tree

def Tree.depth (t : Tree) : Nat :=
  match t with
  | Leaf _ => 0
  | Node (_, t_left, t_right) => 1 + max (Tree.depth t_left) (Tree.depth t_right)

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