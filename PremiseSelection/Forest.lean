import PremiseSelection.Utils
import PremiseSelection.Data
import PremiseSelection.Tree
import Std.Data.HashMap

def Forest.add (min_impur : Float) (n_trees : Nat) (part : Float) (forest : List Tree) (e : Example)
    : IO (List Tree) := do
  let add_tree := forest.length < n_trees
  let k := Int.toNat (Float.toInt (part * Float.ofInt forest.length))
  let k := min forest.length (max 1 k)
  let (trees_to_update, trees_rest) ← randomSplit forest k
  let updated_trees ← evalList
    (trees_to_update.map (fun tree => Tree.add min_impur tree e))
  let forest := updated_trees.append trees_rest
  if add_tree then return leaf e :: forest else return forest

def forest (n_trees : Nat) (examples : List Example) : IO (List Tree) := do
  let add f e := Forest.add 0.5 n_trees 0.2 f e
  let iters := 1
  let mut forest := []
  for _ in List.range iters do
    for e in examples do
      forest ← add forest e
  return forest

def vote (votes : List Label) :=
  let votes := votes.flattenUnordered
  let freqs := freqs votes
  List.sort (fun (_, x) (_, y) => x > y) freqs

def rankingWithScores (forest : List Tree) (e : Example) :=
  let votes := forest.map (Tree.classify e)
  vote votes

--def rankingWithScores (forest : List Tree) (e : Example) :=
--  let tasks := forest.map (fun t => (Task.spawn fun _ => (Tree.classify e t)))
--  let votes := tasks.map Task.get
--  vote votes

def classify forest e :=
  let scores := rankingWithScores forest e
  scores.head!.fst

def ranking forest e :=
  let scores := rankingWithScores forest e
  List.map Prod.fst scores

def saveToFile (forest : List Tree) (path : String) : IO Unit:=
  let listOfTreesAsStrings := List.map Tree.toString forest
  let forestAsString := String.joinWith listOfTreesAsStrings "\n"
  IO.FS.writeFile path (forestAsString ++ "\n")

def loadFromFile (path : String): IO (List Tree) := do
  let lines ← readLines path
  return lines.map Tree.fromString
