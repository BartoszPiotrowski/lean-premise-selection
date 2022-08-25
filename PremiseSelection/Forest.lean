import PremiseSelection.Tree

def Forest.add (m : Float) (n_trees : Nat) (part : Float) (forest : List Tree)
    (e : Example) : IO (List Tree) := do
  let add_tree := forest.length < n_trees
  let add tree := Tree.add m tree e
  let k := (part * Float.ofInt forest.length).toInt.toNat
  let k := min forest.length (max 1 k)
  let (trees_to_update, trees_rest) ← randomSplit forest k
  let updated_trees ← evalList (trees_to_update.map add)
  let forest := updated_trees.append trees_rest
  if add_tree then return leaf e :: forest else return forest

def forest (n_trees : Nat) (passes : Nat) (part : Float) (m : Float)
    (examples : List Example) : IO (List Tree) := do
  let add f e := Forest.add m n_trees part f e
  let mut forest := []
  for _ in List.range passes do
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

-- parallel version of the above
--def rankingWithScores (forest : List Tree) (e : Example) :=
--  let tasks := forest.map (fun t => (Task.spawn fun _ => (Tree.classify e t)))
--  let votes := tasks.map Task.get
--  vote votes

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

def stats (forest : List Tree) : String :=
  let avg_depth := average (forest.map (fun t => Float.ofNat t.depth))
  let avg_n_nodes := average (forest.map (fun t => Float.ofNat t.n_nodes))
  let avg_balance := average (forest.map Tree.balance)
  let avg_avg_node_E := average (forest.map Tree.avgSizeOfLeavesE)
  let avg_max_node_E := average (forest.map Tree.maxSizeOfLeavesE)
  let avg_min_node_E := average (forest.map Tree.minSizeOfLeavesE)
  let avg_avg_node_L := average (forest.map Tree.avgSizeOfLeavesL)
  let avg_max_node_L := average (forest.map Tree.maxSizeOfLeavesL)
  let avg_min_node_L := average (forest.map Tree.minSizeOfLeavesL)
  let avg_avg_node_LdivE := average (forest.map Tree.avgSizeOfLeavesLdivE)
  let avg_max_node_LdivE := average (forest.map Tree.maxSizeOfLeavesLdivE)
  let avg_min_node_LdivE := average (forest.map Tree.minSizeOfLeavesLdivE)
  s!"Average depth of a tree:       {avg_depth}\n" ++
  s!"Average n. of nodes in a tree: {avg_n_nodes}\n" ++
  s!"Average balance of a tree:     {avg_balance}\n" ++
  s!"Labels per leaf,
    min: {avg_min_node_L}, max: {avg_max_node_L}, avg {avg_avg_node_L}\n" ++
  s!"Examples per leaf,
    min: {avg_min_node_E}, max: {avg_max_node_E}, avg {avg_avg_node_E}\n" ++
  s!"(Labels / Examples) per leaf,
    min: {avg_min_node_LdivE}, max: {avg_max_node_LdivE}, avg {avg_avg_node_LdivE}\n"
