import PremiseSelection.Tree

def Forest.add (nTrees : Nat) (part initThreshold optimLevel : Float)
    (forest : List Tree) (e : Example) : IO (List Tree) := do
  let addTree := forest.length < nTrees
  let add tree := Tree.add initThreshold optimLevel tree e
  let k := (part * Float.ofInt forest.length).toInt.toNat
  let k := min forest.length (max 1 k)
  let (treesToUpdate, treesRest) ← randomSplit forest k
  let updatedTrees ← evalList (treesToUpdate.map add)
  let forest := updatedTrees.append treesRest
  if addTree then return leaf e :: forest else return forest

def forest (passes nTrees : Nat) (part initThreshold optimLevel : Float)
    (examples : List Example) : IO (List Tree) := do
  let add := Forest.add nTrees part initThreshold optimLevel
  let mut forest := []
  for _ in List.range passes do
    for e in examples do
      forest ← add forest e
  return forest

def vote (votes : List Label) :=
  let votes := votes.flattenUnordered
  let freqs := (freqs votes).toList
  List.sort (fun (_, x) (_, y) => x > y) freqs

def rankingWithScores (forest : List Tree) (e : Example) :=
  let votes := forest.map (Tree.classify e)
  vote votes

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
  let avgDepth := average (forest.map (fun t => Float.ofNat t.depth))
  let avgNNodes := average (forest.map (fun t => Float.ofNat t.nNodes))
  let avgBalance := average (forest.map Tree.balance)
  let avgAvgNodeE := average (forest.map Tree.avgSizeOfLeavesE)
  let avgMaxNodeE := average (forest.map Tree.maxSizeOfLeavesE)
  let avgMinNodeE := average (forest.map Tree.minSizeOfLeavesE)
  let avgAvgNodeL := average (forest.map Tree.avgSizeOfLeavesL)
  let avgMaxNodeL := average (forest.map Tree.maxSizeOfLeavesL)
  let avgMinNodeL := average (forest.map Tree.minSizeOfLeavesL)
  let avgAvgNodeLdivE := average (forest.map Tree.avgSizeOfLeavesLdivE)
  let avgMaxNodeLdivE := average (forest.map Tree.maxSizeOfLeavesLdivE)
  let avgMinNodeLdivE := average (forest.map Tree.minSizeOfLeavesLdivE)
  s!"Average depth of a tree:       {avgDepth}\n" ++
  s!"Average n. of nodes in a tree: {avgNNodes}\n" ++
  s!"Average balance of a tree:     {avgBalance}\n" ++
  s!"Labels per leaf, min: {avgMinNodeL}, max: {avgMaxNodeL}, avg {avgAvgNodeL}\n" ++
  s!"Examples per leaf, min: {avgMinNodeE}, max: {avgMaxNodeE}, avg {avgAvgNodeE}\n" ++
  s!"(Labels / Examples) per leaf, min: {avgMinNodeLdivE}, max: {avgMaxNodeLdivE}, avg {avgAvgNodeLdivE}\n"
