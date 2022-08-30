import PremiseSelection.Utils

open Std List

def Label := List String
def Features := HashSet String
structure Example := (features : Features) (label : Label)
instance : Inhabited Example := { default := ⟨HashSet.empty, []⟩ }
def Examples := List Example

inductive Direction : Type
| Left : Direction
| Right : Direction

open Direction

def load (path : String) : IO (List (List String)) := do
  let lines ← readLines path
  return lines.map String.splitOn

def saveLabels (labels : List Label) (path : String) : IO Unit:=
  let labelsAsStrings := List.map (fun x => String.joinWith x " ") labels
  let labelsAsString := String.joinWith labelsAsStrings "\n"
  IO.FS.writeFile path (labelsAsString ++ "\n")

def labels examples :=
    List.map Example.label examples

def unlabeled (features : List String) : Example :=
    ⟨(HashSet.ofList features), []⟩

def labeled (features : List String) (label : List String) : Example :=
    ⟨(HashSet.ofList features), label⟩

def loadLabeled (features : String) (labels : String) : IO (List Example) := do
  let features ← load features
  let labels ← load labels
  dbg_trace "hello 1"
  let featuresLabels := List.zip features labels
  dbg_trace "hello 2"
  let labeled := fun (f, l) => labeled f l
  dbg_trace "hello 3"
  return List.map labeled featuresLabels

def randomFeature (examples : List Example) : IO String := do
    let randomExample_1 := (← chooseRandom examples).features
    let randomExample_2 := (← chooseRandom examples).features
    let ex1MinusEx2 := HashSet.diff randomExample_1 randomExample_2
    if HashSet.isEmpty ex1MinusEx2
      then chooseRandom (HashSet.toList randomExample_1)
      else chooseRandom (HashSet.toList ex1MinusEx2)

def randomFeatures examples n :=
    let rec loop acc m :=
      match m with
      | 0 => acc
      | n + 1 => loop ((randomFeature examples) :: acc) n
    let l := loop [] n
    evalList l

def randomLabel (es : List Example) : IO Label := do
    let e ← chooseRandom es
    match e.label with
    | [] => panic! "unlabeled example"
    | l => return l

def unionOfLabels (examples : Examples) : Label :=
  union (examples.map Example.label)

def ruleOfFea (f : String) (e : Example) :=
  match e.features.contains f with
  | true => Left
  | false => Right

variable {α} [BEq α] [Hashable α]

def giniImpur (l : List α) : Float :=
  let len := l.length
  let update (tbl : HashMap α Int) i :=
    if tbl.contains i then tbl.insert i (tbl.find! i + 1)
    else tbl.insert i 1
  let tbl := List.foldl (fun tbl i => update tbl i) HashMap.empty l
  let update :=
    fun s _ x => s + Float.pow ((Float.ofInt x) / (Float.ofNat len)) 2
  1 - HashMap.fold update 0 tbl

def split (fea : String) (examples : List Example) : (List Example × List Example) :=
  let rule := ruleOfFea fea
  let rec loop examplesL examplesR l :=
    match l with
    | [] => (examplesL, examplesR)
    | h :: t =>
      match (rule h) with
      | Left  => loop (h :: examplesL) examplesR t
      | Right => loop examplesL (h :: examplesR) t
  loop [] [] examples

def splitImpurGini fea examples :=
    let rule := ruleOfFea fea
    let append leftRight e :=
      let (left, right) := leftRight
      match (rule e) with
      | Left => (e.label :: left, right)
      | Right => (left, e.label :: right)
    let (left, right) := List.foldl append ([], []) examples
    let el := Float.ofInt (List.length left)
    let er := Float.ofInt (List.length right)
    let e := Float.ofInt (List.length examples)
    let fl := Float.sqrt (el / e)
    let fr := Float.sqrt (er / e)
    ((giniImpur (List.flattenUnordered left)) * fl +
    (giniImpur (List.flattenUnordered right)) * fr)

def splitImpurInter fea examples :=
    let (left, right) := split fea examples
    let leftLabels := left.map Example.label
    let rightLabels := right.map Example.label
    let (leftUnion, rightUnion) := (union leftLabels, union rightLabels)
    let i := intersection leftUnion rightUnion
    i.length + ((rightUnion.length ^ 2) + (leftUnion.length ^ 2)) / i.length

def optimizedRule (optimLevel : Float) (examples : List Example) : (IO String) := do
  let n := (max 1 (optimLevel * Float.ofNat examples.length)).toInt.toNat
  let randomFeas ← randomFeatures examples n
  let impurFromFea f := splitImpurInter f examples
  let impurs := List.map impurFromFea randomFeas
  let impursFeas := List.zip impurs randomFeas
  let compare := fun (x, _) (y, _) => x < y
  let impursFeas := List.sort compare impursFeas
  let (_, bestFea) := List.head! impursFeas
  return bestFea

def randomRule (examples : List Example) : (IO String) := do
  randomFeature examples

def cover (n : Nat) (ranking : List α) (true : List α) :=
  let n := min ranking.length (true.length + n)
  let rankingTruncated := HashSet.ofList (ranking.initSeg n)
  let covered := true.map rankingTruncated.contains
  let coveredSum := covered.foldl (fun acc x => if x then acc + 1 else acc) 0
  (Float.ofNat coveredSum) / (Float.ofNat true.length)

def avgCover (rankings : List (List α)) (true : List (List α)) :=
  let rankingsTrue := List.zip rankings true
  let coverages := rankingsTrue.map (fun (x, y) => (cover 0) x y)
  average coverages

def avgCover10 (rankings : List (List α)) (true : List (List α)) :=
  let rankingsTrue := List.zip rankings true
  let coverages := rankingsTrue.map (fun (x, y) => (cover 10) x y)
  average coverages
