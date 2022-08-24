import PremiseSelection.Utils
import PremiseSelection.Impurity
import Std.Data.HashSet

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

def loadFeatures (path : String) : IO (List (List String)) := do
  let lines ← readLines path
  return lines.map String.splitOn

-- TODO more general instead of Strings?
def loadLabels (path : String) : IO (List (List String)) := do
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
  let features ← loadFeatures features
  let labels ← loadLabels labels
  let features_labels := List.zip features labels
  let labeled' f_l :=
    let (f, l) := f_l
    labeled f l
  return List.map labeled' features_labels

def randomFeature (examples : List Example) : IO String := do
    let random_example_1 := (← chooseRandom examples).features
    let random_example_2 := (← chooseRandom examples).features
    let ex_1_minus_ex_2 := HashSet.diff random_example_1 random_example_2
    if HashSet.isEmpty ex_1_minus_ex_2
      then chooseRandom (HashSet.toList random_example_1)
      else chooseRandom (HashSet.toList ex_1_minus_ex_2)

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

def split (fea : String) (examples : List Example) : (List Example × List Example) :=
  let rule := ruleOfFea fea
  let rec loop examples_l examples_r l :=
    match l with
    | [] => (examples_l, examples_r)
    | h :: t =>
      match (rule h) with
      | Left  => loop (h :: examples_l) examples_r t
      | Right => loop examples_l (h :: examples_r) t
  loop [] [] examples

def add (es : List Example) (e : Example) :=
    e :: es

def splitImpurGini fea examples :=
    let rule := ruleOfFea fea
    let append left_right e :=
      let (left, right) := left_right
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
    let left_labels := left.map Example.label
    let right_labels := right.map Example.label
    let (left_union, right_union) := (union left_labels, union right_labels)
    let i := intersection left_union right_union
    i.length + ((right_union.length ^ 2) + (left_union.length ^ 2)) / i.length

def optimizedRule (examples : List Example) : (IO String) := do
  let n := examples.length
  let random_feas ← randomFeatures examples n
  let impur_from_fea f := splitImpurInter f examples
  let impurs := List.map impur_from_fea random_feas
  let impurs_feas := List.zip impurs random_feas
  let compare := fun (x, _) (y, _) => x < y
  let impurs_feas := List.sort compare impurs_feas
  let (_, best_fea) := List.head! impurs_feas
  return best_fea

def randomRule (examples : List Example) : (IO String) := do
  randomFeature examples

variable {α} [BEq α] [Hashable α]

def cover (n : Nat) (ranking : List α) (true : List α) :=
  let n := min ranking.length (true.length + n)
  let ranking_truncated := HashSet.ofList (ranking.initSeg n)
  let covered := true.map ranking_truncated.contains
  let covered_sum := covered.foldl (fun acc x => if x then acc + 1 else acc) 0
  (Float.ofNat covered_sum) / (Float.ofNat true.length)

def avgCover (rankings : List (List α)) (true : List (List α)) :=
  let rankings_true := List.zip rankings true
  let coverages := rankings_true.map (fun (x, y) => (cover 0) x y)
  average coverages

def avgCover10 (rankings : List (List α)) (true : List (List α)) :=
  let rankings_true := List.zip rankings true
  let coverages := rankings_true.map (fun (x, y) => (cover 10) x y)
  average coverages
