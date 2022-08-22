import LeanRandomForest.Utils
import LeanRandomForest.Impurity
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
  let readLine l := l.splitOn " "
  return List.map readLine lines

def saveLabels (labels : List Label) (path : String) : IO Unit:=
  let labelsAsStrings := List.map (fun x => String.joinWith x " ") labels
  let labelsAsString := String.joinWith labelsAsStrings "\n"
  IO.FS.writeFile path (labelsAsString ++ "\n")

-- TODO more general instead of Strings?
def loadLabels (path : String) : IO (List (List String)) := do
  let lines ← readLines path
  return (lines.map String.splitOn)

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

--def uniformLabels (e : List Example) : Bool :=
--  let labels := labels e
--  let rec uniform inds :=
--    match inds with
--    | []            => True
--    | [_]           => True
--    | h1 :: h2 :: t =>
--      if h1 = h2 then uniform (h2 :: t) else False
--  uniform labels

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
      | Left => loop examples_l (h :: examples_r) t
      | Right  => loop (h :: examples_l) examples_r t
  loop [] [] examples

@[inline]
def add (es : List Example) (e : Example) :=
    e :: es

def splitImpur (impur : List String → Float) fea examples :=
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
    ((impur (union left)) * fl + (impur (union right)) * fr)

def giniRule (examples : List Example) : (IO String) := do
  let n := examples.length
  let m := Int.toNat (Float.toInt (Float.sqrt (Float.ofNat n)))
  let random_feas ← randomFeatures examples m
  let impur_from_fea f := splitImpur giniImpur f examples
  let impurs := List.map impur_from_fea random_feas
  let impurs_feas := List.zip impurs random_feas
  let compare := fun (x1, _) (y1, _) => x1 < y1
  let impurs_feas := List.sort compare impurs_feas
  let (_, best_fea) := List.head! impurs_feas
  return best_fea

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
