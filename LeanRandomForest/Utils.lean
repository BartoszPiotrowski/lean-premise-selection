import Init.Data.Random
import Std.Data.HashSet
import Std.Data.HashMap

section List

variable {α} [Inhabited α]

def minList [LE α] [DecidableRel (@LE.le α _)] : List α → α
  | []         => panic! "Empty list"
  | l@(h :: _) => l.foldl min h

def maxList [LT α] [DecidableRel (@LT.lt α _)] : List α → α
  | []         => panic! "Empty list"
  | l@(h :: _) => l.foldl max h

def average (l : List Float) :=
  let sum := l.foldl (fun acc x => acc + x) 0
  sum / (Float.ofNat l.length)

def accuracy [DecidableEq α] (l₁ l₂ : List α) : Float :=
  assert! l₁.length = l₂.length
  let pairs := List.zip l₁ l₂
  let correct := pairs.filter (fun (x, y) => x = y)
  (Float.ofNat correct.length) / (Float.ofNat pairs.length)

def accuracy' [DecidableEq α] (l₁ : List α) (l₂ : List (List α)) : Float :=
  let pairs := List.zip l₁ l₂
  let correct := pairs.filter (fun (x, y) => y.contains x)
  (Float.ofNat correct.length) / (Float.ofNat pairs.length)

def arraySubset (x : List α) (inds : List Nat) : Array α :=
  Array.mk $ inds.map (fun i => x.get! i)

def sampleWithReplace (l : List α) (n : Nat) : IO (List α) :=
  let a := Array.mk l
  let rec loop i r :=
    match i with
    | 0 => return r
    | k + 1 => do loop k $ a.get! (← IO.rand 0 (a.size - 1)) :: r
  loop n []


def evalList {α} (l : List (IO α)) : IO (List α) := do
  let mut ll := []
  for x in l do
    ll := (← x) :: ll
  return ll.reverse

namespace List

def insert (compare : α → α → Bool) (x : α) (l : List α) : (List α) :=
  match l with
  | [] => [x]
  | h :: t => if compare x h then x :: h :: t else h :: insert compare x t

def insert_sort (compare : α → α → Bool) (l : List α) : (List α) :=
  let rec loop : List α → List α → List α
    | acc, [] => acc
    | acc, h :: t => loop (insert compare h acc) t
  loop [] l

def split {α} (l : List α) :=
  let rec split_aux l left right :=
    match l with
    | [] => (left, right)
    | h :: t => split_aux t right (h :: left)
  split_aux l [] []

partial def merge {α} (compare : α → α → Bool) (l1 l2 : List α) :=
  match (l1, l2) with
  | ([], l) => l
  | (l, []) => l
  | (h1 :: t1, h2 :: t2) =>
    if compare h1 h2 then h1 :: merge compare t1 l2
    else                  h2 :: merge compare t2 l1

partial def merge_sort {α} (compare : α → α → Bool) (l : List α) :=
  match l with
  | [] => l
  | [_] => l
  | _ =>
    let (l1, l2) := split l
    merge compare (merge_sort compare l1) (merge_sort compare l2)

abbrev sort {α} : (α → α → Bool) → (List α) → (List α) := merge_sort

def sample (l : List α) (n : Nat) : IO (List α) :=
  if l.length < n then panic! "List shorter than n" else do
    let mut a := Array.mk l
    for i in List.range n do
      let j ← IO.rand 0 (a.size - i - 1)
      let e := a.get! (i + j)
      a := a.set! (i + j) (a.get! i)
      a := a.set! i e
    return (a.extract 0 n).data

def chooseRandom (l : List α) : IO α := do
  return l.get! (← IO.rand 0 (l.length - 1))

def initSeg {α} (l : List α) n :=
    match l with
    | [] => panic! "init_seg"
    | h :: t => if n = 1 then [h] else h :: initSeg t (n-1)

def initSegAndTail {α} (l : List α) (n : Nat) :=
    let rec aux acc n l := match l with
        | []     => (acc.reverse, [])
        | h :: t => if n = 0 then (acc.reverse, h :: t) else aux (h :: acc) (n-1) t
    aux [] n l

def append_unordered (l₁ : List α) (l₂ : List α) :=
  match l₂ with
  | [] => l₁
  | h :: t => h :: (append_unordered l₁ t)

-- TODO is it optimal?
def flatten_unordered (l : List (List β)) : List β :=
  let rec aux acc rest :=
    match rest with
    | [] => acc
    | h :: t => aux (append_unordered acc h) t
  aux [] l

end List

def shuffle {α} (l : List α) : IO (List α) := do
    let r ← (evalList (List.map (fun _ => IO.rand 0 10000) l))
    let rl := List.zip r l
    let sl := List.sort (fun (a, _) (b, _) => a < b) rl
    return List.map (fun x => x.snd) sl

def randomSplit {α : Type} (l : List α) (n : Nat) : IO (List α × List α) := do
    let sl ← shuffle l
    return sl.initSegAndTail n

end List

def readLines (path : String) : IO (List String) := do
  let handle ← IO.FS.Handle.mk path IO.FS.Mode.read
  let content ← handle.readToEnd
  return content.trim.splitOn "\n"

def time (f : α → β) (x : α) : IO β := do
  timeit "Execution time: " (return f x)

def Float.toInt (f : Float) : Int :=
  if f < 0
  then - (- f).toUInt64.val
  else f.toUInt64.val

def floatOfString (s : String) : Float :=
  let (s, sign) := if s.get 0 = '-'
    then ((s.toSubstring.drop 1).toString, -1)
    else (s, 1)
  let a := Array.mk (s.splitOn ".")
  let (S, s) := (a[0]!,a[1]!)
  let length_s := Float.ofInt s.length
  let S := Float.ofInt S.toInt!
  let s := Float.ofInt s.toInt!
  (Float.ofInt sign) * (S + (s / 10 ^ length_s))

def remove_last (l : List α) : (List α) :=
  match l with
  | [] => []
  | [h] => []
  | h :: t => h :: (remove_last t)

open Std

variable {β : Type} [BEq β] [Hashable β]

def HashSet.ofList (l : List β) :=
  List.foldl HashSet.insert HashSet.empty l

def HashSet.insertMany (s : HashSet β) (l : List β) :=
  List.foldl HashSet.insert s l

def union (l : List (List β)) : List β :=
  (l.foldl HashSet.insertMany HashSet.empty).toList

def multiUnion (l : List (List β)) : List β :=
  match l with
  | [] => []
  | hl :: tl => hl ++ (multiUnion tl) --TODO make linear version

def dedup (l : List β) : List β :=
  let set := HashSet.ofList l
  set.toList

def HashSet.diff (a : HashSet β) (b : HashSet β) : HashSet β :=
  HashSet.fold HashSet.erase a b

def freqs (l : List β) :=
  let update (tbl : HashMap β Int) (i : β) :=
    if tbl.contains i then tbl.insert i (tbl.find! i + 1)
    else tbl.insert i 1
  let tbl := List.foldl (fun tbl i => update tbl i) HashMap.empty l
  tbl.toList

def unionFreqs (l : List (List β)) :=
  let update (tbl : HashMap β Int) (i : β) :=
    if tbl.contains i then tbl.insert i (tbl.find! i + 1)
    else tbl.insert i 1
  let updateMany (tbl : HashMap β Int) (l : List β) :=
    l.foldl update tbl
  (l.foldl updateMany HashMap.empty).toList

def String.joinWith (l : List String) (c : String) : String :=
  match l with
  | h :: t => h ++ t.foldl (fun r s => r ++ c ++ s) ""
  | [] => ""

def List.mapParallel {α β} (f : α → β) (l : List α) :=
  let spawn := l.map (fun e => (Task.spawn fun _ => f e))
  spawn.map Task.get
