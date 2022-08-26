import PremiseSelection.Utils
import PremiseSelection.Data
import Std.Data.HashMap

open Std

def similarity (fCounts : HashMap String Int) (nTheorems : Nat) (f1 f2 : Features) : Float :=
  let fI := HashSet.intersection f1 f2
  let trans l n := (Float.log (Float.ofInt l / Float.ofInt n)) ^ 2
  let count f := if fCounts.contains f then fCounts.find! f else 1
  let count_trans f := trans nTheorems (count f)
  let sum l := List.foldl (fun acc x => acc + x) 0 l
  let f1 := f1.toList.map count_trans
  let f2 := f2.toList.map count_trans
  let fI := fI.toList.map count_trans
  let (s1, s2, sI) := (sum f1, sum f2, sum fI)
  sI / (s1 + s2 - sI)

def predictOne (data : List Example) (nNeighbours : Nat) (f : Features) : List String :=
  let allFeatures := (data.map (fun e => e.features.toList)).flattenUnordered
  let fCounts := freqs allFeatures
  let sim := similarity fCounts data.length f
  let simils := data.map (fun e => (sim e.features, e.label))
  let simils := simils.sort (fun (x, _) (y, _) => x > y)
  let simils := simils.initSeg nNeighbours
  let add s (tbl : HashMap String Float) p :=
    if tbl.contains p then tbl.insert p (tbl.find! p + s) else tbl.insert p s
  let addMany tbl s_ps := let (s, ps) := s_ps; ps.foldl (add s) tbl
  let premisesScores := simils.foldl addMany HashMap.empty
  let ranking := premisesScores.toList.sort (fun (_, x) (_, y) => x > y)
  ranking.map (fun (x, _) => x)

def predict (trainData testData : List Example) (nNeighbours : Nat) :=
  let testData := testData.map Example.features
  let predictOne := predictOne trainData nNeighbours
  testData.mapParallel predictOne
