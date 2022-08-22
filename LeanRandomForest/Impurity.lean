import Std.Data.HashMap
import LeanRandomForest.Utils

open Std

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
