import Lean
import Mathlib.Tactic.ToAdditive

open Lean System

namespace ToAdditive

/-- I'm guessing this is the inverse of `guessNameDict`? -/
private def reverseGuessNameDict (is_comm : Bool) : List String → List (List String)
| "nonneg" :: s    => (reverseGuessNameDict false s).map (addCommPrefix is_comm "one" :: "le" :: ·)
| "pos" :: s       => (reverseGuessNameDict false s).map (addCommPrefix is_comm "one" :: "lt" :: ·)
| "nonpos" :: s    => (reverseGuessNameDict false s).map (addCommPrefix is_comm "le" :: "one" :: ·) 
| "single" :: s    => (reverseGuessNameDict false s).map ("mul" :: addCommPrefix is_comm "single"    :: ·)
| "support" :: s   => (reverseGuessNameDict false s).map ("mul" :: addCommPrefix is_comm "support"   :: ·)
| "tsupport" :: s  => (reverseGuessNameDict false s).map ("mul" :: addCommPrefix is_comm "tsupport"  :: ·)
| "indicator" :: s => (reverseGuessNameDict false s).map ("mul" :: addCommPrefix is_comm "indicator" :: ·)
| "vadd" :: s      => (reverseGuessNameDict false s).map (addCommPrefix is_comm "smul"        :: ·)
| "neg" :: s       => (reverseGuessNameDict false s).map (addCommPrefix is_comm "inv"         :: ·) ++
                      (reverseGuessNameDict false s).map (addCommPrefix is_comm "lt" :: "one" :: ·)
| "sub" :: s       => (reverseGuessNameDict false s).map (addCommPrefix is_comm "div"         :: ·)
| "zero" :: s      => (reverseGuessNameDict false s).map (addCommPrefix is_comm "one"         :: ·) 
| "sum" :: s       => (reverseGuessNameDict false s).map (addCommPrefix is_comm "prod"        :: ·)
| "finsum" :: s    => (reverseGuessNameDict false s).map (addCommPrefix is_comm "finprod"     :: ·)
| "nsmul" :: s     => (reverseGuessNameDict false s).map (addCommPrefix is_comm "pow"         :: ·) ++ 
                      (reverseGuessNameDict false s).map (addCommPrefix is_comm "npow"        :: ·)
| "zsmul" :: s     => (reverseGuessNameDict false s).map (addCommPrefix is_comm "zpow"        :: ·)
| "add" :: "monoid" :: s    => (reverseGuessNameDict false s).map (addCommPrefix is_comm "monoid"    :: ·)
| "add" :: "submonoid" :: s => (reverseGuessNameDict false s).map (addCommPrefix is_comm "submonoid" :: ·)
| "add" :: "group" :: s     => (reverseGuessNameDict false s).map (addCommPrefix is_comm "group"     :: ·)
| "add" :: "subgroup" :: s  => (reverseGuessNameDict false s).map (addCommPrefix is_comm "subgroup"  :: ·)
| "add" :: "semigroup" :: s => (reverseGuessNameDict false s).map (addCommPrefix is_comm "semigroup" :: ·)
| "add" :: "magma" :: s     => (reverseGuessNameDict false s).map (addCommPrefix is_comm "magma"     :: ·)
| "add" :: "haar" :: s      => (reverseGuessNameDict false s).map (addCommPrefix is_comm "haar"      :: ·)
| "add" :: "prehaar" :: s   => (reverseGuessNameDict false s).map (addCommPrefix is_comm "prehaar"   :: ·)
| "add" :: "unit" :: s      => (reverseGuessNameDict false s).map (addCommPrefix is_comm "unit"      :: ·)
| "add" :: "units" :: s     => (reverseGuessNameDict false s).map (addCommPrefix is_comm "units"     :: ·)
| "add" :: s                => (reverseGuessNameDict false s).map (addCommPrefix is_comm "mul"       :: ·)
| "comm" :: s => reverseGuessNameDict true s
| x :: s      => (reverseGuessNameDict false s).map (addCommPrefix is_comm x :: ·)
| []          => bif is_comm then [["comm"]] else [[]]

/-- Reverse name autogenerated by `to_additive`. -/
def reverseGuessName : String → List String :=
  mapTokensList ''' <| fun s => 
    List.map "_".intercalate (reverseGuessNameDict false (s.splitOn "_"))
where 
  mapTokensList (c : Char) (f : String → List String) : String → List String :=
    let c := String.singleton c
    List.map c.intercalate ∘ List.map f ∘ (·.splitOn c)

end ToAdditive

namespace String 

/-- Check if a string is contained in another one. -/
partial def isSubstrOf (target str : String) : Bool :=
  loop "" 0
where
  loop (acc : String) (pos : String.Pos) : Bool :=
    if pos.byteIdx + target.endPos.byteIdx > str.endPos.byteIdx then
      false
    else if str.substrEq pos target 0 target.endPos.byteIdx then
      true
    else
      loop acc (str.next pos)

end String 

/- Find file path from module imported from Mathbin. -/
def pathFromMathbinImport (mod : Name) : MetaM (Option FilePath) := do 
  let mathbinPath : System.FilePath := "." / "lean_packages" / "mathlib3port"
  SearchPath.findWithExt [mathbinPath] "lean" mod

/- Given a theorem name and a file path, extract the proof text. -/
def proofSource (thm : Name) (modulePath : FilePath) : MetaM (Option String) := do
  let mut res := "" 
  for thmName in thm.toString :: ToAdditive.reverseGuessName thm.toString do 
    let keywords := 
      "(theorem|instance|class|attribute|section|namespace|def|variable)"
    let matchThmBody := 
      "/theorem " ++ thmName ++ "/{flag=1;next}/" ++ keywords ++ "/{flag=0}flag"
    let args := #[matchThmBody, modulePath.toString]
    let output ← IO.Process.output { cmd := "awk", args := args }
    if output.exitCode == 0 then 
      res := output.stdout
    if !res.isEmpty then 
      break 
  
  return (if res.isEmpty then none else some res)

/- Given a list of premises and proof text, get rid of the ones that do not 
appear. We take into account `ToAdditive` name translations. -/
def filterUserPremises (premises : List Name) (proofSource : String) : List Name := 
  let appearsInProof (s : String) : Bool := s.isSubstrOf proofSource
  premises.filter (fun p => 
    (p.toString :: ToAdditive.reverseGuessName p.toString).any appearsInProof)