import Lake
open Lake DSL

package leanPremiseSelection

--require mathlib from git "https://github.com/leanprover-community/mathlib4.git"

@[defaultTarget]
lean_lib PremiseSelection

@[defaultTarget]
lean_exe TrainAndPredict

@[defaultTarget]
lean_exe LoadAndPredict
