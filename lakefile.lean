import Lake
open Lake DSL

package leanRandomForest

--require mathlib from git "https://github.com/leanprover-community/mathlib4.git"

@[defaultTarget]
lean_lib LeanRandomForest

@[defaultTarget]
lean_exe TrainAndPredict

@[defaultTarget]
lean_exe LoadAndPredict
