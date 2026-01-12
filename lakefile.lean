import Lake
open Lake DSL

package leanPremiseSelection

@[default_target]
lean_lib PremiseSelection

@[default_target]
lean_lib Tests

@[default_target]
lean_exe Train where
  root := `Scripts.Train

@[default_target]
lean_exe Predict where
  root := `Scripts.Predict

@[default_target]
lean_exe KnnPredict where
  root := `Scripts.KnnPredict

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"
