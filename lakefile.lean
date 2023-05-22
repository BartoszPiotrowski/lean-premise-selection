import Lake
open Lake DSL

package leanPremiseSelection

require mathlib3port from git "https://github.com/leanprover-community/mathlib3port.git"@"7a29664099a8edda5cee335f763b5598626b26c8"

@[default_target]
lean_lib PremiseSelection

-- Separate test library. Useful to import mathbin separately.
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

