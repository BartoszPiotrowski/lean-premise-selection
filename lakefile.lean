import Lake
open Lake DSL

package leanPremiseSelection

-- require mathlib3port from git "https://github.com/leanprover-community/mathlib3port.git"@"65ed2fb26d6ba05daf18072566b2a2c72f5cd370"
require mathlib3port from git "https://github.com/leanprover-community/mathlib3port.git"@"f4e5dfe2aa778b4cc42620b6b58442504348d20d"

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
