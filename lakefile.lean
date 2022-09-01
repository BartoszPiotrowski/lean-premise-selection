import Lake
open Lake DSL

package leanPremiseSelection

require mathlib3port from git "https://github.com/leanprover-community/mathlib3port.git"@"cce383df712a48f0c773a10e8c3295164fbb9f57"

@[defaultTarget]
lean_lib PremiseSelection 

-- Separate test library. Useful to import mathbin separately.
@[defaultTarget]
lean_lib Tests

@[defaultTarget]
lean_exe TrainAndPredict where 
  root := `Scripts.TrainAndPredict

@[defaultTarget]
lean_exe LoadAndPredict where 
  root := `Scripts.LoadAndPredict

@[defaultTarget]
lean_exe KnnPredict where
  root := `Scripts.KnnPredict
