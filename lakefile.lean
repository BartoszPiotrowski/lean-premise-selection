import Lake
open Lake DSL

package leanPremiseSelection

require mathlib3port from git "https://github.com/leanprover-community/mathlib3port.git"@"cce383df712a48f0c773a10e8c3295164fbb9f57"

@[defaultTarget]
lean_lib PremiseSelection

@[defaultTarget]
lean_exe TrainAndPredict

@[defaultTarget]
lean_exe LoadAndPredict

@[defaultTarget]
lean_exe KnnPredict
