import Lake
open Lake DSL

package leanPremiseSelection

require mathlib3port from git "https://github.com/leanprover-community/mathlib3port.git"@"1749724b7917aeea55830be63bdc0aee6587451c"

@[defaultTarget]
lean_lib PremiseSelection

@[defaultTarget]
lean_exe TrainAndPredict

@[defaultTarget]
lean_exe LoadAndPredict

@[defaultTarget]
lean_exe KnnPredict
