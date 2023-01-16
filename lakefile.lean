import Lake
open Lake DSL

package leanPremiseSelection

require mathlib3port from git "https://github.com/leanprover-community/mathlib3port.git"@"f4e5dfe2aa778b4cc42620b6b58442504348d20d"

@[default_target]
lean_lib PremiseSelection

-- Separate test library. Useful to import mathbin separately.
@[default_target]
lean_lib Tests

@[default_target]
lean_exe TrainAndPredict where
  root := `Scripts.TrainAndPredict

@[default_target]
lean_exe LoadAndPredict where
  root := `Scripts.LoadAndPredict

@[default_target]
lean_exe KnnPredict where
  root := `Scripts.KnnPredict

def npmCmd : String := "npm"

target packageLock : FilePath := do
  let widgetDir := __dir__ / "widget"
  let packageFile ← inputFile <| widgetDir / "package.json"
  let packageLockFile := widgetDir / "package-lock.json"
  buildFileAfterDep packageLockFile packageFile fun _srcFile => do
    proc {
      cmd := npmCmd
      args := #["install"]
      cwd := some widgetDir
    }

def tsxTarget (pkg : Package) (tsxName : String) [Fact (pkg.name = _package.name)]
    : IndexBuildM (BuildJob FilePath) := do
  let widgetDir := __dir__ / "widget"
  let jsFile := widgetDir / "dist" / s!"{tsxName}.js"
  let deps : Array (BuildJob FilePath) := #[
    ← inputFile <| widgetDir / "src" / s!"{tsxName}.tsx",
    ← inputFile <| widgetDir / "rollup.config.js",
    ← inputFile <| widgetDir / "tsconfig.json",
    ← fetch (pkg.target ``packageLock)
  ]
  buildFileAfterDepArray jsFile deps fun _srcFile => do
    proc {
      cmd := npmCmd
      args := #["run", "build", "--", "--tsxName", tsxName]
      cwd := some widgetDir
    }

@[default_target]
target widgetJs (pkg : Package) : FilePath := tsxTarget pkg "index"
