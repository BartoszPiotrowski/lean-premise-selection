import PremiseSelection

open PremiseSelection

def main (args : List String) : IO Unit := do
  let labelsPath   := args.get! 0
  let featuresPath := args.get! 1
  
  extractPremisesFromImportsToFiles (recursive := true) (user := true) labelsPath featuresPath

  IO.println s!"Labels saved at {labelsPath}."
  IO.println s!"Features saved at {featuresPath}."
