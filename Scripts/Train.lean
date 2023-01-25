import PremiseSelection

open PremiseSelection

def main (args : List String) : IO Unit := do
  let train_features       := args.get! 0
  let train_labels         := args.get! 1
  let forest_save_path     := args.get! 2
  -- hyperparameters
  let n_trees              := args.get! 3
  let passes               := args.get! 4 -- sane default: 3
  let part                 := args.get! 5 -- sane default: 0.2
  let initThreshold        := args.get! 6 -- sane default: 2.0
  let optimLevel           := args.get! 7 -- sane default: 1.0
  let n_trees := n_trees.toInt!.toNat
  let passes := passes.toInt!.toNat
  let part := floatOfString part
  let initThreshold := floatOfString initThreshold
  let optimLevel := floatOfString optimLevel
  IO.println s!"Loading data..."
  let train_data ← loadLabeled train_features train_labels
  IO.println s!"Training random forest..."
  let f ← forest passes n_trees part initThreshold optimLevel train_data
  IO.println "\nStats about the forest:"
  IO.println (stats f)
  IO.println s!"Saving forest..."
  saveToFile f forest_save_path
  IO.println s!"Forest saved at {forest_save_path}"
