import PremiseSelection

def main (args : List String) : IO Unit := do
  let train_features := args.get!        0
  let train_labels := args.get!          1
  let test_features := args.get!         2
  let test_labels := args.get!           3
  let test_preds_save_path := args.get!  4
  let forest_save_path := args.get!      5
  let n_trees := args.get!               6
  let n_trees := n_trees.toInt!.toNat
  -- hyperparameters
  let passes := 1
  let part := 0.2
  let m := 2.0
  IO.println s!"Loading data..."
  let train_data ← loadLabeled train_features train_labels
  let test_data ← loadLabeled test_features test_labels
  IO.println s!"Training random forest..."
  let f ← forest n_trees passes part m train_data
  IO.println "Stats about the forest:"
  IO.println (stats f)
  IO.println s!"Saving forest..."
  saveToFile f forest_save_path
  IO.println s!"Forest saved at {forest_save_path}"
  IO.println s!"Classifying test data..."
  let pred_test_labels := test_data.mapParallel (ranking f)
  IO.println s!"Saving predictions..."
  saveLabels pred_test_labels test_preds_save_path
  IO.println s!"Predictions saved at {test_preds_save_path}"
