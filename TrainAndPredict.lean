import LeanRandomForest

def main (args : List String) : IO Unit := do
  let train_features := args.get!        0
  let train_labels := args.get!          1
  let test_features := args.get!         2
  let test_labels := args.get!           3
  let test_preds_save_path := args.get!  4
  IO.println s!"Loading data..."
  let train_data ← loadLabeled train_features train_labels
  let test_data ← loadLabeled test_features test_labels
  IO.println s!"Training random forest..."
  let my_forest ← forest train_data
  let avg_depth := average (my_forest.map (fun t => Float.ofNat t.depth))
  IO.println s!"Average depth of a tree: {avg_depth}"
  IO.println s!"Classifying test data..."
  let pred_test_labels := List.mapParallel (ranking my_forest) test_data
  IO.println s!"Saving predictions..."
  saveLabels pred_test_labels test_preds_save_path
  IO.println s!"Predictions saved at {test_preds_save_path}"
