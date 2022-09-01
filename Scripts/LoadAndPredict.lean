import PremiseSelection

open PremiseSelection

def main (args : List String) : IO Unit := do
  let forest := args.get!                0
  let test_features := args.get!         1
  let test_labels := args.get!           2
  let test_preds_save_path := args.get!  3
  IO.println s!"Loading data..."
  let my_forest ← loadFromFile forest
  IO.println s!"Loading forest..."
  let test_data ← loadLabeled test_features test_labels
  let avg_depth := average (my_forest.map (fun t => Float.ofNat t.depth))
  IO.println s!"Average depth of a tree: {avg_depth}"
  IO.println s!"Classifying test data..."
  let pred_test_labels := List.mapParallel (ranking my_forest) test_data
  IO.println s!"Saving predictions..."
  saveLabels pred_test_labels test_preds_save_path
  IO.println s!"Predictions saved at {test_preds_save_path}"
