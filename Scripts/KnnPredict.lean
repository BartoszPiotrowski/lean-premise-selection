import PremiseSelection.Knn

open PremiseSelection

def main (args : List String) : IO Unit := do
  let train_features := args.get!        0
  let train_labels := args.get!          1
  let test_features := args.get!         2
  let test_labels := args.get!           3
  let test_preds_save_path := args.get!  4
  let n_neighbours := args.get!          5
  let n_neighbours := n_neighbours.toInt!.toNat
  IO.println s!"Loading data..."
  let train_data ← loadLabeled train_features train_labels
  let test_data ← loadLabeled test_features test_labels
  IO.println s!"Classifying test data..."
  let pred_test_labels := predict train_data test_data n_neighbours
  IO.println s!"Saving predictions..."
  saveLabels pred_test_labels test_preds_save_path
  IO.println s!"Predictions saved at {test_preds_save_path}"
