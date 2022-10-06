# Premise selection for Lean

The aim of this project (which is for now a work in progress) is to provide a
tool for premise selection for Lean users.

The premise selection mechanism is based on machine learning -- a custom
version of the random forest algorithm.

The machine learning model is trained on data extracted from `mathlib`. Each
training data point is a pair `(features T, premises T)`, where `T` is a theorem,
`features T` are features of a statement of `T` extracted by our featurizer, and
`premises T` is a list of names of premises used in a proof of `T`.

The user can ask for advice from the trained model by using the provided tactic
`suggest_premises`. It returns a ranking of premises likely useful for proving a
theorem at hand. The user may also extract their own training data and train
their own machine learning model for premise selection.

# Building the widget

In order to see the interactive list of premises you need to
run the following:

```
cd widget
npm i
npm run build -- --tsxName index
```

Then head over to `TacticTest.lean` and hover on `suggest_premises`.