import Mathbin

import Mathbin.Algebra.Group.Basic

import PremiseSelection.Extractor 

extract_premises_from_thm comp_assoc_right

extract_premises_from_thm ite_mul_one

theorem test (a b c : Nat) (h : a < 4) : a + 0 = a := by rfl

extract_premises_from_thm test
