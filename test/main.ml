open OUnit2
open Core.Std

let () =
  ignore (OUnit2.run_test_tt_main Suite.tests)
