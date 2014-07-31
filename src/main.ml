open OUnit
open Core.Std

let () =
    ignore (OUnit.run_test_tt ~verbose:true Transittest.tests)
