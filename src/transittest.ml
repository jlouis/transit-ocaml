open OUnit

let tests = "Transit" >::: [
    "dummy" >::
      (fun () -> assert_equal true true);
  ]
