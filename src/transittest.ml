open OUnit
open Core.Std

let read_json t =
    let fname = String.concat ["../transit-format/examples/0.8/simple/"; t; ".json"] in
    let d = In_channel.read_all fname in
      Transit.from_string d
      
let exemplar t expect =
    let real = read_json t in
      assert_equal expect real

let tests = "Transit" >::: [
    "dummy" >::
      (fun () -> assert_equal true true);
    "nil" >::
      (fun () -> exemplar "nil" (`Null));
  ]
