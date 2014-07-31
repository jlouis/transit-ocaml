open OUnit
open Core.Std

let read_json t =
    let fname = String.concat ["../transit-format/examples/0.8/simple/"; t; ".json"] in
    let d = In_channel.read_all fname in
      Transit.from_string d
      
let exemplar t expect =
    let real = read_json t in
      assert_equal ~printer:Transit.to_string
      	expect real

let tests = "Transit" >::: [
    "dummy" >::
      (fun () -> assert_equal true true);
    "nil" >::
      (fun () -> exemplar "nil" `Null);
    "true" >::
      (fun () -> exemplar "true" (`Bool true));
    "false" >::
      (fun () -> exemplar "false" (`Bool false));
    "zero" >::
      (fun () -> exemplar "zero" (`Int (Int64.of_int 0)));
    "one" >::
      (fun () -> exemplar "one" (`Int (Int64.of_int 1)));
    "one_string" >::
      (fun () -> exemplar "one_string" (`String "hello"));
    "one_keyword" >::
      (fun () -> exemplar "one_keyword" (`Keyword "hello"));
    "one_symbol" >::
      (fun () -> exemplar "one_symbol" (`Symbol "hello"));
  ]
