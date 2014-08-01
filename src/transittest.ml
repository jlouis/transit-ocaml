open OUnit2
open Core.Std

let read_json t =
    let fname = String.concat ["../transit-format/examples/0.8/simple/"; t; ".json"] in
    let d = In_channel.read_all fname in
      Transit.from_string d
      
let from_timestamp f = Time.of_float f

let exemplar t expect =
    let real = read_json t in
      assert_equal ~printer:Transit.to_string
      	expect real

let array_simple = `Array [`Int (Int64.of_int 1);
                           `Int (Int64.of_int 2);
                           `Int (Int64.of_int 3)]

let i x = `Int (Int64.of_int x)
let s x = `String x

let array_mixed = `Array [
	i 0; i 1; `Float 2.0; `Bool true; `Bool false; `String "five"; `Keyword "six"; `Symbol "seven"; `String "~eight"; `Null]

let array_nested = `Array [array_simple; array_mixed]

let small_strings = `Array [s ""; s "a"; s "ab"; s "abc"; s "abcd"; s "abcde"; s "abcdef"]

let t n expect = n >:: (fun(_) -> exemplar n expect)

let tests = "Transit" >::: [
    "dummy" >::
      (fun (_) -> assert_equal true true);
    t "nil" `Null;
    t "true" (`Bool true);
    t "false" (`Bool false);
    t "zero" (`Int (Int64.of_int 0));
    t "one" (`Int (Int64.of_int 1));
    t "one_string" (`String "hello");
    t "one_keyword" (`Keyword "hello");
    t "one_symbol" (`Symbol "hello");
    t "one_date" (`Date (from_timestamp 946728000.0));
    t "vector_simple" array_simple;
    t "vector_empty" (`Array []);
    t "vector_mixed" array_mixed;
    t "vector_nested" array_nested;
    t "small_strings" small_strings;
  ]
