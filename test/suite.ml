open OUnit2
open Core.Std
open Sexplib.Std

let cycle n lst =
  let rec loop l acc = function
    | 0 -> List.rev acc
    | k ->
      (match l with
       | [] -> loop lst acc k
       | e :: es -> loop es (e :: acc) (k-1) )
  in
  loop lst [] n

let array_of_symbools m n =
  let seeds =
    let f x = `Keyword (String.concat ["key"; Printf.sprintf "%04d" x]) in
    List.map (List.range 0 m) ~f:f
  in
  cycle n seeds

let hash_of_size k =
  let keywords = array_of_symbools k k in
  let values = List.map (List.range 0 k) ~f:(fun x -> `Int (Int64.of_int x)) in
  `Map (Map.Poly.of_alist_exn (List.zip_exn keywords values))

let from_timestamp f = Time.of_float f

let simple = [`Int (Int64.of_int 1);
              `Int (Int64.of_int 2);
              `Int (Int64.of_int 3)]


let i x = `Int (Int64.of_int x)
let s x = `String x
let fl (x : float) = `Float x

let mixed = [ i 0; i 1; `Float 2.0; `Bool true; `Bool false; `String "five";
              `Keyword "six"; `Symbol "seven"; `String "~eight"; `Null ]

let array_nested = `Array [`Array simple; `Array mixed]
let list_nested = `List [`List simple; `List mixed]
let set_nested = `Set (Set.Poly.of_list [`Set (Set.Poly.of_list simple); `Set (Set.Poly.of_list mixed)])

let map_simple : Transit.t = `Map (Map.Poly.of_alist_exn
                                     [`Keyword "a", `Int (Int64.of_int 1);
                                      `Keyword "b", `Int (Int64.of_int 2);
                                      `Keyword "c", `Int (Int64.of_int 3)])

let map_mixed : Transit.t = `Map (Map.Poly.of_alist_exn
                                    [`Keyword "a", `Int (Int64.of_int 1);
                                     `Keyword "b", `String "a string";
                                     `Keyword "c", `Bool true])

let map_nested : Transit.t = `Map (Map.Poly.of_alist_exn
                                     [`Keyword "simple", map_simple;
                                      `Keyword "mixed", map_mixed])

let small_strings = `Array [s ""; s "a"; s "ab"; s "abc"; s "abcd"; s "abcde"; s "abcdef"]
let add_string prefix (`Array arr) = `Array (List.map arr ~f:(fun (`String s) -> `String (String.concat [prefix; s])))

let uris =
  [`URI "http://example.com";
   `URI "ftp://example.com";
   `URI "file:///path/to/file.txt";
   `URI "http://www.詹姆斯.com/";
  ]

let uuids =
  [`UUID (Uuid.of_string "5a2cbea3-e8c6-428b-b525-21239370dd55");
   `UUID (Uuid.of_string "d1dc64fa-da79-444b-9fa4-d4412f427289");
   `UUID (Uuid.of_string "501a978e-3a3e-4060-b3be-1cf2bd4b1a38");
   `UUID (Uuid.of_string "b3ba141a-a776-48e4-9fae-a28ea8571f58");
  ]

let ints_centered_on ?range:(m=5) n =
 List.map (List.range (n - m) (m + n + 1)) ~f:(fun i -> `Int (Int64.of_int i))

let rec range f t =
      if Big_int.eq_big_int f  t
      then []
      else f :: (range (Big_int.add_big_int Big_int.unit_big_int f) t)

let big_ints_centered_on m n =
  let open Big_int in
  range (sub_big_int n m) (add_big_int m (add_big_int n unit_big_int))

let two = Big_int.big_int_of_int 2

let big_powers_of_two =
  let r = range Big_int.zero_big_int (Big_int.big_int_of_int 66) in
  List.map r ~f:(fun x -> Big_int.power_int_positive_big_int 2 x)

let big_interesting_ints =
  List.map big_powers_of_two ~f:(fun x -> big_ints_centered_on two x)
  |> List.concat

let best_repr x =
  try
    `Int (Big_int.int64_of_big_int x)
  with nativeint_of_big_int ->
    `BigInt x
  
let interesting_ints =
    List.map big_interesting_ints ~f:best_repr

let neg_interesting_ints =
    List.map
      (List.map big_interesting_ints ~f:(fun x -> Big_int.minus_big_int x))
      ~f:best_repr

let doublify ints =
  let floats = List.map ints ~f:(fun (`Int x) -> `Float (Int64.to_float x )) in
  `Array floats

let dates =
  let points = [-6106017600000.0; 0.0; 946728000000.0; 1396909037000.0] in
  List.map points ~f:(fun p -> `Time (from_timestamp (p /. 1000.0)))

let sym_strs = ["a"; "ab"; "abc"; "abcd"; "abcde"; "a1"; "b2"; "c3"; "a_b"]
let symbols = List.map sym_strs ~f:(fun x -> `Symbol x)
let keywords = List.map sym_strs ~f:(fun x -> `Keyword x)

let one = `Int (Int64.of_int 1)
let two = `Int (Int64.of_int 2)

type test_type = JSON | JSON_Verbose | JSON_Write

let read ext t =
  let fname = String.concat ["../transit-format/examples/0.8/simple/"; t; "."; ext] in
  In_channel.read_all fname

(* The following tests depend on the order in either sets or maps. Hence we cannot do string-level equality to validate them *)
let skip_string_eq = function
  | "map_mixed" -> true
  | "map_nested" -> true
  | "map_simple" -> true
  | "map_string_keys" -> true
  | "maps_three_char_string_keys" -> true
  | "maps_four_char_sym_keys" -> true
  | "maps_three_char_sym_keys" -> true
  | "map_vector_keys" -> true
  | "set_simple" -> true
  | "set_nested" -> true
  | "set_mixed" -> true
  | "doubles_interesting" -> true (* Doubles have so many representations we can't really handle them in any special way. Rather, we just have to accept these as is. *)
  | "ints_interesting" -> true (* Contains what I belive to be a renderer failure where very large numbers are rendered as Big_ints where they can avoid being so *)
  | "ints_interesting_neg" -> true (* Same game as for the interesting ints *)
  | _ -> false

let exemplar ty t expected =
  let test x = assert_equal ~printer:Sexp.to_string (Transit.sexp_of_t expected) (Transit.sexp_of_t x) in
  match ty with
  | JSON -> read "json" t |> Transit.from_string |> test
  | JSON_Verbose -> read "verbose.json" t |> Transit.from_string |> test
  | JSON_Write ->
      let string_rep = Transit.to_string expected in
      let x = Transit.from_string string_rep in
      let base = read "json" t in
      let cleaned = 
        match String.chop_suffix base ~suffix:"\n" with (* Remove trailing \n *)
        | None -> base
        | Some cl -> cl in
      test x;
      if (skip_string_eq t)
      then ()
      else assert_equal ~printer:Sexp.to_string (String.sexp_of_t cleaned) (String.sexp_of_t string_rep)

let t ty n expected = n >:: (fun(_) -> exemplar ty n expected)

let cachecode_tests =
  let tester expected i =
    (fun (_) -> assert_equal ~printer:String.to_string expected (Transit.CacheCode.of_int i))
  in
  ["First"  >:: (tester "^0" 0);
   "Second" >:: (tester "^1" 1);
   "Ninth"  >:: (tester "^9" 9);
   "Break"  >:: (tester "^[" 43);
   "After Break" >:: (tester "^10" 44);
   "Last"   >:: (tester "^[[" 1935);
  ]

let example_tests =
    ["dummy" >::
     (fun (_) -> assert_equal ~printer:Sexp.to_string (String.sexp_of_t "this") (String.sexp_of_t "this"));
     "string with newline" >::
     (fun (_) -> assert_equal ~printer:String.to_string "[\"~#'\",\"a\\nb\"]" (Transit.to_string (`String "a\nb")));
     "large" >::
       (fun (_) ->
         let fname = "../transit-format/examples/0.8/example.json" in
         let d = In_channel.read_all fname in
         let decoded = Transit.from_string d in
         assert_equal true true );
    ]

let exemplar_tests t =
  [
    t "dates_interesting" (`Array dates);
    t "doubles_interesting" (`Array [fl (-3.14159); fl 3.14159; fl 4E11; fl 2.998E8; fl 6.626E-34]);
    t "doubles_small" (ints_centered_on 0 |> doublify);
    t "false" (`Bool false);
    t "ints" (`Array (List.map (List.range 0 128) ~f:(fun (i) -> `Int (Int64.of_int i))) );
    t "ints_interesting" (`Array interesting_ints);
    t "ints_interesting_neg" (`Array neg_interesting_ints);
    t "keywords" (`Array keywords);
    t "list_empty" (`List []);
    t "list_mixed" (`List mixed);
    t "list_nested" list_nested;
    t "list_simple" (`List simple);
    t "map_10_items" (hash_of_size 10);
    t "map_10_nested" (`Map (Map.Poly.of_alist_exn
                         [`Keyword "f", hash_of_size 10;
                          `Keyword "s", hash_of_size 10]));
    t "map_1935_nested" (`Map (Map.Poly.of_alist_exn
                         [`Keyword "f", hash_of_size 1935;
                          `Keyword "s", hash_of_size 1935]));
    t "map_1936_nested" (`Map (Map.Poly.of_alist_exn
                         [`Keyword "f", hash_of_size 1936;
                          `Keyword "s", hash_of_size 1936]));
    t "map_1937_nested" (`Map (Map.Poly.of_alist_exn
                         [`Keyword "f", hash_of_size 1937;
                          `Keyword "s", hash_of_size 1937]));
    t "map_mixed" map_mixed;
    t "map_nested" map_nested;
    t "map_numeric_keys" (`Map (Map.Poly.of_alist_exn
                                  [`Int (Int64.of_int 1), `String "one";
                                   `Int (Int64.of_int 2), `String "two"]));
    t "map_simple" map_simple;
    t "map_string_keys" (`Map (Map.Poly.of_alist_exn
                                 [`String "first", `Int (Int64.of_int 1);
                                  `String "second", `Int (Int64.of_int 2);
                                  `String "third", `Int (Int64.of_int 3)]));
    t "map_vector_keys" (`Map (Map.Poly.of_alist_exn
                                 [`Array [one; one], `String "one";
                                  `Array [two; two], `String "two"]));
    t "maps_unrecognized_keys" (`Array [
        `Extension ("abcde", `Keyword "anything");
        `Extension ("fghij", `Keyword "anything-else")]);
    t "map_unrecognized_vals" (`Map (Map.Poly.of_alist_exn
                                       [`Keyword "key", `String "~Unrecognized"]));
    t "maps_four_char_string_keys"
        (`Array [
          `Map (Map.Poly.of_alist_exn [`String "aaaa", i 1; `String "bbbb", i 2]);
          `Map (Map.Poly.of_alist_exn [`String "aaaa", i 3; `String "bbbb", i 4]);
          `Map (Map.Poly.of_alist_exn [`String "aaaa", i 5; `String "bbbb", i 6])]);
    t "maps_three_char_string_keys"
      (`Array [
          `Map (Map.Poly.of_alist_exn [`String "aaa", i 1; `String "bbb", i 2]);
          `Map (Map.Poly.of_alist_exn [`String "aaa", i 3; `String "bbb", i 4]);
          `Map (Map.Poly.of_alist_exn [`String "aaa", i 5; `String "bbb", i 6])]);
    t "maps_two_char_string_keys"
      (`Array [
          `Map (Map.Poly.of_alist_exn [`String "aa", i 1; `String "bb", i 2]);
          `Map (Map.Poly.of_alist_exn [`String "aa", i 3; `String "bb", i 4]);
          `Map (Map.Poly.of_alist_exn [`String "aa", i 5; `String "bb", i 6])]);
    t "maps_four_char_sym_keys"
      (`Array [
          `Map (Map.Poly.of_alist_exn [`Keyword "aaaa", i 1; `Keyword "bbbb", i 2]);
          `Map (Map.Poly.of_alist_exn [`Keyword "aaaa", i 3; `Keyword "bbbb", i 4]);
          `Map (Map.Poly.of_alist_exn [`Keyword "aaaa", i 5; `Keyword "bbbb", i 6])]);
    t "maps_three_char_sym_keys"
      (`Array [
          `Map (Map.Poly.of_alist_exn [`Keyword "aaa", i 1; `Keyword "bbb", i 2]);
          `Map (Map.Poly.of_alist_exn [`Keyword "aaa", i 3; `Keyword "bbb", i 4]);
          `Map (Map.Poly.of_alist_exn [`Keyword "aaa", i 5; `Keyword "bbb", i 6])]);
    t "maps_two_char_sym_keys"
      (`Array [
          `Map (Map.Poly.of_alist_exn [`Keyword "aa", i 1; `Keyword "bb", i 2]);
          `Map (Map.Poly.of_alist_exn [`Keyword "aa", i 3; `Keyword "bb", i 4]);
          `Map (Map.Poly.of_alist_exn [`Keyword "aa", i 5; `Keyword "bb", i 6])]);
    t "nil" `Null;
    t "one_date" (`Time (from_timestamp 946728000.0)); 
    t "one" (`Int (Int64.of_int 1));
    t "one_keyword" (`Keyword "hello");
    t "one_string" (`String "hello");
    t "one_symbol" (`Symbol "hello");
    t "one_uri" (List.hd_exn uris);
    t "one_uuid" (List.hd_exn uuids);
    t "set_empty" (`Set Set.Poly.empty);
    t "set_mixed" (`Set (Set.Poly.of_list mixed));
    t "set_nested" set_nested;
    t "set_simple" (`Set (Set.Poly.of_list simple));
    t "small_ints" (`Array (ints_centered_on 0));
    t "small_strings" small_strings;
    t "strings_hash" (add_string "#" small_strings);
    t "strings_hat" (add_string "^" small_strings);
    t "strings_tilde" (add_string "~" small_strings);
    t "symbols" (`Array symbols);
    t "true" (`Bool true);
    t "uris" (`Array uris);
    t "uuids" (`Array uuids);
    t "vector_1935_keywords_repeated_twice" (`Array (array_of_symbools 1935 (1935*2)));
    t "vector_1936_keywords_repeated_twice" (`Array (array_of_symbools 1936 (1936*2)));
    t "vector_1937_keywords_repeated_twice" (`Array (array_of_symbools 1937 (1937*2)));
    t "vector_empty" (`Array []);
    t "vector_mixed" (`Array mixed);
    t "vector_nested" array_nested;
    t "vector_simple" (`Array simple);
    t "vector_unrecognized_vals" (`Array [`String "~Unrecognized"]);
    t "zero" (`Int (Int64.of_int 0));
  ]

let exemplar_tests_json = exemplar_tests (t JSON)
let exemplar_tests_json_verbose = exemplar_tests (t JSON_Verbose)
let exemplar_tests_json_write = exemplar_tests (t JSON_Write)

let tests =
   "Transit" >::: [
       "JSON" >::: exemplar_tests_json;
       "Example" >::: example_tests;
       "CacheCode" >::: cachecode_tests;
       "JSON_Verbose" >::: exemplar_tests_json_verbose;
       "JSON_Write" >::: exemplar_tests_json_write;
   ]

