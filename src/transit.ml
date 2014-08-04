(* Transit *)
open Core.Std

(* This is a very simple cache implementation which runs on an 'string Int.Map.t'. This is a requirement for correct
  * transit operation. We use a Map, even though an Array would be faster on one side of things and is suggested by
  * the transit people. However, a map is symmetric and nicer for a functional language
  *)
  
module type Cache = sig
    type t
    
    val empty : t
    val track : t -> string -> t
    val find_exn : t -> int -> string
end

module String_cache = struct
    type t = {
	m : string Int.Map.t;
	c : int
    }

    let empty =
      { m = Int.Map.empty; c = 0 }
      
    let track { m; c } s =
        let m' = Int.Map.add m ~key:c ~data:s in
            { m = m'; c = c + 1 }
            
    let find_exn {m ; _ } x = Int.Map.find_exn m x

end

module Cache : Cache = String_cache

module Parser = struct

  (* Used for internal errors in the parser that should never happen *)
  exception Internal_error
  
  (* Used for errors in the parse input which we don't understand *)
  exception Parse_error of string

  type 'a ground =
    [ | `Null
      | `String of string
      | `Bool of Bool.t
      | `Int of Int64.t
      | `Float of Float.t
      | `Array of 'a list
      | `Map of ('a * 'a) list ]
  
  module Extension = struct
  
    type t =
      [ | t ground
        | `UUID of Uuid.t
        | `Keyword of string
        | `Symbol of string
        | `Date of Time.t
        | `URI of string
      ]
      
    let decode_tagged s = function
      | 'u' -> Some (`UUID (Uuid.of_string s))
      | 'r' -> Some (`URI s)
      | ':' -> Some (`Keyword s)
      | '$' -> Some (`Symbol s)
      | 'm' ->
        let f = Big_int.float_of_big_int (Big_int.big_int_of_string s) in
          Some (`Date (Time.of_float (f /. 1000.0)))
      | _ -> None

    let to_string = function
      | `UUID uuid -> String.concat ["uuid("; Uuid.to_string uuid; ")"]
      | `URI uri -> String.concat ["uri("; uri; ")"]
      | `Keyword s -> String.concat [":"; s]
      | `Symbol s -> String.concat ["$"; s]
      | `Date ts -> Time.to_string ts
      | _ -> raise Internal_error
  end

  type t = Extension.t

  (* Type of parser contexts *)
  type context =
    | Focus of t list
    | Array of t list * context
    | Map of ((t * t) list) * context

  let ctx_to_string x =
    match x with
    | Focus lst -> String.concat ~sep:" " ["Focus"; Int.to_string (List.length lst)]

  let push e = function
    | Focus es -> Focus (e :: es)
    | Array (es, ctx) -> Array (e :: es, ctx)

  let decode_tagged s = function
    | '_' -> `Null
    | 's' -> `String s
    | '?' ->
      (match s with
       | "t" -> `Bool true
       | "f" -> `Bool false
       | _ -> raise (Parse_error "decode_tagged ? case"))
    | 'i' -> `Int (Int64.of_string s)
    | 'd' -> `Float (Float.of_string s)
    | t -> (match Extension.decode_tagged s t with
            | None -> `String s
            | Some value -> value)

  let decode_string s =
    let analyze_string = function
      | ('~', '~') -> `String (String.drop_prefix s 1)
      | ('~', '^') -> `String (String.drop_prefix s 1)
      | ('~', '#') -> `String s (* Array tag *)
      | ('~', t) -> decode_tagged (String.drop_prefix s 2) t
      | _ -> `String s
    in
      match String.length s with
      | 0 | 1 -> `String s
      | _ -> analyze_string (s.[0], s.[1])
    
  (* Parser rules *)
  exception Todo
  let on_null (cache, ctx) = (cache, push `Null ctx)
  let on_bool (cache, ctx) b = (cache, push (`Bool b) ctx)
  let on_int (cache, ctx) i = (cache, push (`Int i) ctx)
  let on_float (cache, ctx) f = (cache, push (`Float f) ctx)
  let on_string (cache, ctx) buf offset len =
    let str = String.sub buf offset len in
      (cache, push (decode_string str) ctx)
  let on_start_map _ = raise Todo
  let on_map_key _ buf offset len = raise Todo
  let on_end_map _ = raise Todo
  let on_start_array (cache, ctx) = (cache, Array ([], ctx))
  let on_end_array (cache, ctx) =
    match ctx with
    | Array (res, parent) ->
      let es = List.rev res in
        (match es with
        |  [] -> (cache, push (`Array []) parent)
        |  [`String "~#'"; e] -> (cache, push e parent)
        |  elems -> (cache, push (`Array elems) parent))
        
  
  let callbacks = {
    YAJL.on_null = on_null;
    on_bool = on_bool;
    on_number = `Parse_numbers ((`Int64 on_int), on_float);
    on_string = on_string;
    on_start_map = on_start_map;
    on_map_key = on_map_key;
    on_end_map = on_end_map;
    on_start_array = on_start_array;
    on_end_array = on_end_array;
  }
    
  let from_string str =
    let p = YAJL.make_parser callbacks (Cache.empty, Focus []) in
    let () = YAJL.parse p str in
    let (_, result) = YAJL.complete_parse p in
      match result with
      | Focus [elem] -> elem
      | x -> print_endline (ctx_to_string x); raise Internal_error
      
end

type t = Parser.Extension.t
let from_string = Parser.from_string

let rec to_string x =
  match x with
  | `Null -> "null"
  | `String s -> String.concat ["\""; s; "\""]
  | `Bool true -> "true"
  | `Bool false -> "false"
  | `Int i -> Int64.to_string i
  | `Float f -> Float.to_string f
  | `Array arr ->
    let contents = List.map arr ~f:to_string in
    let contents' = String.concat ~sep:" | " contents in
        String.concat (["["; contents'; "]"])
  | `Map m ->
    "?MAP"
  | ext -> Parser.Extension.to_string ext


(*
exception NotImplemented
exception Invalid
exception Parse_error of string
exception Cache_lookup

type t =
  [ | `Null
    | `String of string
    | `Bool of Bool.t
    | `Int of Int64.t
    | `Float of Float.t
    | `Array of t list
    | `Map of (t * t) list
]

let untag c t s =
  let r = match t with
    | '_' -> `Null
    | 's' -> `String s
    | '?' -> (match s with
              | "t" -> `Bool true
              | "f" -> `Bool false
              | _ -> raise (Parse_error "untag ? case"))
    | 'i' -> `Int (Int64.of_string s)
    | 'd' -> `Float (Float.of_string s)
    | ':' -> `Keyword s
    | '$' -> `Symbol s
    | 'u' -> `UUID (Uuid.of_string s)
    | 'r' -> `URI s
    | 'n' -> `BigInt (Big_int.big_int_of_string s)
    | 'm' ->
        let f = Big_int.float_of_big_int (Big_int.big_int_of_string s) in
          `Date (Time.of_float (f /. 1000.0))
    | _ -> raise (Parse_error "untag")
  in
    (r, c)

let decode_string c s =
  match String.length s with
  | 0 | 1 -> (`String s, c)
  | _ -> (match (s.[0], s.[1]) with
  | ('^', _) ->
      let i = Int.of_string (String.drop_prefix s 1)
      in
       (match Cache.find c i with
        | Some v -> (`String v, c)
        | None -> raise Cache_lookup)
  | ('~', '~') -> (`String (String.drop_prefix s 1), c)
  | ('~', '^') -> (`String (String.drop_prefix s 1), c)
  | ('~', tag) -> untag c tag (String.drop_prefix s 2)
  | _ -> (`String s, c))

type extension =
    | Quote
    | List
    | Set

let view_tag = function
  | [] -> None
  | ((`String h) :: t) ->
      (match h with
       | "~#'" -> Some Quote
       | "~#list" -> Some List
       | "~#set" -> Some Set
       | _ -> None)
  | _ -> None

let rec to_string x =
  match x with
  | `Null -> "null"
  | `String s -> String.concat ["\""; s; "\""]
  | `Bool true -> "true"
  | `Bool false -> "false"
  | `Int i -> Int64.to_string i
  | `Float f -> Float.to_string f
  | `Keyword s -> String.concat [":"; s]
  | `Symbol s -> String.concat ["$"; s]
  | `Date ts -> Time.to_string ts
  | `Array arr ->
    let contents = List.map arr ~f:to_string in
    let contents' = String.concat ~sep:" | " contents in
        String.concat (["["; contents'; "]"])
  | `Map m ->
    "?MAP"
  | `URI s -> String.concat ["uri("; s; ")"]
  | `UUID uuid -> String.concat ["uuid("; Uuid.to_string uuid; ")"]
  | `BigInt i -> Big_int.string_of_big_int i
  | `List lst ->
    let contents = List.map lst ~f:to_string in
    let contents' = String.concat ~sep:"; " contents in
        String.concat (["["; contents'; "]"])


let from_string str =
  let empty_cache = Cache.empty in
  let rec decode_array c arr =
      match view_tag arr with
        | Some Quote ->
            let [rest] = List.tl_exn arr in
              conv c rest
        | Some List ->
            let [rest] = List.tl_exn arr in
            (match conv c rest with
            | (`Array x, c) -> (`List x, c)
            | _ -> raise Invalid)
        | Some _ -> raise NotImplemented
        | None ->
                 let f (es, c) e = let (r, c') = conv c e in (r :: es, c') in
                 let (r, c') = List.fold_left arr ~init:([], c) ~f:f in
                   (`Array (List.rev r), c')
  and decode_map c assocs =
	let f (es, c) (str, json) =
	    let (s, c') = decode_string c str in
	    let (j, c'') = conv c' json in
	    let r = (s, j) in
	      (r :: es, c'') in
	let (r, c') =
	  List.fold_left assocs ~init:([], c) ~f:f
	in
	  (`Map (List.rev r), c')
  and conv c = function
    | `Null -> (`Null, c)
    | `String s -> decode_string c s
    | `Bool b -> (`Bool b, c)
    | `Int i -> (`Int (Int64.of_int i), c)
    | `Float f -> (`Float f, c)
    | `List arr -> decode_array c arr
    | `Assoc assocs -> decode_map c assocs in
  let (r, _) = conv empty_cache (Yojson.Basic.from_string str)
  in
    r

*)
