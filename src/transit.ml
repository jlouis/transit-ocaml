(* Transit *)
open Core.Std

(* This is a very simple cache implementation which runs on an 'string Int.Map.t'. This is a requirement for correct
  * transit operation. We use a Map, even though an Array would be faster on one side of things and is suggested by
  * the transit people. However, a map is symmetric and nicer for a functional language
  *)
  

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
      | `Array of 'a list ] with compare, sexp
  
  module Extension = struct
  
    type t =
      [ | t ground
        | `UUID of Uuid.t
        | `Keyword of string
        | `Symbol of string
        | `Date of Time.t
        | `URI of string
        | `List of t list
        | `Set of t Set.Poly.t
        | `Map of (t, t) Map.Poly.t
      ] with sexp, compare
      
    let decode_tagged s = function
      | 'u' -> Some (`UUID (Uuid.of_string s))
      | 'r' -> Some (`URI s)
      | ':' -> Some (`Keyword s)
      | '$' -> Some (`Symbol s)
      | 'm' ->
        let f = Big_int.float_of_big_int (Big_int.big_int_of_string s) in
          Some (`Date (Time.of_float (f /. 1000.0)))
      | _ -> None

    let unarray = function
      | `Array arr -> arr
      | _ -> raise Internal_error

    let decode_array es = function
      | "~#list" -> Some (`List (unarray es))
      | "~#set" -> Some (`Set (Set.Poly.of_list (unarray es)))
      | _ -> None

    let to_string = function
      | `UUID uuid -> String.concat ["uuid("; Uuid.to_string uuid; ")"]
      | `URI uri -> String.concat ["uri("; uri; ")"]
      | `Keyword s -> String.concat [":"; s]
      | `Symbol s -> String.concat ["$"; s]
      | `Date ts -> Time.to_string ts
      | `List lst -> "?List"
      | `Set set -> "?Set"
      | _ -> raise Internal_error
  end

  type t = Extension.t

  module type Cache = sig
    type t
    
    val empty : t
    val track : t -> Extension.t -> t
    val find_exn : t -> int -> Extension.t
  end

  module Transit_cache = struct
    type t = {
	  m : Extension.t Int.Map.t;
	  c : int
    }

    let empty =
      { m = Int.Map.empty; c = 0 }
      
    let track { m; c } s =
        let m' = Int.Map.add m
                             ~key:c
                             ~data:s in
          { m = m';
            c = c + 1 }
            
    let find_exn {m ; _ } x = Int.Map.find_exn m x

  end

  module Cache : Cache = Transit_cache

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

  let decode_string cache s =
    let analyze_string = function
      | ('~', '~') -> `String (String.drop_prefix s 1)
      | ('~', '^') -> `String (String.drop_prefix s 1)
      | ('~', '#') -> `String s (* Array tag *)
      | ('~', t) -> decode_tagged (String.drop_prefix s 2) t
      | ('^', ' ') -> `String s (* Map as array *)
      | ('^', _) ->
        let i = Int.of_string (String.drop_prefix s 1)in
          Cache.find_exn cache i
      | _ -> `String s
    in
      match String.length s with
      | 0 | 1 -> `String s
      | _ -> analyze_string (s.[0], s.[1])
    
  let cache_check cache (str : String.t) ctx =
    let track () =
      let decoded = decode_string cache str in
        (Cache.track cache decoded, push decoded ctx)
    in
      if String.length str > 3
      then
        match (str.[0], str.[1]) with
        | ('~', ':') -> track ()
        | ('~', '$') -> track ()
        | ('~', '#') -> track ()
        | _ -> (cache, push (decode_string cache str) ctx)
      else
        (cache, push (decode_string cache str) ctx)

  let rec map_as_array es =
    let rec loop = function
      | [] -> []
      | (k :: v :: rest) -> (k, v) :: (loop rest)
      | _ -> raise (Parse_error "Map-as-array has an odd number of arguments")
    in
      `Map (Map.Poly.of_alist_exn (loop es))

  (* Parser rules *)
  exception Todo
  let on_null (cache, ctx) = (cache, push `Null ctx)
  let on_bool (cache, ctx) b = (cache, push (`Bool b) ctx)
  let on_int (cache, ctx) i = (cache, push (`Int i) ctx)
  let on_float (cache, ctx) f = (cache, push (`Float f) ctx)
  let on_string (cache, ctx) buf offset len =
    let str = String.sub buf offset len in
      cache_check cache str ctx
  let on_start_map _ = raise Todo
  let on_map_key _ buf offset len = raise Todo
  let on_end_map _ = raise Todo
  let on_start_array (cache, ctx) = (cache, Array ([], ctx))
  let on_end_array (cache, ctx) =
    let handle_array parent = function
      | [] -> (cache, push (`Array []) parent)
      | (`String "^ ") :: elems -> (cache, push (map_as_array elems) parent)
      | [`String "~#'"; quoted] -> (cache, push quoted parent)
      | [`String tag; value] as elems ->
          (match Extension.decode_array value tag with
           | None -> (cache, push (`Array elems) parent)
           | Some value -> (cache, push value parent))
      | es -> (cache, push (`Array es) parent)
    in
      match ctx with
      | Array (res, parent) -> List.rev res |> handle_array parent
  
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
        String.concat ["["; contents'; "]"]
  | `Map m -> "?MAP"
  | ext -> Parser.Extension.to_string ext

let sexp_of_t = Parser.Extension.sexp_of_t
let t_of_sexp = Parser.Extension.t_of_sexp
