(* Transit *)
open Core.Std

(* TODO LIST of what needs implementation:
 * - Caching logic in the string read path
 * - Tagged values in the array read path
 * - Tagged values in the map read path
 * - Layered decoders for specialization
 * - All of the write path
 *)

(* This is a very simple cache implementation which runs on an 'string Int.Map.t'. This is a requirement for correct
  * transit operation. We use a Map, even though an Array would be faster on one side of things and is suggested by
  * the transit people. However, a map is symmetric and nicer for a functional language
  *)
module type Cache = sig
    type t
    
    val empty : t
    val track : t -> string -> t
    val find : t -> int -> string option
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
            
    let find {m ; _ } x = Int.Map.find m x
    
end

module Cache : Cache = String_cache
    
exception NotImplemented
exception Parse_error of string

type t =
  [ | `Null
    | `String of string
    | `Bool of Bool.t
    | `Int of Int64.t
    | `Float of Float.t
    | `Bytes of string
    | `Array of t list
    | `Map of (t * t) list
    (* Extension types *)
    | `Keyword of string
    | `Symbol of string
    | `BigInt of Big_int.big_int ]

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
    | 'b' -> `Bytes (Base64.decode_string s)

    (* Extension types *)
    | ':' -> `Keyword s
    | '$' -> `Symbol s
    | 'n' -> `BigInt (Big_int.big_int_of_string s)
    | _ -> raise (Parse_error "untag")
  in
    (r, c)

let decode_string c s =
  match (s.[0], s.[1]) with
  | ('~', '~') -> (`String (String.drop_prefix s 1), c)
  | ('~', tag) -> untag c tag (String.drop_prefix s 2)
  | _ -> (`String s, c)

type tag_view =
   Yes of char
 | No

let view_tag = function
  | _ -> No

let from_string str =
  let empty_cache = Cache.empty in
  let rec decode_array c arr =
      match view_tag arr with
        | Yes _ -> raise NotImplemented
        | No ->
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

                  
