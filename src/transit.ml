(* Transit *)
open Core.Std

(* We choose to make the base type a conglomeration of everything in the transit-format
 * spec. This is deliberate, since it is hard to claim to support Transit without implementing
 * All the spec, ground and extension types.
 *
 * This choice makes it a bit easier to work with the types in question. *)

module T = struct
  type t =
    [ | `Null
      | `String of String.t
      | `Bool of Bool.t
      | `Int of Int64.t
      | `Float of Float.t
      | `Array of t list
      | `Map of (t, t) Map.Poly.t
      | `UUID of Uuid.t
      | `Keyword of String.t
      | `Symbol of String.t
      | `Date of Time.t
      | `URI of String.t
      | `List of t list
      | `Set of t Set.Poly.t ] with sexp, compare
end

module CacheCode = struct
  exception Invalid_cache_code

  let base_char_index = 48
  let cache_code_digits = 44

  let to_int s =
    match String.length s with
    | 1 ->
      (Char.to_int s.[0]) - base_char_index
    | 2 ->
      ((Char.to_int s.[0]) - base_char_index) * cache_code_digits +
      ((Char.to_int s.[1]) - base_char_index)
    | _ -> raise Invalid_cache_code

end

module type Cache = sig
  type t

  val empty : t
  val track : t -> T.t -> t
  val find_exn : t -> int -> T.t
end

module Transit_cache = struct
  type t = {
    m : T.t Int.Map.t;
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

module Parser = struct

  (* Used for internal errors in the parser that should never happen
   * Internal_errors means the parser is wrong in its assumptions somewhere
  *)
  exception Internal_error

  (* Used for errors in the parse input which we don't understand
   * Parser_errors puts the blame on the data in the file which are not valid
   * Transit
  *)
  exception Parse_error of string

  (* Type of parser contexts *)
  type context =
    | Empty
    | Focused of T.t
    | Array of T.t list * context
  with sexp

  let ctx_to_string x = sexp_of_context x |> Sexp.to_string

  (* Push the element "e" onto the Context, depending on what it looks like *)
  let push e = function
    | Empty -> Focused e
    | Array (es, ctx) -> Array (e :: es, ctx)
    | Focused _ -> raise Internal_error

  let unarray = function
    | `Array arr -> arr
    | _ -> raise (Parse_error "Expected array but got something else")

  let map_as_array es =
    let rec loop = function
      | [] -> []
      | (k :: v :: rest) -> (k, v) :: (loop rest)
      | _ -> raise (Parse_error "Map-as-array has an odd number of arguments")
    in
    `Map (Map.Poly.of_alist_exn (loop es))

  let decode_array es = function
    | "~#list" -> Some (`List (unarray es))
    | "~#set" -> Some (`Set (Set.Poly.of_list (unarray es)))
    | "~#cmap" -> Some (map_as_array (unarray es))
    | _ -> None

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
    | 'u' -> (`UUID (Uuid.of_string s))
    | 'r' -> (`URI s)
    | ':' -> (`Keyword s)
    | '$' -> (`Symbol s)
    | 'm' ->
      let f = Big_int.float_of_big_int (Big_int.big_int_of_string s) in
      (`Date (Time.of_float (f /. 1000.0)))
    | _ -> `String s

  let decode_string cache s =
    let analyze_string = function
      | ('~', '~') -> `String (String.drop_prefix s 1)
      | ('~', '^') -> `String (String.drop_prefix s 1)
      | ('~', '#') -> `String s (* Array tag *)
      | ('~', t) -> decode_tagged (String.drop_prefix s 2) t
      | ('^', ' ') -> `String s (* Map as array *)
      | ('^', _) ->
        let i = CacheCode.to_int (String.drop_prefix s 1)in
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
        (match decode_array value tag with
         | None -> (cache, push (`Array elems) parent)
         | Some value -> (cache, push value parent))
      | es -> (cache, push (`Array es) parent)
    in
    match ctx with
    | Array (res, parent) -> List.rev res |> handle_array parent

  module JSON = struct
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
      let p = YAJL.make_parser callbacks (Cache.empty, Empty) in
      let () = YAJL.parse p str in
      let (_, result) = YAJL.complete_parse p in
      match result with
      | Focused elem -> elem
      | x -> print_endline (ctx_to_string x); raise Internal_error
  end
end

type t = T.t
let from_string = Parser.JSON.from_string

let sexp_of_t = T.sexp_of_t
let t_of_sexp = T.t_of_sexp
