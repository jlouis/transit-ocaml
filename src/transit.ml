(* Transit *)
open Core.Std

(* We choose to make the base type a conglomeration of everything in the transit-format
 * spec. This is deliberate, since it is hard to claim to support Transit without implementing
 * All the spec, ground and extension types.
 *
 * This choice makes it a bit easier to work with the types in question. *)

(* module T contains the Transit format specification with comparators and sexp'ers *)
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

(* Cache codes in transit follow a radix 44 encoding. Transform these into integers
 * internally by means of helper functions. There are deliberately no CacheCode.t internal
 * representation
 *)
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

(* Signature of the cache as an abstract thing *)
module type Cache = sig
  type t

  val empty : t
  val track : t -> T.t -> t
  val find_exn : t -> int -> T.t
end

(* Implementation via a map over integers *)
module Transit_cache = struct
  type t = {
    m : T.t Int.Map.t;
    c : int
  }

  let max_count = 44 * 44

  let empty =
    { m = Int.Map.empty; c = 0 }

  let track { m; c } s =
    let m' = Int.Map.add m
        ~key:c
        ~data:s
    in
    if c > max_count then empty else { m = m'; c = c + 1 }

  let find_exn {m ; _ } x = Int.Map.find_exn m x

end

module Cache : Cache = Transit_cache

(* Code for parsing transit structures *)
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

  module Context = struct
    (* When decoding arrays, they will be tagged. These are the possible tag values *)
    type tag =
      | Quote
      | List
      | Set
      | CMap
      | Unknown of string
    with sexp

    (* Type of parser contexts:
     * A parser is operating by means of a parsing stack which explains
     * "where-we-are" in a parse. It starts out empty but may be consed onto when we
     * enter a new context (an array say). Note the transit-parser is context sensitive, so
     * the context is needed for correct parses of data *)
    type context =
      | Empty
      | Focused of T.t
      | Array of T.t list * context
      | TaggedArray of tag * T.t list * context
      | MapKey of (T.t * T.t) list * context
      | MapValue of T.t * (T.t * T.t) list * context
    with sexp

    (* For convenience *)
    let to_string x = sexp_of_context x |> Sexp.to_string

    (* Push the element "e" onto the Context, depending on what it looks like. Essentially it "does the right thing™"  *)
    let push e = function
      | Empty -> Focused e
      | Focused _ -> raise Internal_error
      | Array (es, ctx) -> Array (e :: es, ctx)
      | TaggedArray (t, es, ctx) -> TaggedArray (t, e :: es, ctx)
      | MapKey (es, ctx) -> MapValue (e, es, ctx)
      | MapValue (k, es, ctx) -> MapKey ((k, e) :: es, ctx)

    let tag_of_string = function
      | "~#'" -> Quote
      | "~#list" -> List
      | "~#set" -> Set
      | "~#cmap" -> CMap
      | s -> Unknown s
  end

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


  (* Decode and check if the string is cacheable *)
  let decode_string s (cache, ctx) head =
    let track s x =
      if String.length s > 3
      then (Cache.track cache x, Context.push x ctx)
      else (cache, Context.push x ctx) in
    match head with
    | ('^', ' ') ->
      (* If the thing we have is a map-as-array-marker, handle it specially! *)
      (match ctx with
      | Array ([], parent) -> (cache, Context.MapKey ([], parent))
      | _ -> raise (Parse_error "Map-as-array marker in wrong location!"))
    | ('^', _) ->
      let i = CacheCode.to_int (String.drop_prefix s 1) in
      (cache, Context.push (Cache.find_exn cache i) ctx)
    | ('~', '~') -> (cache, Context.push (`String (String.drop_prefix s 1)) ctx)
    | ('~', '^') -> (cache, Context.push (`String (String.drop_prefix s 1)) ctx)
    | ('~', '#') ->
        (match ctx with
         | Array ([], parent) -> (cache, TaggedArray (Context.tag_of_string s, [], parent))
         | _ -> raise (Parse_error "Array tag, but not parsing an array"))
    | ('~', t) ->
       (match decode_tagged (String.drop_prefix s 2) t with
       | `Symbol s -> track s (`Symbol s)
       | `Keyword k -> track s (`Keyword k)
       | value -> (cache, Context.push value ctx))
    | _ ->
      (match ctx with
       | MapKey (_, _) -> track s (`String s)
       | _ -> (cache, Context.push (`String s) ctx))

  let rec pairup = function
    | [] -> []
    | k :: v :: rest -> (k, v) :: (pairup rest)
    | _ -> raise (Parse_error "Odd number of elements in cmap")

  (* Parser rules *)
  exception Todo
  let on_null (cache, ctx) = (cache, Context.push `Null ctx)

  let on_bool (cache, ctx) b = (cache, Context.push (`Bool b) ctx)

  let on_int (cache, ctx) i = (cache, Context.push (`Int i) ctx)

  let on_float (cache, ctx) f = (cache, Context.push (`Float f) ctx)

  let on_string (cache, ctx) buf offset len =
    let str = String.sub buf offset len in
    match String.length str with
    | 0 | 1 -> (cache, Context.push (`String str) ctx)
    | _ -> decode_string str (cache, ctx) (str.[0], str.[1])

  let on_start_map _ = raise Todo

  let on_map_key _ _ _ _ = raise Todo

  let on_end_map _ = raise Todo

  let on_start_array (cache, ctx) = (cache, Context.Array ([], ctx))

  let on_end_array (cache, ctx) =
    (cache,
     match ctx with
     | Context.Array (res, parent) -> Context.push (`Array (List.rev res)) parent
     | Context.TaggedArray (Context.List, [`Array res], parent) -> Context.push (`List res) parent
     | Context.TaggedArray (Context.List, _, _) -> raise (Parse_error "Wrong ~#list encoding")
     | Context.TaggedArray (Context.Set, [`Array res], parent) -> Context.push (`Set (Set.Poly.of_list res)) parent
     | Context.TaggedArray (Context.Set, _, _) -> raise (Parse_error "Wrong ~#set encoding")
     | Context.TaggedArray (Context.Quote, [res], parent) -> Context.push res parent
     | Context.TaggedArray (Context.Quote, _, _) -> raise (Parse_error "Quote with multi-elem array")
     | Context.TaggedArray (Context.CMap, [`Array res], parent) ->
         Context.push (`Map (Map.Poly.of_alist_exn (pairup res))) parent
     | Context.MapValue (_, _, _) -> raise (Parse_error "Odd number of k/v pairs in map-as-array or cmap")
     | Context.MapKey (res, parent) -> Context.push (`Map (Map.Poly.of_alist_exn res)) parent
     | Context.Empty -> raise (Parse_error "end_of_array called in Empty context")
     | Context.Focused _ -> raise (Parse_error "end_of_array called in Focused context")
    )

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
      | x -> print_endline (Context.to_string x); raise Internal_error
  end
end

type t = T.t
let from_string = Parser.JSON.from_string

let sexp_of_t = T.sexp_of_t
let t_of_sexp = T.t_of_sexp
