(* Transit *)
open Core.Std

module Big_int = struct
  include Big_int
  exception Error
  
  let sexp_of_big_int x = Sexp.Atom (Big_int.string_of_big_int x)
  let big_int_of_sexp sexp =
    match sexp with
    | Sexp.Atom str -> Big_int.big_int_of_string str
    | Sexp.List _ -> raise Error
end

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
      | `BigInt of Big_int.big_int
      | `Float of Float.t
      | `Array of t list
      | `Map of (t, t) Map.Poly.t
      | `UUID of Uuid.t
      | `Keyword of String.t
      | `Symbol of String.t
      | `Date of Time.t
      | `URI of String.t
      | `Tagged of char * t
      | `List of t list
      | `Set of t Set.Poly.t ] with sexp, compare
end

(* Cache codes in transit follow a radix 44 encoding. Transform these into integers
 * internally by means of helper functions. There are deliberately no CacheCode.t internal
 * representation
 *)
module CacheCode = struct
  exception Invalid_cache_code of string

  let base_char_index = 48
  let cache_code_digits = 44

  let to_int s =
    match String.length s with
    | 1 ->
      (Char.to_int s.[0]) - base_char_index
    | 2 ->
      ((Char.to_int s.[0]) - base_char_index) * cache_code_digits +
      ((Char.to_int s.[1]) - base_char_index)
    | _ -> raise (Invalid_cache_code s)

end

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

    let tag_of_string = function
      | "~#'" -> Quote
      | "~#list" -> List
      | "~#set" -> Set
      | "~#cmap" -> CMap
      | s -> Unknown s

    (* Implementation via a map over integers *)
    module Transit_cache = struct
      type entry = ETransit of T.t
                 | ETag of tag

      type t = { m : entry Int.Map.t;
         		 c : int }
    
      let max_count = 44 * 44
    
      let empty =
        { m = Int.Map.empty; c = 0 }
    
      let track { m; c } s =
        let m' = Int.Map.add m
            ~key:c
            ~data:s
        in
        if c > max_count then empty else { m = m'; c = c + 1 }
    
      let track_transit x v = track x (ETransit v)
      let track_tag x v = track x (ETag v)

      let find_exn {m ; _ } x = Int.Map.find_exn m x
    
    end
    
    module Cache = Transit_cache
   
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

    type t = Ctx of Cache.t * context

    let empty = Ctx (Cache.empty, Empty)
    let result (Ctx (_, ctx)) =
      match ctx with
      | Focused v -> v
      | _ -> raise Internal_error

    (* For convenience *)
    let to_string (Ctx (_, x)) = sexp_of_context x |> Sexp.to_string

	let track_transit (Ctx (cache, ctx)) x =
	    Ctx (Cache.track_transit cache x, ctx)

    let track_tag (Ctx (cache, ctx)) x =
        Ctx (Cache.track_tag cache x, ctx)

    (* Push the element "e" onto the Context, depending on what it looks like. Essentially it "does the right thing™"  *)
    let add ((Ctx (cache, ctx)) as cx) e =
      match ctx with
      | Empty -> Ctx (cache, Focused e)
      | Focused _ -> raise Internal_error
      | Array (es, ctx) -> Ctx (cache, Array (e :: es, ctx))
      | TaggedArray (t, es, ctx) -> Ctx (cache, TaggedArray (t, e :: es, ctx))
      | MapKey (es, ctx) ->
          (match e with
           | `String s when String.length s > 3 ->
               Ctx (Cache.track_transit cache e, MapValue (e, es, ctx))
           | _ ->
               Ctx (cache, MapValue (e, es, ctx)) )
      | MapValue (k, es, ctx) -> Ctx (cache, MapKey ((k, e) :: es, ctx))
    
    let push_map (Ctx (cache, ctx)) =
      match ctx with
      | Array ([], parent) -> Ctx (cache, MapKey ([], parent))
      | _ -> raise (Parse_error "Map-as-array marker in wrong location!")

    let push_array (Ctx (cache, ctx)) = Ctx (cache, Array ([], ctx))

    let push_tagged_array (Ctx (cache, ctx)) t =
     match ctx with
     | Array ([], parent) -> Ctx (cache, TaggedArray (t, [], parent))
     | _ -> raise (Parse_error "Array tag, but not parsing an array")

    let pop_array (Ctx (cache, ctx)) =
      let rec pairup = function
        | [] -> []
        | k :: v :: rest -> (k, v) :: (pairup rest)
        | _ -> raise (Parse_error "Odd number of elements in cmap")
      in
       match ctx with
       | Array (res, parent) -> add (Ctx (cache, parent)) (`Array (List.rev res))
       | TaggedArray (List, [`Array res], parent) -> add (Ctx (cache, parent)) (`List res)
       | TaggedArray (List, _, _) -> raise (Parse_error "Wrong ~#list encoding")
       | TaggedArray (Set, [`Array res], parent) ->
           add (Ctx (cache, parent)) (`Set (Set.Poly.of_list res))
       | TaggedArray (Set, _, _) -> raise (Parse_error "Wrong ~#set encoding")
       | TaggedArray (Quote, [res], parent) -> add (Ctx (cache, parent)) res
       | TaggedArray (Quote, _, _) -> raise (Parse_error "Quote with multi-elem array")
       | TaggedArray (CMap, [`Array res], parent) ->
           add (Ctx (cache, parent)) (`Map (Map.Poly.of_alist_exn (pairup res)))
       | MapValue (_, _, _) -> raise (Parse_error "Odd number of k/v pairs in map-as-array/cmap")
       | MapKey (res, parent) -> add (Ctx (cache, parent)) (`Map (Map.Poly.of_alist_exn res))
       | Empty -> raise (Parse_error "end_of_array called in Empty context")
       | Focused _ -> raise (Parse_error "end_of_array called in Focused context")

    let cache_lookup ( (Ctx (cache, _)) as cx ) s =
      match CacheCode.to_int s |> Cache.find_exn cache with
      | Cache.ETransit x -> add cx x
      | Cache.ETag t -> push_tagged_array cx t
      
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
    | 'n' -> `BigInt (Big_int.big_int_of_string s)
    | 'd' -> `Float (Float.of_string s)
    | 'u' -> (`UUID (Uuid.of_string s))
    | 'r' -> (`URI s)
    | ':' -> (`Keyword s)
    | '$' -> (`Symbol s)
    | 'm' ->
      let f = Big_int.float_of_big_int (Big_int.big_int_of_string s) in
      (`Date (Time.of_float (f /. 1000.0)))
    | t -> `Tagged (t, `String s)


  (* Decode and check if the string is cacheable *)
  let decode_string s ctx head =
    let track s x =
      if String.length s > 3
      then Context.add (Context.track_transit ctx x) x
      else Context.add ctx x in
    match head with
    | ('^', ' ') -> Context.push_map ctx
    | ('^', _) -> Context.cache_lookup ctx (String.drop_prefix s 1)
    | ('~', '~') -> `String (String.drop_prefix s 1) |> Context.add ctx
    | ('~', '^') -> `String (String.drop_prefix s 1) |> Context.add ctx
    | ('~', '#') ->
      let array_tag = Context.tag_of_string s in
      let ctx' =
        if String.length s > 3
        then Context.track_tag ctx array_tag
        else ctx
      in
      Context.push_tagged_array ctx' array_tag
    | ('~', t) ->
       (match decode_tagged (String.drop_prefix s 2) t with
       | `Symbol s -> track s (`Symbol s)
       | `Keyword k -> track s (`Keyword k)
       | value -> Context.add ctx value)
    | _ -> Context.add ctx (`String s)

  (* Parser functions *)
  exception Todo
  let on_null ctx = Context.add ctx `Null
  let on_bool ctx b = Context.add ctx (`Bool b)
  let on_int ctx i = Context.add ctx (`Int i)
  let on_float ctx f = Context.add ctx (`Float f)

  let on_string ctx buf offset len =
    let str = String.sub buf offset len in
    match String.length str with
    | 0 | 1 -> Context.add ctx (`String str)
    | _ -> decode_string str ctx (str.[0], str.[1])

  let on_start_map _ = raise Todo

  let on_map_key _ _ _ _ = raise Todo

  let on_end_map _ = raise Todo

  module JSON = struct
    let callbacks = {
      YAJL.on_null = on_null;
      on_bool = on_bool;
      on_number = `Parse_numbers ((`Int64 on_int), on_float);
      on_string = on_string;
      on_start_map = on_start_map;
      on_map_key = on_map_key;
      on_end_map = on_end_map;
      on_start_array = Context.push_array;
      on_end_array = Context.pop_array;
    }

    let from_string str =
      let p = YAJL.make_parser callbacks Context.empty in
      let () = YAJL.parse p str in
      YAJL.complete_parse p |> Context.result
  end
end

type t = T.t
let from_string = Parser.JSON.from_string

let sexp_of_t = T.sexp_of_t
let t_of_sexp = T.t_of_sexp
