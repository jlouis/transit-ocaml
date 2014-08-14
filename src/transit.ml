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
      | `Bytes of string
      | `Array of t list
      | `Map of (t, t) Map.Poly.t
      | `UUID of Uuid.t
      | `Keyword of String.t
      | `Symbol of String.t
      | `Time of Time.t
      | `URI of String.t
      | `List of t list
      | `Set of t Set.Poly.t
      | `Extension of string * t ] with sexp, compare
end

(* Cache codes in transit follow a radix 44 encoding. Transform these into integers
 * internally by means of helper functions. There are deliberately no CacheCode.t internal
 * representation
 *)
module CacheCode = struct
  exception Invalid_cache_code of string

  let base_char_index = 48
  let cache_code_digits = 44
  let max_count = 44 * 44 - 1

  let to_int s =
    match String.length s with
    | 1 ->
      (Char.to_int s.[0]) - base_char_index
    | 2 ->
      ((Char.to_int s.[0]) - base_char_index) * cache_code_digits +
      ((Char.to_int s.[1]) - base_char_index)
    | _ -> raise (Invalid_cache_code s)

  let of_int i =
    let hi = i / cache_code_digits in
    let lo = i % cache_code_digits in
    if i > max_count
    then raise (Invalid_cache_code "Integer is larger than 1936")
    else if hi = 0
         then "^" ^ String.of_char (Char.of_int_exn (lo + base_char_index))
         else "^" ^ String.of_char (Char.of_int_exn (hi + base_char_index))
                  ^ String.of_char (Char.of_int_exn (lo + base_char_index))

end

(* Code for parsing transit structures *)
module Reader = struct

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
      | s -> Unknown (String.drop_prefix s 2)

    (* Implementation via a map over integers *)
    module Transit_cache = struct
      type entry = ETransit of T.t
                 | ETag of tag

      type t = { m : entry Int.Map.t;
         		 c : int }
    
      let empty =
        { m = Int.Map.empty; c = 0 }
    
      let track { m; c } s =
        let m' = Int.Map.add m
            ~key:c
            ~data:s
        in
        if c > CacheCode.max_count then empty else { m = m'; c = c + 1 }
    
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
      | MapKey of (T.t * T.t) list * context
      | MapValue of T.t * (T.t * T.t) list * context
      | Tagged of tag * context
    with sexp

    type t = Cache.t * context

    let empty = (Cache.empty, Empty)
    let result (_, ctx) =
      match ctx with
      | Focused v -> v
      | _ -> raise Internal_error

      let track_transit (cache, ctx) s x =
        if String.length s > 3
        then (Cache.track_transit cache x, ctx)
        else (cache, ctx)

    let track_tag (cache, ctx) s x =
	   if String.length s > 3
	   then (Cache.track_tag cache x, ctx)
	   else (cache, ctx)

    (* Push the element "e" onto the Context, depending on what it looks like. Essentially it "does the right thing™"  *)
    let rec add (cache, ctx) e =
      match ctx with
      | Empty -> (cache, Focused e)
      | Focused _ -> raise Internal_error
      | Array (es, ctx) -> (cache, Array (e :: es, ctx))
      | Tagged (t, inner) ->
          let (cache', inner') = add (cache, inner) e in
          (cache', Tagged (t, inner'))
      | MapKey (es, ctx) ->
          (match e with
           | `String s when String.length s > 3 ->
               (Cache.track_transit cache e, MapValue (e, es, ctx))
           | _ ->
               (cache, MapValue (e, es, ctx)) )
      | MapValue (k, es, ctx) -> (cache, MapKey ((k, e) :: es, ctx))
    
    let push_map_as_array (cache, ctx) =
      match ctx with
      | Array ([], parent) -> (cache, MapKey ([], parent))
      | _ -> raise (Parse_error "Map-as-array marker in wrong location!")

    let push_array (cache, ctx) = (cache, Array ([], ctx))
    let push_map (cache, ctx) = (cache, MapKey ([], ctx))
    
    let push_tagged (cache, ctx) t =
     match ctx with
     | Array ([], _) as cx -> (cache, Tagged (t, cx))
     | MapKey ([], parent) -> (cache, Tagged (t, MapValue (`Null, [], parent)))
     | _ -> raise (Parse_error "Array tag, but not parsing an array")

    let rec pairup = function
      | [] -> []
      | k :: v :: rest -> (k, v) :: (pairup rest)
      | _ -> raise (Parse_error "Odd number of pairs")

    let pop_map (cache, ctx) =
      match ctx with
      | MapKey (res, parent) -> add (cache, parent) (`Map (Map.Poly.of_alist_exn res))
      | Tagged (Quote, MapKey ([_, res], parent)) -> add (cache, parent) res
      | Tagged (Set, MapKey ([_, `Array res], parent)) -> add (cache, parent) (`Set (Set.Poly.of_list res))
      | Tagged (List, MapKey ([_, `Array res], parent)) -> add (cache, parent) (`List res)
      | Tagged (CMap, MapKey ([_, `Array res], parent)) ->
           add (cache, parent) (`Map (Map.Poly.of_alist_exn (pairup res)))
      | Tagged (Unknown t, MapKey ([_, res], parent)) -> add (cache, parent) (`Extension (t, res))
      | Tagged (_, _) -> raise Internal_error
      | MapValue (_, _, _) -> raise (Parse_error "end_of_map called with an “free” key")
      | Array (_, _) -> raise (Parse_error "end_of_map in array context")
      | Empty -> raise (Parse_error "end_of_map called in empty context")
      | Focused _ -> raise (Parse_error "end_of_map called in focused context")

    let pop_array (cache, ctx) =
       match ctx with
       | Array (res, parent) -> add (cache, parent) (`Array (List.rev res))
       | Tagged (List, Array ([`Array res], parent)) -> add (cache, parent) (`List res)
       | Tagged (List, Array ( _, _)) -> raise (Parse_error "Wrong ~#list encoding")
       | Tagged (Set, Array ([`Array res], parent)) -> add (cache, parent) (`Set (Set.Poly.of_list res))
       | Tagged (Set, Array ( _, _)) -> raise (Parse_error "Wrong ~#set encoding")
       | Tagged (Quote, Array ([res], parent)) -> add (cache, parent) res
       | Tagged (Quote, Array (_, _)) -> raise (Parse_error "Quote with multi-elem array")
       | Tagged (CMap, Array ([`Array res], parent)) ->
           add (cache, parent) (`Map (Map.Poly.of_alist_exn (pairup res)))
       | Tagged (Unknown t, Array ([res], parent)) -> add (cache, parent) (`Extension (t, res))
       | Tagged (_, _) -> raise Internal_error
       | MapValue (_, _, _) -> raise (Parse_error "Odd number of k/v pairs in map-as-array/cmap")
       | MapKey (res, parent) -> add (cache, parent) (`Map (Map.Poly.of_alist_exn res))
       | Empty -> raise (Parse_error "end_of_array called in Empty context")
       | Focused _ -> raise (Parse_error "end_of_array called in Focused context")

    let cache_lookup ( (cache, _) as cx ) s =
      match CacheCode.to_int s |> Cache.find_exn cache with
      | Cache.ETransit x -> add cx x
      | Cache.ETag t -> push_tagged cx t
      
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
    | 'n' ->
      let i = Big_int.big_int_of_string s
      in
      (try
         `Int (Big_int.int64_of_big_int i)
       with Failure "nativeint_of_big_int" ->
         `BigInt i)
    | 'd' -> `Float (Float.of_string s)
    | 'b' -> `Bytes (Base64.decode_string s)
    | 'u' -> (`UUID (Uuid.of_string s))
    | 'r' -> (`URI s)
    | ':' -> (`Keyword s)
    | '$' -> (`Symbol s)
    | 'm' ->
      let f = Big_int.float_of_big_int (Big_int.big_int_of_string s) in
      (`Time (Time.of_float (f /. 1000.0)))
    | 't' ->
      let tp = Time.of_string s in
      `Time tp
    | t -> `Extension (String.of_char t, `String s)


  (* Decode and check if the string is cacheable *)
  let decode_string s ctx head =
    let track x = Context.add (Context.track_transit ctx s x) x in
    match head with
    | ('^', ' ') -> Context.push_map_as_array ctx
    | ('^', _) -> Context.cache_lookup ctx (String.drop_prefix s 1)
    | ('~', '~') -> `String (String.drop_prefix s 1) |> Context.add ctx
    | ('~', '^') -> `String (String.drop_prefix s 1) |> Context.add ctx
    | ('~', '#') ->
      let array_tag = Context.tag_of_string s in
      Context.push_tagged (Context.track_tag ctx s array_tag) array_tag
    | ('~', t) ->
       (match decode_tagged (String.drop_prefix s 2) t with
       | `Symbol sy -> track (`Symbol sy)
       | `Keyword k -> track (`Keyword k)
       | value -> Context.add ctx value)
    | _ -> Context.add ctx (`String s)

  (* Parser functions *)
  let on_null ctx = Context.add ctx `Null
  let on_bool ctx b = Context.add ctx (`Bool b)
  let on_int ctx i = Context.add ctx (`Int i)
  let on_float ctx f = Context.add ctx (`Float f)

  let on_string ctx buf offset len =
    let str = String.sub buf ~pos:offset ~len:len in
    match String.length str with
    | 0 | 1 -> Context.add ctx (`String str)
    | _ -> decode_string str ctx (str.[0], str.[1])

  module JSON = struct
    let callbacks = {
      YAJL.on_null = on_null;
      on_bool = on_bool;
      on_number = `Parse_numbers ((`Int64 on_int), on_float);
      on_string = on_string;
      on_start_map = Context.push_map;
      on_map_key = on_string;
      on_end_map = Context.pop_map;
      on_start_array = Context.push_array;
      on_end_array = Context.pop_array;
    }

    let from_string str =
      let p = YAJL.make_parser callbacks Context.empty in
      let () = YAJL.parse p str in
      YAJL.complete_parse p |> Context.result
  end
end

module Writer = struct
    exception Todo

    module Cache = struct
      type t = { count : int;
                 cache : int String.Map.t }

      let empty = { count = 0; cache = String.Map.empty }
      let find { cache; _} = String.Map.find cache
      let track {cache; count} key =
        if String.length key > 3
        then if count > CacheCode.max_count
             then empty
             else { count = count + 1; cache = String.Map.add cache ~key ~data:count }
        else {cache; count}
    end

    (* Build a state monad over the context *)
    module CtxBase = struct
      type 'a t = (Cache.t * YAJL.gen -> 'a * Cache.t * YAJL.gen)
      let runState f init = f init

      let return x = fun (c, g) -> (x, c, g)

      let bind act1 fact2 s =
        let (iv, ic, ig) = runState act1 s in
        let act2 = fact2 iv in
        runState act2 (ic, ig)
        
      let map = `Define_using_bind
    end
    
    module Ctx = struct
      include Monad.Make(CtxBase)
      let runState f init = f init
      let get_gen (c, g) = (g, c, g)
      let get_cache (c, g) = (c, c, g)
      let put_cache c' (c, g) = ((), c', g)
    end

    let int_53_bit_upper = Int64.of_int 9007199254740992
    let int_53_bit_lower = Int64.of_int (-9007199254740992)
    
    let to_string t =
      let open Ctx.Monad_infix in
      let (>>) f1 f2 = f1 >>= (fun () -> f2) in
      let string x = Ctx.get_gen >>= (fun gen -> YAJL.gen_string gen x; Ctx.return ()) in
      let int i = Ctx.get_gen >>= (fun gen -> YAJL.gen_int64 gen i; Ctx.return ()) in
      let null = Ctx.get_gen >>= (fun gen -> YAJL.gen_null gen ; Ctx.return ()) in
      let bool b = Ctx.get_gen >>= (fun gen -> YAJL.gen_bool gen b; Ctx.return ()) in
      let float f = Ctx.get_gen >>= (fun gen -> YAJL.gen_float gen f; Ctx.return ()) in
      let start_array = Ctx.get_gen >>= (fun gen -> Ctx.return (YAJL.gen_start_array gen)) in
      let end_array = Ctx.get_gen >>= (fun gen -> Ctx.return (YAJL.gen_end_array gen)) in

      let track str =
        Ctx.get_cache >>= (fun c ->
          match Cache.find c str with
          | None ->
            let c' = Cache.track c str in
            Ctx.put_cache c' >> string str
          | Some i ->
            string (CacheCode.of_int i) ) in
            
      let is_composite_map m = false in
      let rec array_tagged tag x =
        start_array >>
        track tag >>
        write_json x ~string_key:false >>
        end_array
      and composite_map m =
        let f (key, data) =
          write_json key ~string_key:true >> write_json data ~string_key:false in
        let l = Map.Poly.to_alist m
        in
          start_array >>
          string "~#cmap" >>
          Ctx.all_ignore (List.map l ~f) >>
          end_array
      and simple_map m =
        let f (key, data) =
          write_json key ~string_key:true >> write_json data ~string_key:false in
        let l = Map.Poly.to_alist m
        in
          start_array >>
          string "^ " >>
          Ctx.all_ignore (List.map l ~f) >>
          end_array
      and write_json x ~string_key =
        let string = if string_key then track else string in
        match x with
        | `Null -> null
        | `Bool b -> bool b
        | `String "" -> string ""
        | `String s ->
          let str = match s.[0] with
                    | '~' -> "~" ^ s
                    | '^' -> "~" ^ s
                    | _ -> s in
          if string_key
          then track str
          else string str
        | `Int i ->
          if string_key
          then track ("~i" ^ Int64.to_string i)
          else if i < int_53_bit_upper && i >= int_53_bit_lower
               then int i
               else string ("~i" ^ Int64.to_string i)
        | `BigInt n -> string ("~n" ^ Big_int.string_of_big_int n)
        | `Keyword k -> track ("~:" ^ k)
        | `Symbol symb -> track ("~$" ^ symb)
        | `UUID uuid -> string ("~u" ^ Uuid.to_string uuid)
        | `URI s -> string ("~r" ^ s)
        | `Time t ->
            let i = (Time.to_float t) *. 1000.0 |> Float.to_int64 in
            string ("~m" ^ Int64.to_string i)
        | `Float f ->
          if string_key
          then track ("~d" ^ Float.to_string f)
          else float f
        | `Array ts ->
            begin
              start_array >>
              Ctx.all_ignore (List.map ts ~f:(fun x -> write_json x ~string_key:false)) >>
              end_array
            end
        | `List ts -> array_tagged "~#list" (`Array ts)
        | `Set s -> array_tagged "~#set" (`Array (Set.Poly.to_list s))
        | `Map m ->
            if is_composite_map m
            then composite_map m
            else simple_map m
        | `Extension (tag, x) ->
          start_array >>
          string ("~#" ^ tag) >>
          write_json x ~string_key:string_key >>
          end_array
        | _ -> raise Todo in
  
      let quote x =
         start_array >>
         string "~#'" >>
         write_json x ~string_key:false >>
         end_array in
  
      let write_json_toplevel =
        function
        | `Null -> quote `Null
        | `Bool b -> quote (`Bool b)
        | `String s -> quote (`String s)
        | `Float f -> quote (`Float f)
        | `Int i -> quote (`Int i)
        | `Time t -> quote (`Time t)
        | `Keyword kw -> quote (`Keyword kw)
        | `Symbol symb -> quote (`Symbol symb)
        | `URI u -> quote (`URI u)
        | `UUID uuid -> quote (`UUID uuid)
        | x -> write_json x ~string_key:false in
  
      let write x =
        let gen = YAJL.make_gen () in
        let ((), _, _) = Ctx.runState (write_json_toplevel x) (Cache.empty, gen) in
        let (buf, pos, len) = YAJL.gen_get_buf gen in
        let res = String.sub buf ~pos ~len in
        YAJL.gen_clear gen;
        res
      in
        write t
end

type t = T.t
let from_string = Reader.JSON.from_string
let to_string = Writer.to_string


let sexp_of_t = T.sexp_of_t
let t_of_sexp = T.t_of_sexp
