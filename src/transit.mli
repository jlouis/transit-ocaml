open Core.Std

type t =
  [ | `Null
    | `String of string
    | `Bool of Bool.t
    | `Int of Int64.t
    | `BigInt of Big_int.big_int
    | `Float of Float.t
    | `Bytes of string
    | `Array of t list
    | `Map of (t, t) Map.Poly.t
    | `Keyword of string
    | `Symbol of string
    | `Time of Time.t
    | `UUID of Uuid.t
    | `URI of string
    | `List of t list
    | `Set of t Set.Poly.t
    | `Extension of string * t
  ]

val from_string : string -> t
val to_string : t -> string

include Sexpable with type t := t
