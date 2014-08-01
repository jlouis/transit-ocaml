open Core.Std

type t =
  [ (* Ground values, scalars and composites *)
    | `Null
    | `String of string
    | `Bool of Bool.t
    | `Int of Int64.t
    | `Float of Float.t
    | `Array of t list
    | `Map of (t * t) list
    (* Extensions *)
    | `Keyword of string
    | `Symbol of string
    | `Date of Time.t
 ]

val from_string : string -> t

val to_string : t -> string
