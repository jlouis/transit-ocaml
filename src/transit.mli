open Core.Std

type t =
  [ | `Null
    | `String of string
    | `Bool of Bool.t
    | `Int of Int64.t
    | `Float of Float.t
    | `Array of t list
    | `Map of (t * t) list
    | `Keyword of string
    | `Symbol of string
    | `Date of Time.t
    | `UUID of Uuid.t
    | `URI of string
    | `List of t list
    | `Set of t list
 ]

val from_string : string -> t

val to_string : t -> string
