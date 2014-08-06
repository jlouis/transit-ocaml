open Core.Std

type t =
  [ | `Null
    | `String of string
    | `Bool of Bool.t
    | `Int of Int64.t
    | `Float of Float.t
    | `Array of t list
    | `Map of (t, t) Map.Poly.t
    | `Keyword of string
    | `Symbol of string
    | `Date of Time.t
    | `UUID of Uuid.t
    | `URI of string
    | `List of t list
    | `Set of t Set.Poly.t
 ]

val from_string : string -> t

include Sexpable with type t := t