open Core.Std

type t =
  [ | `Null
    | `String of string
    | `Bool of Bool.t
    | `Int of Int64.t
    | `Float of Float.t
    | `Bytes of string
    | `Array of t list
    | `Map of (t * t) list
    | `Keyword of string
    | `Symbol of string
    | `BigInt of Big_int.big_int ]

val from_string : string -> t

