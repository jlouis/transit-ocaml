(*
 * Copyright (c) 2006-2009 Citrix Systems Inc.
 * Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Core.Std
       
(* taken from https://github.com/avsm/ocaml-cohttp/blob/master/cohttp/base64.ml *)

let code = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
let padding = '='

let of_char x = if x = padding then 0 else String.index_exn code x

let to_char x = code.[x]

let decode (x : Bytes.t) : Bytes.t =
  let words = Bytes.length x / 4 in
  let padding = 
    if Bytes.length x = 0 then 0 else (
      if x.[Bytes.length x - 2] = padding
      then 2 else (if x.[Bytes.length x - 1] = padding then 1 else 0)) in
  let output = Bytes.create (words * 3 - padding) in
  for i = 0 to words - 1 do
    let a = of_char x.[4 * i + 0]
    and b = of_char x.[4 * i + 1]
    and c = of_char x.[4 * i + 2]
    and d = of_char x.[4 * i + 3] in
    let n = (a lsl 18) lor (b lsl 12) lor (c lsl 6) lor d in
    let x = (n lsr 16) land 255
    and y = (n lsr 8) land 255
    and z = n land 255 in
    String.unsafe_set output (3 * i + 0) (char_of_int x);
    if i <> words - 1 || padding < 2 then
      String.unsafe_set output (3 * i + 1) (char_of_int y);
    if i <> words - 1 || padding < 1 then
      String.unsafe_set output (3 * i + 2) (char_of_int z);
  done;
  output

let decode_string = decode

let encode x = 
  let length = String.length x in
  let words = (length + 2) / 3 in (* rounded up *)
  let padding = if length mod 3 = 0 then 0 else 3 - (length mod 3) in
  let output = String.make (words * 4) '\000' in
  let get i = if i >= length then 0 else int_of_char x.[i] in
  let setv i pos x = String.unsafe_set output (4 * i + pos) (to_char x) in
  for i = 0 to words - 1 do
    let x = get (3 * i + 0)
    and y = get (3 * i + 1)
    and z = get (3 * i + 2) in
    let n = (x lsl 16) lor (y lsl 8) lor z in 
    let a = (n lsr 18) land 63
    and b = (n lsr 12) land 63
    and c = (n lsr 6) land 63
    and d = n land 63 in
    setv i 0 a;
    setv i 1 b;
    setv i 2 c;
    setv i 3 d;
  done;
  for i = 1 to padding do
    output.[String.length output - i] <- '=';
  done;
  output

let encode_string = encode

