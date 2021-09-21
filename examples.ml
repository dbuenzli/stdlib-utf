(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Utf_x_adhoc

(* Uutf style folders
   https://erratique.ch/software/uutf/doc/Uutf.String.html *)

type utf_decode = [ `Malformed of string | `Uchar of Uchar.t ]
type 'a folder = 'a -> int -> utf_decode -> 'a

let fold_utf_8 : 'a folder -> 'a -> string -> 'a = fun f acc s ->
  let rec loop b i acc =
    if i >= Bytes.length b then acc else
    let dec = Bytes.get_utf_8_uchar b i in
    let used = Bytes.utf_x_decode_used_bytes dec in
    let i' = i + used in
    match Bytes.utf_x_decode_valid dec with
    | false -> loop b i' (f acc i (`Malformed (String.sub s i used)))
    | true -> loop b i' (f acc i (`Uchar (Bytes.utf_x_decode_uchar dec)))
  in
  loop (Bytes.unsafe_of_string s) 0 acc

(* Seq based iterators.

   Replace malformations by {!Uchar.rep} using the WHATWG Encoding
   standard, see https://hsivonen.fi/broken-utf-8/. It's nice that no
   particular logic has to be added for decode errors. *)

let utf_8_string_to_uchar_seq  : string -> Uchar.t Seq.t = fun s ->
  let rec uchar b i () =
    if i >= Bytes.length b then Seq.Nil else
    let dec = Bytes.get_utf_8_uchar b i in
    let u = Bytes.utf_x_decode_uchar dec in
    Seq.Cons (u, uchar b (i + Bytes.utf_x_decode_used_bytes dec))
  in
  uchar (Bytes.unsafe_of_string s) 0
