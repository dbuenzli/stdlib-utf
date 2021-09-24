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

module Uchar = Utf_uchar
module Bytes = struct
  include Bytes

  (* Unsafe internal additions *)

  external unsafe_set_uint8 : t -> int -> int -> unit = "%bytes_unsafe_set"
  external unsafe_get_uint8 : t -> int -> int = "%bytes_unsafe_get"

  (* UTF-X codecs and validations *)

  let dec_err = Uchar.utf_decode_error
  let[@inline] dec_ret n u = Uchar.utf_decode n (Uchar.unsafe_of_int u)

  (* UTF-8 *)

  let utf_8_dfa =
    (* The first 256 entries map bytes to their class. Then the
       DFA follows. The DFA automaton is MIT-licensed
       Copyright (c) 2008-2009 Bjoern Hoehrmann.
       See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/#variations*)
    unsafe_of_string
    "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
     \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
     \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
     \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
     \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
     \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
     \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
     \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
     \001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\
     \009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
     \007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
     \007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
     \008\008\002\002\002\002\002\002\002\002\002\002\002\002\002\002\
     \002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\
     \010\003\003\003\003\003\003\003\003\003\003\003\003\004\003\003\
     \011\006\006\006\005\008\008\008\008\008\008\008\008\008\008\008\
     \000\012\024\036\060\096\084\012\012\012\048\072\
     \012\012\012\012\012\012\012\012\012\012\012\012\
     \012\000\012\012\012\012\012\000\012\000\012\012\
     \012\024\012\012\012\012\012\024\012\024\012\012\
     \012\012\012\012\012\012\012\024\012\012\012\012\
     \012\024\012\012\012\012\012\012\012\024\012\012\
     \012\012\012\012\012\012\012\036\012\036\012\012\
     \012\036\012\012\012\012\012\036\012\036\012\012\
     \012\036\012\012\012\012\012\012\012\012\012\012"

  let accept = 0
  let reject = 12

(* Rewriting the following as an imperative loop is uglier but
   more efficient.

  let get_utf_8_uchar b i =
    let max = length b - 1 in
    if i < 0 || i > max then invalid_arg "index out of bounds" else
    let rec loop st u j =
      if j > max then dec_err (j - i) else
      let byte = unsafe_get_uint8 b j in
      let class' = unsafe_get_uint8 utf_8_dfa byte in
      let u' =
        if st <> accept
        then (u lsl 6) lor (byte land 0x3F)
        else byte land (0xFF lsr class')
      in
      let st' = unsafe_get_uint8 utf_8_dfa (256 + st + class') in
      if st' = reject then dec_err (j - i + 1) else
      if st' = accept then dec_ret (j - i + 1) u' else
      loop st' u' (j + 1)
    in
    loop accept 0 i
*)

  let get_utf_8_uchar b i =
    let max = length b - 1 in
    if i < 0 || i > max then invalid_arg "index out of bounds" else
    let stop = ref false in
    let st = ref accept in
    let j = ref i in
    let u = ref 0 in
    while (not !stop) do
      let byte = unsafe_get_uint8 b !j in
      let class' = unsafe_get_uint8 utf_8_dfa byte in
      u :=
        (if !st <> accept
         then (!u lsl 6) lor (byte land 0x3F)
         else byte land (0xFF lsr class'));
      st := unsafe_get_uint8 utf_8_dfa (256 + !st + class');
      if (!st = reject || !st = accept)
      then stop := true
      else (incr j; if !j > max then stop := true);
    done;
    if !j > max then dec_err (!j - i) else
    if !st = accept then dec_ret (!j - i + 1) !u else
    if !st = reject then dec_err (if i = !j then 1 else (!j - i)) else
    assert false

  let set_utf_8_uchar b i u =
    let set = unsafe_set_uint8 in
    let max = length b - 1 in
    match Uchar.to_int u with
    | u when u <= 0x007F ->
        set_uint8 b i u;
        1
    | u when u <= 0x07FF ->
        let last = i + 1 in
        if last > max then 0 else
        (set_uint8 b i (0xC0 lor (u lsr 6));
         set b last (0x80 lor (u land 0x3F));
         2)
    | u when u <= 0xFFFF ->
        let last = i + 2 in
        if last > max then 0 else
        (set_uint8 b i (0xE0 lor (u lsr 12));
         set b (i + 1) (0x80 lor ((u lsr 6) land 0x3F));
         set b last (0x80 lor (u land 0x3F));
         3)
    | u ->
        let last = i + 3 in
        if last > max then 0 else
        (set_uint8 b i (0xF0 lor (u lsr 18));
         set b (i + 1) (0x80 lor ((u lsr 12) land 0x3F));
         set b (i + 2) (0x80 lor ((u lsr 6) land 0x3F));
         set b last (0x80 lor (u land 0x3F));
         4)

  let is_valid_utf_8 b =
    (* This could be optimized using the state machine. We didn't
       do it for now since it seems the adhoc implementation yields
       better perfs. *)
    let rec loop b i =
      if i >= length b then true else
      let dec = get_utf_8_uchar b i in
      if Uchar.utf_decode_is_valid dec
      then loop b (i + Uchar.utf_decode_used_bytes dec)
      else false
    in
    loop b 0

  include Utf_16
end
