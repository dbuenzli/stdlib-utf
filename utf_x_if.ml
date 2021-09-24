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

  (* In case of decoding error, if we error on the first byte, we
     consume the byte, otherwise we consume the [n] bytes preceeding
     erroring byte.

     This means that if a client uses decodes without caring about
     validity it naturally replace bogus data with Uchar.rep according
     to the WHATWG Encoding standard. Other schemes are possible by
     consulting the number of used bytes on invalid decodes. For more
     details see https://hsivonen.fi/broken-utf-8/

     For this reason in [get_utf_8_uchar] we gradually check the next
     byte is available rather than doing it immediately after the
     first byte. Contrast with [is_valid_utf_8]. *)

  (* UTF-8 *)

  let[@inline] not_in_x80_to_xBF b = b lsr 6 <> 0b10
  let[@inline] not_in_xA0_to_xBF b = b lsr 5 <> 0b101
  let[@inline] not_in_x80_to_x9F b = b lsr 5 <> 0b100
  let[@inline] not_in_x90_to_xBF b = b < 0x90 || 0xBF < b
  let[@inline] not_in_x80_to_x8F b = b lsr 4 <> 0x8

  let[@inline] utf_8_uchar_2 b0 b1 =
    ((b0 land 0x1F) lsl 6) lor
    ((b1 land 0x3F))

  let[@inline] utf_8_uchar_3 b0 b1 b2 =
    ((b0 land 0x0F) lsl 12) lor
    ((b1 land 0x3F) lsl 6) lor
    ((b2 land 0x3F))

  let[@inline] utf_8_uchar_4 b0 b1 b2 b3 =
    ((b0 land 0x07) lsl 18) lor
    ((b1 land 0x3F) lsl 12) lor
    ((b2 land 0x3F) lsl 6) lor
    ((b3 land 0x3F))

  let get_utf_8_uchar b i =
    let b0 = get_uint8 b i in (* raises if [i] is not a valid index. *)
    let get = unsafe_get_uint8 in
    if b0 <= 0x7F then (* 00..7F *) dec_ret 1 b0 else
    if b0 land 0xE0 = 0xC0 && b0 >= 0xC2 then begin
      (* C2..DF *)
      let max = length b - 1 in
      let i = i + 1 in if i > max then dec_err 1 else
      let b1 = get b i in if not_in_x80_to_xBF b1 then dec_err 1 else
      dec_ret 2 (utf_8_uchar_2 b0 b1)
    end else if b0 land 0xF0 = 0xE0 then begin
      let max = length b - 1 in
      if b0 = 0xE0 then begin
        (* E0 *)
        let i = i + 1 in if i > max then dec_err 1 else
        let b1 = get b i in if not_in_xA0_to_xBF b1 then dec_err 1 else
        let i = i + 1 in if i > max then dec_err 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_err 2 else
        dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
      end else if b0 = 0xED then begin
        (* ED *)
        let i = i + 1 in if i > max then dec_err 1 else
        let b1 = get b i in if not_in_x80_to_x9F b1 then dec_err 1 else
        let i = i + 1 in if i > max then dec_err 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_err 2 else
        dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
      end else begin
        (* E1..EC or EE..EF *)
        let i = i + 1 in if i > max then dec_err 1 else
        let b1 = get b i in if not_in_x80_to_xBF b1 then dec_err 1 else
        let i = i + 1 in if i > max then dec_err 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_err 2 else
        dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
      end
    end else if b0 land 0xF8 = 0xF0 && b0 <= 0xF4 then begin
      let max = length b - 1 in
      if b0 = 0xF0 then begin
        (* F0 *)
        let i = i + 1 in if i > max then dec_err 1 else
        let b1 = get b i in if not_in_x90_to_xBF b1 then dec_err 1 else
        let i = i + 1 in if i > max then dec_err 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_err 2 else
        let i = i + 1 in if i > max then dec_err 3 else
        let b3 = get b i in if not_in_x80_to_xBF b3 then dec_err 3 else
        dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
      end else if b0 = 0xF4 then begin
        (* F4 *)
        let i = i + 1 in if i > max then dec_err 1 else
        let b1 = get b i in if not_in_x80_to_x8F b1 then dec_err 1 else
        let i = i + 1 in if i > max then dec_err 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_err 2 else
        let i = i + 1 in if i > max then dec_err 3 else
        let b3 = get b i in if not_in_x80_to_xBF b3 then dec_err 3 else
        dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
      end else begin
        let i = i + 1 in if i > max then dec_err 1 else
        let b1 = get b i in if not_in_x80_to_xBF b1 then dec_err 1 else
        let i = i + 1 in if i > max then dec_err 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_err 2 else
        let i = i + 1 in if i > max then dec_err 3 else
        let b3 = get b i in if not_in_x80_to_xBF b3 then dec_err 3 else
        dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
      end
    end else dec_err 1

  let set_utf_8_uchar b i u =
    let set = unsafe_set_uint8 in
    let max = length b - 1 in
    match Uchar.to_int u with
    | u when u < 0 -> assert false
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
    let rec loop max b i =
      if i > max then true else
      let get = unsafe_get_uint8 in
      let b0 = get b i in
      if b0 <= 0x7F then (* 00..7F *) loop max b (i + 1) else
      if b0 land 0xE0 = 0xC0 && b0 >= 0xC2 then begin
        (* C2..DF *)
        let last = i + 1 in
        last > max
        || not_in_x80_to_xBF (get b last)
        || loop max b (last + 1)
      end else if b0 land 0xF0 = 0xE0 then begin
        if b0 = 0xE0 then begin
          (* E0 *)
          let last = i + 2 in
          last > max
          || not_in_xA0_to_xBF (get b (i + 1))
          || not_in_x80_to_xBF (get b last)
          || loop max b (last + 1)
        end else if b0 = 0xED then begin
          let last = i + 2 in
          last > max
          || not_in_x80_to_x9F (get b (i + 1))
          || not_in_x80_to_xBF (get b last)
          || loop max b (last + 1)
        end else begin
          (* E1..EC or EE..EF *)
          let last = i + 2 in
          last > max
          || not_in_x80_to_xBF (get b (i + 1))
          || not_in_x80_to_xBF (get b last)
          || loop max b (last + 1)
        end
      end else if b0 land 0xF8 = 0xF0 && b0 <= 0xF4 then begin
        if b0 = 0xF0 then begin
          (* F0 *)
          let last = i + 3 in
          last > max
          || not_in_x90_to_xBF (get b (i + 1))
          || not_in_x80_to_xBF (get b (i + 2))
          || not_in_x80_to_xBF (get b last)
          || loop max b (last + 1)
        end else if b0 = 0xF4 then begin
          (* F4 *)
          let last = i + 3 in
          last > max
          || not_in_x80_to_x8F (get b (i + 1))
          || not_in_x80_to_xBF (get b (i + 2))
          || not_in_x80_to_xBF (get b last)
          || loop max b (last + 1)
        end else begin
          (* F1..F3 *)
          let last = i + 3 in
          last > max
          || not_in_x80_to_xBF (get b (i + 1))
          || not_in_x80_to_xBF (get b (i + 2))
          || not_in_x80_to_xBF (get b last)
          || loop max b (last + 1)
        end
      end else false
    in
    loop (length b - 1) b 0

  include Utf_16
end
