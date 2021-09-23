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

  external swap16 : int -> int = "%bswap16" (* That one will be in Bytes *)

  (* Unsafe internal additions *)

  external unsafe_set_uint8 : t -> int -> int -> unit = "%bytes_unsafe_set"
  external unsafe_get_uint8 : t -> int -> int = "%bytes_unsafe_get"

  external unsafe_get_uint16_ne : t -> int -> int = "%caml_bytes_get16u"
  external unsafe_set_uint16_ne : t -> int -> int -> unit = "%caml_bytes_set16u"

  let unsafe_get_uint16_le b i =
    if Sys.big_endian
    then swap16 (unsafe_get_uint16_ne b i)
    else unsafe_get_uint16_ne b i

  let unsafe_get_uint16_be b i =
    if Sys.big_endian
    then unsafe_get_uint16_ne b i
    else swap16 (unsafe_get_uint16_ne b i)

  let unsafe_set_uint16_le b i x =
    if Sys.big_endian
    then unsafe_set_uint16_ne b i (swap16 x)
    else unsafe_set_uint16_ne b i x

  let unsafe_set_uint16_be b i x =
    if Sys.big_endian
    then unsafe_set_uint16_ne b i x
    else unsafe_set_uint16_ne b i (swap16 x)

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

  let utf_8_decode_case =
    (* Case according to first byte. This corresponds to the the lines
       of The Unicode Standard Table 3.7 with lines E1..EC and EE..EF
       sharing the same case and unmentioned bytes mapped to 8.

       let dump_table () =
         let print_case c = Printf.printf "\\%03d" c in
         let cut i = if i mod 16 = 0 then (print_char '\\'; print_newline ()) in
         for i = 0x00 to 0x7F do (cut i; print_case 0) done;
         for i = 0x80 to 0xC1 do (cut i; print_case 8) done;
         for i = 0xC2 to 0xDF do (cut i; print_case 1) done;
         for i = 0xE0 to 0xE0 do (cut i; print_case 2) done;
         for i = 0xE1 to 0xEC do (cut i; print_case 3) done;
         for i = 0xED to 0xED do (cut i; print_case 4) done;
         for i = 0xEE to 0xEF do (cut i; print_case 3) done;
         for i = 0xF0 to 0xF0 do (cut i; print_case 5) done;
         for i = 0xF1 to 0xF3 do (cut i; print_case 6) done;
         for i = 0xF4 to 0xF4 do (cut i; print_case 7) done;
         for i = 0xF5 to 0xFF do (cut i; print_case 8) done;
         () *)
      "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
       \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
       \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
       \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
       \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
       \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
       \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
       \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
       \008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
       \008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
       \008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
       \008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
       \008\008\001\001\001\001\001\001\001\001\001\001\001\001\001\001\
       \001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\
       \002\003\003\003\003\003\003\003\003\003\003\003\003\004\003\003\
       \005\006\006\006\007\008\008\008\008\008\008\008\008\008\008\008"

  let utf_8_decode_case' = unsafe_of_string utf_8_decode_case

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
    let max = length b - 1 in
    match get utf_8_decode_case' b0 with
    | 0 (* 00..7F *) -> dec_ret 1 b0
    | 1 (* C2..DF *) ->
        let i = i + 1 in if i > max then dec_err 1 else
        let b1 = get b i in if not_in_x80_to_xBF b1 then dec_err 1 else
        dec_ret 2 (utf_8_uchar_2 b0 b1)
    | 2 (* E0 *) ->
        let i = i + 1 in if i > max then dec_err 1 else
        let b1 = get b i in if not_in_xA0_to_xBF b1 then dec_err 1 else
        let i = i + 1 in if i > max then dec_err 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_err 2 else
        dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
    | 3 -> (* E1..EC or EE..EF *)
        let i = i + 1 in if i > max then dec_err 1 else
        let b1 = get b i in if not_in_x80_to_xBF b1 then dec_err 1 else
        let i = i + 1 in if i > max then dec_err 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_err 2 else
        dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
    | 4 -> (* ED *)
        let i = i + 1 in if i > max then dec_err 1 else
        let b1 = get b i in if not_in_x80_to_x9F b1 then dec_err 1 else
        let i = i + 1 in if i > max then dec_err 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_err 2 else
        dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
    | 5 -> (* F0 *)
        let i = i + 1 in if i > max then dec_err 1 else
        let b1 = get b i in if not_in_x90_to_xBF b1 then dec_err 1 else
        let i = i + 1 in if i > max then dec_err 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_err 2 else
        let i = i + 1 in if i > max then dec_err 3 else
        let b3 = get b i in if not_in_x80_to_xBF b3 then dec_err 3 else
        dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
    | 6 -> (* F1..F3 *)
        let i = i + 1 in if i > max then dec_err 1 else
        let b1 = get b i in if not_in_x80_to_xBF b1 then dec_err 1 else
        let i = i + 1 in if i > max then dec_err 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_err 2 else
        let i = i + 1 in if i > max then dec_err 3 else
        let b3 = get b i in if not_in_x80_to_xBF b3 then dec_err 3 else
        dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
    | 7 -> (* F4 *)
        let i = i + 1 in if i > max then dec_err 1 else
        let b1 = get b i in if not_in_x80_to_x8F b1 then dec_err 1 else
        let i = i + 1 in if i > max then dec_err 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_err 2 else
        let i = i + 1 in if i > max then dec_err 3 else
        let b3 = get b i in if not_in_x80_to_xBF b3 then dec_err 3 else
        dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
    | 8 -> dec_err 1
    | _ -> assert false

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
        if last > max then - 2 else
        (set_uint8 b i (0xC0 lor (u lsr 6));
         set b last (0x80 lor (u land 0x3F));
         2)
    | u when u <= 0xFFFF ->
        let last = i + 2 in
        if last > max then -3 else
        (set_uint8 b i (0xE0 lor (u lsr 12));
         set b (i + 1) (0x80 lor ((u lsr 6) land 0x3F));
         set b last (0x80 lor (u land 0x3F));
         3)
    | u ->
        let last = i + 3 in
        if last > max then -4 else
        (set_uint8 b i (0xF0 lor (u lsr 18));
         set b (i + 1) (0x80 lor ((u lsr 12) land 0x3F));
         set b (i + 2) (0x80 lor ((u lsr 6) land 0x3F));
         set b last (0x80 lor (u land 0x3F));
         4)

  let is_valid_utf_8 b =
    let rec loop max b i =
      if i > max then true else
      let get = unsafe_get_uint8 in
      match get utf_8_decode_case' (get b i) with
      | 0 (* 00..7F *) -> loop max b (i + 1)
      | 1 (* C2..DF *) ->
          let last = i + 1 in
          last > max
          || not_in_x80_to_xBF (get b last)
          || loop max b (last + 1)
      | 2 (* E0 *) ->
          let last = i + 2 in
          last > max
          || not_in_xA0_to_xBF (get b (i + 1))
          || not_in_x80_to_xBF (get b last)
          || loop max b (last + 1)
      | 3 -> (* E1..EC or EE..EF *)
          let last = i + 2 in
          last > max
          || not_in_x80_to_xBF (get b (i + 1))
          || not_in_x80_to_xBF (get b last)
          || loop max b (last + 1)
      | 4 -> (* ED *)
          let last = i + 2 in
          last > max
          || not_in_x80_to_x9F (get b (i + 1))
          || not_in_x80_to_xBF (get b last)
          || loop max b (last + 1)
      | 5 -> (* F0 *)
          let last = i + 3 in
          last > max
          || not_in_x90_to_xBF (get b (i + 1))
          || not_in_x80_to_xBF (get b (i + 2))
          || not_in_x80_to_xBF (get b last)
          || loop max b (last + 1)
      | 6 -> (* F1..F3 *)
          let last = i + 3 in
          last > max
          || not_in_x80_to_xBF (get b (i + 1))
          || not_in_x80_to_xBF (get b (i + 2))
          || not_in_x80_to_xBF (get b last)
          || loop max b (last + 1)
      | 7 -> (* F4 *)
          let last = i + 3 in
          last > max
          || not_in_x80_to_x8F (get b (i + 1))
          || not_in_x80_to_xBF (get b (i + 2))
          || not_in_x80_to_xBF (get b last)
          || loop max b (last + 1)
      | 8 -> false
      | _ -> assert false
    in
    loop (length b - 1) b 0

  (* UTF-16BE *)

  let get_utf_16be_uchar b i =
    let get = unsafe_get_uint16_be in
    let max = length b - 1 in
    if i < 0 || i > max then invalid_arg "index out of bounds" else
    if i = max then dec_err 1 else
    match get b i with
    | u when u < 0xD800 || u > 0xDFFF -> dec_ret 2 u
    | u when u > 0xDBFF -> dec_err 2
    | hi -> (* combine [hi] with a low surrogate *)
        let last = i + 3 in
        if last > max then dec_err (max - i + 1) else
        match get b i with
        | u when u < 0xDC00 || u > 0xDFFF -> dec_err 2 (* retry here *)
        | lo ->
            let u = (((hi land 0x3FF) lsl 10) lor (lo land 0x3FF)) + 0x10000 in
            dec_ret 4 u

  let set_utf_16be_uchar b i u =
    let set = unsafe_set_uint16_be in
    let max = length b - 1 in
    if i < 0 || i > max then invalid_arg "index out of bounds" else
    match Uchar.to_int u with
    | u when u < 0 -> assert false
    | u when u <= 0xFFFF ->
        let last = i + 1 in
        if last > max then -2 else (set b i u; 2)
    | u ->
        let last = i + 3 in
        if last > max then -4 else
        let u' = u - 0x10000 in
        let hi = (0xD800 lor (u' lsr 10)) in
        let lo = (0xDC00 lor (u' land 0x3FF)) in
        set b i hi; set b (i + 2) lo; 4

  let is_valid_utf_16be b =
    let rec loop max b i =
      let get = unsafe_get_uint16_be in
      if i > max then true else
      if i = max then false else
      match get b i with
      | u when u < 0xD800 || u > 0xDFFF -> loop max b (i + 2)
      | u when u > 0xDBFF -> false
      | hi ->
          let last = i + 3 in
          if last > max then false else
          match get b i with
          | u when u < 0xDC00 || u > 0xDFFF -> false
          | lo -> loop max b (i + 4)
    in
    loop (length b - 1) b 0

  (* UTF-16LE *)

  let get_utf_16le_uchar b i =
    let get = unsafe_get_uint16_le in
    let max = length b - 1 in
    if i < 0 || i > max then invalid_arg "index out of bounds" else
    if i = max then dec_err 1 else
    match get b i with
    | u when u < 0xD800 || u > 0xDFFF -> dec_ret 2 u
    | u when u > 0xDBFF -> dec_err 2
    | hi -> (* combine [hi] with a low surrogate *)
        let last = i + 3 in
        if last > max then dec_err (max - i + 1) else
        match get b i with
        | u when u < 0xDC00 || u > 0xDFFF -> dec_err 2 (* retry here *)
        | lo ->
            let u = (((hi land 0x3FF) lsl 10) lor (lo land 0x3FF)) + 0x10000 in
            dec_ret 4 u

  let set_utf_16le_uchar b i u =
    let set = unsafe_set_uint16_le in
    let max = length b - 1 in
    if i < 0 || i > max then invalid_arg "index out of bounds" else
    match Uchar.to_int u with
    | u when u < 0 -> assert false
    | u when u <= 0xFFFF ->
        let last = i + 1 in
        if last > max then -2 else (set b i u; 2)
    | u ->
        let last = i + 3 in
        if last > max then -4 else
        let u' = u - 0x10000 in
        let hi = (0xD800 lor (u' lsr 10)) in
        let lo = (0xDC00 lor (u' land 0x3FF)) in
        set b i hi; set b (i + 2) lo; 4

  let is_valid_utf_16le b =
    let rec loop max b i =
      let get = unsafe_get_uint16_le in
      if i > max then true else
      if i = max then false else
      match get b i with
      | u when u < 0xD800 || u > 0xDFFF -> loop max b (i + 2)
      | u when u > 0xDBFF -> false
      | hi ->
          let last = i + 3 in
          if last > max then false else
          match get b i with
          | u when u < 0xDC00 || u > 0xDFFF -> false
          | lo -> loop max b (i + 4)
    in
    loop (length b - 1) b 0
end
