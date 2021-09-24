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


  (* This won't be needed *)

  let dec_err = Utf_uchar.utf_decode_error
  let[@inline] dec_ret n u = Utf_uchar.utf_decode n (Uchar.unsafe_of_int u)
  external length : bytes -> int = "%bytes_length"
  type t = Bytes.t
  external swap16 : int -> int = "%bswap16" (* That one will be in Bytes *)

  (* That will be needed *)

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
        match get b (i + 2) with
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
        if last > max then 0 else (set b i u; 2)
    | u ->
        let last = i + 3 in
        if last > max then 0 else
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
          match get b (i + 2) with
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
        match get b (i + 2) with
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
        if last > max then 0 else (set b i u; 2)
    | u ->
        let last = i + 3 in
        if last > max then 0 else
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
          match get b (i + 2) with
          | u when u < 0xDC00 || u > 0xDFFF -> false
          | lo -> loop max b (i + 4)
    in
    loop (length b - 1) b 0
