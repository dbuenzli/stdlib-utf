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

include Uchar

(* UTF codecs tools *)

type utf_decode = int
(* This is an int [0xDUUUUUU] decomposed as follows:
   - [D] is four bits for decode information, the highest bit is set if the
     decode is valid. The three lower bits indicate the number of elements
     from the source that were consumed by the decode.
   - [UUUUUU] is the decoded Unicode character or the Unicode replacement
     character U+FFFD if for invalid decodes. *)

let valid_bit = 27
let decode_bits = 24

let[@inline] utf_decode_is_valid d = (d lsr valid_bit) = 1
let[@inline] utf_decode_length d = (d lsr decode_bits) land 0b111
let[@inline] utf_decode_uchar d = Uchar.unsafe_of_int (d land 0xFFFFFF)
let[@inline] utf_decode n u = ((8 lor n) lsl decode_bits) lor (Uchar.to_int u)
let[@inline] utf_decode_invalid n = (n lsl decode_bits) lor 0xFFFD (* rep *)

let utf_8_byte_length u = match to_int u with
| u when u < 0 -> assert false
| u when u <= 0x007F -> 1
| u when u <= 0x07FF -> 2
| u when u <= 0xFFFF -> 3
| u when u <= 0x10FFFF -> 4
| _ -> assert false

let utf_16_byte_length u = match to_int u with
| u when u < 0 -> assert false
| u when u <= 0xFFFF -> 2
| u when u <= 0x10FFFF -> 4
| _ -> assert false
