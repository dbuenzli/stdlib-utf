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

module type S = sig

module Uchar : sig
  include module type of Uchar

  val utf_8_byte_length : t -> int
  (** [utf_8_byte_length u] is the number of bytes needed to encode
      [u] in UTF-8.

      @since 4.XX *)

  val utf_16_byte_length : t -> int
  (** [utf_16_byte_length u] is the number of bytes needed to encode
      [u] in UTF-16.

      @since 4.XX *)
end

module Bytes : sig
  include module type of Bytes

  (** {1:utf_x_codecs UTF-X codecs and validations} *)

  type utf_x_decode
  (** The type for UTF-X decodes.

      @since 4.XX *)

  val utf_x_decode_valid : utf_x_decode -> bool
  (** [utf_x_decode_valid d] is [true] if and only if
      [d] holds a valid decode.

      @since 4.XX *)

  val utf_x_decode_used_bytes : utf_x_decode -> int
  (** [utf_x_decode_used_bytes d] is the number of bytes consumed by the
      decode [d]. This is always strictly positive and smaller or equal
      to 4.

      @since 4.XX *)

  val utf_x_decode_uchar : utf_x_decode -> Uchar.t
  (** [utf_x_decode_uchar d] is the Unicode character decoded by [d] if
      [utf_x_decode_valid d] is [true] and {!Uchar.rep} otherwise.

      @since 4.XX *)

  (** {2:utf_8 UTF-8} *)

  val get_utf_8_uchar : t -> int -> utf_x_decode
  (** [get_utf_8_uchar b i] decodes an UTF-8 character at index [i] in
      [b].

      @since 4.XX *)

  val set_utf_8_uchar : t -> int -> Uchar.t -> int
  (** [set_utf_8_uchar b i u] UTF-8 encodes [u] at index [i] in [b].
      If the result value [n] is:
      {ul
      {- [n > 0], [n] is the number of bytes that were used for encoding
         [u]. A new character can be encoded at [i + n].}
      {- [n < 0], [b] was left untouched because there was no space left
         at [i] to encode [u]. [-n] is the total number of bytes needed for
         encoding [u].}}

      @since 4.XX *)

  val is_valid_utf_8 : t -> bool
  (** [is_valid_utf_8 b] is [true] if and only if [b] contains valid
      UTF-8 data.

      @since 4.XX *)

  (** {2:utf_16be UTF-16BE} *)

  val get_utf_16be_uchar : t -> int -> utf_x_decode
  (** [get_utf_16be_uchar b i] decodes an UTF-16BE character at index
      [i] in [b].

      @since 4.XX *)

  val set_utf_16be_uchar : t -> int -> Uchar.t -> int
  (** [set_utf_16be_uchar b i u] UTF-16BE encodes [u] at index [i] in [b].
      If the result value [n] is:
      {ul
      {- [n > 0], [n] is the number of bytes that were used for encoding
         [u]. A new character can be encoded at [i + n].}
      {- [n < 0], [b] was left untouched because there was no space left
         at [i] to encode [u]. [-n] is the total number of bytes needed for
         encoding [u].}}

      @since 4.XX *)

  val is_valid_utf_16be : t -> bool
  (** [is_valid_utf_16be b] is [true] if and only if [b] contains valid
      UTF-16BE data.

      @since 4.XX *)

  (** {2:utf_16le UTF-16LE} *)

  val get_utf_16le_uchar : t -> int -> utf_x_decode
  (** [get_utf_16le_uchar b i] decodes an UTF-16LE character at index
      [i] in [b].

      @since 4.XX *)

  val set_utf_16le_uchar : t -> int -> Uchar.t -> int
  (** [set_utf_16le_uchar b i u] UTF-16LE encodes [u] at index [i] in [b].
      If the result value [n] is:
      {ul
      {- [n > 0], [n] is the number of bytes that were used for encoding
         [u]. A new character can be encoded at [i + n].}
      {- [n < 0], [b] was left untouched because there was no space left
         at [i] to encode [u]. [-n] is the total number of bytes needed for
         encoding [u].}}

      @since 4.XX *)

  val is_valid_utf_16le : t -> bool
  (** [is_valid_utf_16le b] is [true] if and only if [b] contains valid
      UTF-16LE data.

      @since 4.XX *)
end
end
