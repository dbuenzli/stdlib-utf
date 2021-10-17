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

(* Uchar additions *)

module type Uchar = sig
  include module type of Uchar

  (** {1:utf_codec UTF codecs tools}

      @since 4.14 *)

  type utf_decode [@@immediate]
  (** The type for UTF decode results. Values of this type represent
      the result of a Unicode Transformation Format decoding attempt. *)

  val utf_decode_is_valid : utf_decode -> bool
  (** [utf_decode_is_valid d] is [true] if and only if [d] holds a valid
      decode. *)

  val utf_decode_uchar : utf_decode -> Uchar.t
  (** [utf_x_decode_uchar d] is the Unicode character decoded by [d] if
      [utf_x_decode_is_valid d] is [true] and {!Uchar.rep} otherwise. *)

  val utf_decode_length : utf_decode -> int
  (** [utf_decode_length d] is the number of elements from the source
      that were consumed by the decode [d]. This is always strictly
      positive and smaller or equal to [4]. The kind of source elements
      depends on the actual decoder; for the decoders of the standard
      library this function always returns a length in bytes. *)

  val utf_decode : int -> Uchar.t -> utf_decode
  (** [utf_decode n u] is a valid UTF decode for [u] that consumed [n]
      elements from the source for decoding. [n] must be positive and
      smaller or equal to [4] (this is not checked by the module). *)

  val utf_decode_invalid : int -> utf_decode
  (** [utf_decode_invalid n] is an invalid UTF decode that consumed [n]
      elements from the source to error. [n] must be positive and
      smaller or equal to [4] (this is not checked by the module). The
      resulting decode has {!rep} as the decoded Unicode character. *)

  val utf_8_byte_length : t -> int
  (** [utf_8_byte_length u] is the number of bytes needed to encode
      [u] in UTF-8. *)

  val utf_16_byte_length : t -> int
  (** [utf_16_byte_length u] is the number of bytes needed to encode
      [u] in UTF-16. *)
end

(* Bytes additions *)

module type S = sig
module Uchar : Uchar
module Bytes : sig
  include module type of Bytes

  (** {1:utf_x_codecs UTF-X codecs and validations}

      @since 4.14 *)

  (** {2:utf_8 UTF-8} *)

  val get_utf_8_uchar : t -> int -> Uchar.utf_decode
  (** [get_utf_8_uchar b i] decodes an UTF-8 character at index [i] in
      [b]. *)

  val set_utf_8_uchar : t -> int -> Uchar.t -> int
  (** [set_utf_8_uchar b i u] UTF-8 encodes [u] at index [i] in [b]
      and returns the number of bytes [n] that were written starting
      at [i]. If [n] is [0] there was not enough space to encode [u]
      at [i] and [b] was left untouched. Otherwise a new character can
      be encoded at [i + n]. *)

  val is_valid_utf_8 : t -> bool
  (** [is_valid_utf_8 b] is [true] if and only if [b] contains valid
      UTF-8 data. *)

  (** {2:utf_16be UTF-16BE} *)

  val get_utf_16be_uchar : t -> int -> Uchar.utf_decode
  (** [get_utf_16be_uchar b i] decodes an UTF-16BE character at index
      [i] in [b]. *)

  val set_utf_16be_uchar : t -> int -> Uchar.t -> int
  (** [set_utf_16be_uchar b i u] UTF-16BE encodes [u] at index [i] in [b]
      and returns the number of bytes [n] that were written starting
      at [i]. If [n] is [0] there was not enough space to encode [u]
      at [i] and [b] was left untouched. Otherwise a new character can
      be encoded at [i + n]. *)

  val is_valid_utf_16be : t -> bool
  (** [is_valid_utf_16be b] is [true] if and only if [b] contains valid
      UTF-16BE data. *)

  (** {2:utf_16le UTF-16LE} *)

  val get_utf_16le_uchar : t -> int -> Uchar.utf_decode
  (** [get_utf_16le_uchar b i] decodes an UTF-16LE character at index
      [i] in [b]. *)

  val set_utf_16le_uchar : t -> int -> Uchar.t -> int
  (** [set_utf_16le_uchar b i u] UTF-16LE encodes [u] at index [i] in [b]
      and returns the number of bytes [n] that were written starting
      at [i]. If [n] is [0] there was not enough space to encode [u]
      at [i] and [b] was left untouched. Otherwise a new character can
      be encoded at [i + n]. *)

  val is_valid_utf_16le : t -> bool
  (** [is_valid_utf_16le b] is [true] if and only if [b] contains valid
      UTF-16LE data. *)
end
end
