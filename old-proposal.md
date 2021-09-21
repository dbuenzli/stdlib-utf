# Adding UTF-X decoding support to the stdlib 

Since 4.06 we have UTF-X encoding support via the `Buffer` module. The
following is a proposal to add UTF-X *decoding* (and `bytes` encoding)
support with the following goals:

1. Provide a low-level, allocation-free codec API in `bytes`. This API
   provides all the information needed to implement loops for making
   higher-level UTF-X codec APIs (e.g. `Uutf`'s [folding](http://erratique.ch/software/uutf/doc/Uutf.String.html#1_Stringfolders) functions) 
   operating on `bytes` and `string` values. It's not geared towards the end-user 
   but rather towards library programmers.
   
2. Provide a less performant and less precise (no way to detect
   decoding errors; `Uchar.rep` replacement) decoding convenience API
   based on the `Uchar.t Seq.t` datatype for both `Bytes` and `String`.

## High-level `Seq` based API (`Bytes` and `String`)

In both `Bytes` and `String` we add the following high-level interface
(implemented via the low-level interface):

```ocaml
val utf_8_to_uchar_seq : ?start:int -> ?len:int -> t -> Uchar.t Seq.t
val utf_16le_to_uchar_seq : ?start:int -> ?len:int -> t -> Uchar.t Seq.t
val utf_16be_to_uchar_seq : ?start:int -> ?len:int -> t -> Uchar.t Seq.t
```

These are "best-effort" functions: any decoding error is substituted by
a `Uchar.rep` character until a new synchronizing byte is found.

The `utf_{8,16le,16be}_of_uchar_seq` functions would be a bit difficult to
provide due to dependencies (if implemented via `Buffer`), so
we leave them out.

We could add `Buffer.add_utf_{8,16le,16,be}_uchar_seq` but that's just
a `Seq.iter` away with the existing `Buffer.add_utf_{8,16le,16be}_uchar`.
So we stop here w.r.t. high-level API.

## Low-level `Bytes` API 

We begin with the following additions to the `Uchar` module: 

```ocaml
val utf_8_byte_length : Uchar.t -> int
val utf_16_byte_length : Uchar.t -> int
```

In the `Bytes` module we then add the following:

```ocaml
exception Utf_error of int
(** The control flow exception raised by
    [{get,set}_uchar_utf_{8,16le,16be}] functions. The integer is an
    index where a new decode/encode can be tried or the length of the
    buffer if the end was reached. {b Important.} This exception is used
    for control flow you must not let it uncaught, it is raised with notrace. *)

val get_uchar_utf_8_length : t -> int -> int
(** [get_uchar_utf_8_length b i] is the byte length an UTF-8 encoded
    character at index [i] in [b]. This is [0] if the byte is not a valid
    UTF-8 starter byte. *)

val get_uchar_utf_8 : t -> int -> Uchar.t
(** [get_uchar_utf_8 b i] is the UTF-8 encoded Unicode character at index
    [i] in [b]. The number of bytes consumed by the decode can be
    determined by [get_uchar_utf_8_length b i].  In case of errors
    @raise Utf_error [n] with [n] the next index were a new decode
    can be tried or [length b] if there is no such index. *)

val get_uchar_utf_16le : t -> int -> Uchar.t
(** [get_uchar_utf_16le b i] is the UTF-16LE encoded Unicode character at
    index [i] in [b]. The number of bytes consumed by the decode can
    be determined by using {!Uchar.utf_16_byte_length} on the
    result. @raise Utf_error [n] with [n] the next index were a new
    decode can be tried or [length b] if there is no such index. *)

val get_uchar_utf_16be : t -> int -> Uchar.t
(** [get_uchar_utf_16be] is like {!get_uchar_utf16_le} but decodes
    UTF-16BE. *)

val set_uchar_utf_8 : t -> int -> Uchar.t -> int
(** [set_uchar_utf_8 b i u] sets the UTF-8 encoding of [u] in [b]
    starting at [i] and returns the next index were an encoding can
    be performed or the buffer length if there is no such
    index. @raise Utf_error [n] if there's not enough space for the
    encode. *)

val set_utf_16le : t -> int -> Uchar.t -> int
(** [set_utf_16le] is like {!set_uchar_utf_8} but UTF-16LE encodes. *)

val set_utf_16be : t -> int -> Uchar.t -> int
(** [set_utf_16be] is like {!set_uchar_utf_8} but UTF-16BE encodes. *)
```

For codecing sequences of `Uchar.t` as loops:

1. The `set` functions are good.
2. The `get` functions incur a bit of overhead to compute the next index (the invoked function has the information but we can't return it without allocating). For UTF-8 either we either get the next index via `get_uchar_utf_8_length` (assuming a table-based implementation: two memory accesses) or with `Uchar.utf_8_byte_length` on the result (up to three branches). For UTF-16 we use `Uchar.utf_16_byte_length` (one branch).
