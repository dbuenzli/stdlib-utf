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

(* TODO what we will add to the compiler test suite. *)

open Utf_x_pat

let fold_uchars f acc =
  let rec loop f acc u =
    let acc = f acc u in
    if Uchar.equal u Uchar.max then acc else loop f acc (Uchar.succ u)
  in
  loop f acc Uchar.min

let utf_8_spec =
  (* UTF-8 byte sequences, cf. table 3.7 Unicode 14. *)
  [(0x0000,0x007F),     [|(0x00,0x7F)|];
   (0x0080,0x07FF),     [|(0xC2,0xDF); (0x80,0xBF)|];
   (0x0800,0x0FFF),     [|(0xE0,0xE0); (0xA0,0xBF); (0x80,0xBF)|];
   (0x1000,0xCFFF),     [|(0xE1,0xEC); (0x80,0xBF); (0x80,0xBF)|];
   (0xD000,0xD7FF),     [|(0xED,0xED); (0x80,0x9F); (0x80,0xBF)|];
   (0xE000,0xFFFF),     [|(0xEE,0xEF); (0x80,0xBF); (0x80,0xBF)|];
   (0x10000,0x3FFFF),   [|(0xF0,0xF0); (0x90,0xBF); (0x80,0xBF); (0x80,0xBF)|];
   (0x40000,0xFFFFF),   [|(0xF1,0xF3); (0x80,0xBF); (0x80,0xBF); (0x80,0xBF)|];
   (0x100000,0x10FFFF), [|(0xF4,0xF4); (0x80,0x8F); (0x80,0xBF); (0x80,0xBF)|]]

let utf_16be_spec =
  (* UTF-16BE byte sequences, derived from table 3.5 Unicode 14. *)
  [(0x0000,0xD7FF),    [|(0x00,0xD7); (0x00,0xFF)|];
   (0xE000,0xFFFF),    [|(0xE0,0xFF); (0x00,0xFF)|];
   (0x10000,0x10FFFF), [|(0xD8,0xDB); (0x00,0xFF); (0xDC,0xDF); (0x00,0xFF)|]]

let uchar_map_of_spec spec =
  (* array mapping Uchar.t as ints to byte sequences according to [spec]. *)
  let map = Array.make ((Uchar.to_int Uchar.max) + 1) Bytes.empty in
  let add_range ((umin, umax), bytes) =
    let len = Array.length bytes in
    let bmin i = if i < len then fst bytes.(i) else max_int in
    let bmax i = if i < len then snd bytes.(i) else min_int in
    let uchar = ref umin in
    let buf = Bytes.create len in
    let add len' = match len = len' with
    | false -> ()
    | true -> map.(!uchar) <- Bytes.copy buf; incr uchar
    in
    for b0 = bmin 0 to bmax 0 do Bytes.set_uint8 buf 0 b0;
      for b1 = bmin 1 to bmax 1 do Bytes.set_uint8 buf 1 b1;
        for b2 = bmin 2 to bmax 2 do Bytes.set_uint8 buf 2 b2;
          for b3 = bmin 3 to bmax 3 do Bytes.set_uint8 buf 3 b3; add 4 done;
          add 3;
        done;
        add 2;
      done;
      add 1;
    done;
    assert (!uchar - 1 = umax)
  in
  List.iter add_range spec;
  map

let uchar_map_get u map = map.(Uchar.to_int u)
let utf_8 = uchar_map_of_spec utf_8_spec
let utf_16be = uchar_map_of_spec utf_16be_spec
let utf_16le =
  let swap u b =
    let len = Bytes.length b in
    if len = 0 then () else
    for i = 0 to Bytes.length b / 2 - 1 do
      let j = i * 2 in
      Bytes.set_uint16_le b j (Bytes.get_uint16_be b j);
    done;
  in
  let map = Array.map Bytes.copy utf_16be in
  Array.iteri swap map; map

let test_utf utf utf_len get_utf set_utf utf_is_valid =
  (* Test codec and validation of each Uchar.t against the spec. *)
  let f () u =
    let utf_len = utf_len u in
    let buf = Bytes.create utf_len in
    assert (set_utf buf 0 u = utf_len);
    assert (Bytes.equal buf (uchar_map_get u utf));
    assert (Bytes.equal buf (uchar_map_get u utf));
    let dec = get_utf buf 0 in
    assert (Uchar.utf_decode_is_valid dec);
    assert (Uchar.utf_decode_used_bytes dec = utf_len);
    assert (Uchar.equal (Uchar.utf_decode_uchar dec) u);
    assert (utf_is_valid buf);
    ()
  in
  fold_uchars f ()

let () =
  test_utf utf_8 Uchar.utf_8_byte_length
    Bytes.get_utf_8_uchar Bytes.set_utf_8_uchar Bytes.is_valid_utf_8

let () =
  test_utf utf_16be Uchar.utf_16_byte_length
    Bytes.get_utf_16be_uchar Bytes.set_utf_16be_uchar Bytes.is_valid_utf_16be

let () =
  test_utf utf_16le Uchar.utf_16_byte_length
    Bytes.get_utf_16le_uchar Bytes.set_utf_16le_uchar Bytes.is_valid_utf_16le

let () = Printf.printf "All tests passed!\n"
