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

open Utf_x_pat

(* Assumes the good encoding and decodes have been checked by test.ml
   Exhaustively tests all 1-4 bytes invalid sequences for decodes. This
   ensures we do not decode invalid sequence to uchars. *)

let fold_uchars f acc =
  let rec loop f acc u =
    let acc = f acc u in
    if Uchar.equal u Uchar.max then acc else loop f acc (Uchar.succ u)
  in
  loop f acc Uchar.min

module Sset = Set.Make (String)

let utf_8_encs, utf_16be_encs, utf_16le_encs =
  Printf.printf "Building encoding sequence sets\n%!";
  let add (set8, set16be, set16le) u =
    let s = Bytes.unsafe_to_string in
    let e8 = Bytes.create (Uchar.utf_8_byte_length u) in
    let e16be = Bytes.create (Uchar.utf_16_byte_length u) in
    let e16le = Bytes.create (Uchar.utf_16_byte_length u) in
    ignore (Bytes.set_utf_8_uchar e8 0 u);
    ignore (Bytes.set_utf_16be_uchar e16be 0 u);
    ignore (Bytes.set_utf_16le_uchar e16le 0 u);
    Sset.add (s e8) set8, Sset.add (s e16be) set16be, Sset.add (s e16le) set16le
  in
  fold_uchars add (Sset.empty, Sset.empty, Sset.empty)

let test_seqs utf utf_encs get_utf_char =
  let test seq =
    let dec = get_utf_char seq 0 in
    let valid = Uchar.utf_decode_is_valid dec in
    let is_enc = Sset.mem (Bytes.unsafe_to_string seq) utf_encs in
    if not ((valid && is_enc) || (not valid && not is_enc)) then begin
      for i = 0 to Bytes.length seq - 1 do
        Printf.printf "%02X " (Bytes.get_uint8 seq i);
      done;
      Printf.printf "valid: %b is_encoding: %b decode: U+%04X\n" valid is_enc
        (Uchar.to_int (Uchar.utf_decode_uchar dec));
      assert false
    end;
    valid
  in
  let[@inline] set buf i b = Bytes.unsafe_set buf i (Char.unsafe_chr b) in
  let s1 = Bytes.create 1 and s2 = Bytes.create 2
  and s3 = Bytes.create 3 and s4 = Bytes.create 4 in
  Printf.printf "Testing %s invalid decodes…\n%!" utf;
  for b0 = 0x00 to 0xFF do
    set s1 0 b0;
    if test s1 then ((* this prefix decoded, stop here *)) else begin
      set s2 0 b0;
      for b1 = 0x00 to 0xFF do
        set s2 1 b1;
	      if test s2 then ((* this prefix decoded, stop here *)) else begin
         set s3 0 b0;
         set s3 1 b1;
	       for b2 = 0x00 to 0xFF do
          set s3 2 b2;
	        if test s3 then ((* this prefix decoded, stop here *)) else begin
           set s4 0 b0;
           set s4 1 b1;
           set s4 2 b2;
	         for b3 = 0x00 to 0xFF do set s4 3 b3; ignore (test s4) done;
	       end
	      done;
	     end
      done;
    end
  done

let () =
  test_seqs "UTF-8" utf_8_encs Bytes.get_utf_8_uchar;
  test_seqs "UTF-16BE" utf_16be_encs Bytes.get_utf_16be_uchar;
  test_seqs "UTF-16LE" utf_16le_encs Bytes.get_utf_16le_uchar;
  ()

let () = Printf.printf "All tests passed!\n"
