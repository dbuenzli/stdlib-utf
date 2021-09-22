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

open Utf_x

(* Quickly ported over from uutf. *)

let u_nl = Uchar.of_int 0x000A
let log f = Format.printf (f ^^ "@?")
let strf = Format.sprintf
let fail fmt =
  let fail _ = failwith (Format.flush_str_formatter ()) in
  Format.kfprintf fail Format.str_formatter fmt

module Int = struct type t = int let compare : int -> int -> int = compare end
module Umap = Map.Make (Uchar)
module Bmap = Map.Make (Bytes)

(* Constructs from the specification, the map from uchars to their valid
   UTF-8 byte sequence and the map reverse map from valid UTF-8 byte sequences
   to their uchar.  *)
let utf8_maps () =
  log "Building UTF-8 codec maps from specification.\n";
  let spec = [        (* UTF-8 byte sequences cf. table 3.7 p. 94 Unicode 6. *)
    (0x0000,0x007F),     [|(0x00,0x7F)|];
    (0x0080,0x07FF),     [|(0xC2,0xDF); (0x80,0xBF)|];
    (0x0800,0x0FFF),     [|(0xE0,0xE0); (0xA0,0xBF); (0x80,0xBF)|];
    (0x1000,0xCFFF),     [|(0xE1,0xEC); (0x80,0xBF); (0x80,0xBF)|];
    (0xD000,0xD7FF),     [|(0xED,0xED); (0x80,0x9F); (0x80,0xBF)|];
    (0xE000,0xFFFF),     [|(0xEE,0xEF); (0x80,0xBF); (0x80,0xBF)|];
    (0x10000,0x3FFFF),   [|(0xF0,0xF0); (0x90,0xBF); (0x80,0xBF); (0x80,0xBF)|];
    (0x40000,0xFFFFF),   [|(0xF1,0xF3); (0x80,0xBF); (0x80,0xBF); (0x80,0xBF)|];
    (0x100000,0x10FFFF), [|(0xF4,0xF4); (0x80,0x8F); (0x80,0xBF); (0x80,0xBF)|]]
  in
  let add_range (umap, bmap) ((umin, umax), bytes) =
    let len = Array.length bytes in
    let bmin i = if i < len then fst bytes.(i) else max_int in
    let bmax i = if i < len then snd bytes.(i) else min_int in
    let umap = ref umap in
    let bmap = ref bmap in
    let uchar = ref umin in
    let buf = Bytes.create len in
    let add len' =
      if len <> len' then () else
      begin
        let bytes = Bytes.copy buf in
        let u = Uchar.of_int !uchar in
        umap := Umap.add u bytes !umap;
        bmap := Bmap.add bytes u !bmap;
        incr uchar;
      end
    in
    for b0 = bmin 0 to bmax 0 do
      Bytes.unsafe_set buf 0 (Char.chr b0);
      for b1 = bmin 1 to bmax 1 do
        Bytes.unsafe_set buf 1 (Char.chr b1);
        for b2 = bmin 2 to bmax 2 do
          Bytes.unsafe_set buf 2 (Char.chr b2);
          for b3 = bmin 3 to bmax 3 do
            Bytes.unsafe_set buf 3 (Char.chr b3);
            add 4;
          done;
          add 3;
        done;
        add 2;
      done;
      add 1;
    done;
    assert (!uchar - 1 = umax);
    (!umap, !bmap)
  in
  List.fold_left add_range (Umap.empty, Bmap.empty)  spec

let utf8_encode_test umap = (* This tests Buffer.add_utf_8_uchar *)
  log "Testing UTF-8 encoding of every unicode scalar value against spec.\n";
  let buf = Buffer.create 4 in
  let test u =
    let u = Uchar.unsafe_of_int u in
    let bytes = try Umap.find u umap with Not_found -> assert false in
    let bytes = Bytes.unsafe_to_string bytes in
    Buffer.clear buf; Buffer.add_utf_8_uchar buf u;
    if String.equal bytes (Buffer.contents buf) then () else
    fail "UTF-8 encoding error (U+%04X)" (Uchar.to_int u)
  in
  for i = 0x0000 to 0xD7FF do test i done;
  for i = 0xE000 to 0x10FFFF do test i done

let utf8_decode_test (module Utf_x : Utf_x.S) bmap =
  log "Testing the UTF-8 decoding of all <= 4 bytes sequences (be patient).\n";
  let open Utf_x in
  let spec seq =
    try `Uchar (Bmap.find seq bmap) with Not_found -> `Malformed seq
  in
  let test seq =
    let dec = Bytes.get_utf_8_uchar seq 0 in
    let valid = Bytes.utf_x_decode_valid dec in
    let u = Bytes.utf_x_decode_uchar dec in
    match spec seq, (valid, u) with
    | `Uchar spec, (true, u) when Uchar.equal u spec -> `Decoded
    | `Malformed _, (false, u) when Uchar.equal u Uchar.rep -> `Malformed
    | spec, _ ->
        let spec = match spec with
        | `Uchar u -> Printf.sprintf "U+%04d" (Uchar.to_int u)
        | `Malformed _ -> "malformed"
        in
        let u = Uchar.to_int u in
        let seq = Bytes.unsafe_to_string seq in
        fail "Failure on sequence %s, spec:%S decode: U+%04d, valid:%b"
          spec seq u valid
  in
  let s1 = Bytes.create 1
  and s2 = Bytes.create 2
  and s3 = Bytes.create 3
  and s4 = Bytes.create 4
  in
  for b0 = 0x00 to 0xFF do
    Bytes.unsafe_set s1 0 (Char.unsafe_chr b0);
    if test s1 = `Decoded then () else begin
      Bytes.unsafe_set s2 0 (Char.unsafe_chr b0);
      for b1 = 0x00 to 0xFF do
        Bytes.unsafe_set s2 1 (Char.unsafe_chr b1);
	      if test s2 = `Decoded then ()
       else begin
         Bytes.unsafe_set s3 0 (Char.unsafe_chr b0);
         Bytes.unsafe_set s3 1 (Char.unsafe_chr b1);
	       for b2 = 0x00 to 0xFF do
          Bytes.unsafe_set s3 2 (Char.unsafe_chr b2);
	        if test s3 = `Decoded then ()
         else begin
           Bytes.unsafe_set s4 0 (Char.unsafe_chr b0);
           Bytes.unsafe_set s4 1 (Char.unsafe_chr b1);
           Bytes.unsafe_set s4 2 (Char.unsafe_chr b2);
	         for b3 = 0x00 to 0xFF do
            Bytes.unsafe_set s4 3 (Char.unsafe_chr b3);
		        ignore (test s4)
	        done;
	       end
	      done;
	     end
      done;
    end
  done

let utf8_test impl =
  let umap, bmap = utf8_maps () in
  utf8_encode_test umap;
  utf8_decode_test impl bmap; (* Proof by exhaustiveness... *)
  ()

let test impl =
  Printexc.record_backtrace true;
  utf8_test impl;
  log "All tests succeeded.\n"

let main () =
  let usage = "Usage: test [--adhoc | --dfa | --if | --pat] " in
  let impl = ref (module Utf_x_adhoc : Utf_x.S) in
  let args =
    [ "--adhoc", Arg.Unit (fun () -> impl := (module Utf_x_adhoc : Utf_x.S)),
      "Test the adhoc implementation (default).";
      "--dfa", Arg.Unit (fun () -> impl := (module Utf_x_dfa : Utf_x.S)),
      "Test the DFA implementation";
      "--if", Arg.Unit (fun () -> impl := (module Utf_x_if : Utf_x.S)),
      "Test the if branches implementation.";
      "--pat", Arg.Unit (fun () -> impl := (module Utf_x_pat : Utf_x.S)),
      "Test the pattern implementation." ]
  in
  let fail_pos s = raise (Arg.Bad (strf "Don't know what to do with %S" s)) in
  Arg.parse args fail_pos usage;
  test !impl

let () = if !Sys.interactive then () else main ()
