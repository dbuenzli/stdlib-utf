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

let log fmt = Printf.printf (fmt ^^ "\n%!")

let string_of_file file =
  let ic = open_in_bin file in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len; close_in ic; Bytes.unsafe_to_string buf

let adhoc_trip s b =
  let rec loop b i buf =
    if i >= Utf_x_adhoc.Bytes.length b then () else
    let d = Utf_x_adhoc.Bytes.get_utf_8_uchar b i in
    let used = Utf_x_adhoc.Uchar.utf_decode_length d in
    Buffer.add_utf_8_uchar buf (Utf_x_adhoc.Uchar.utf_decode_uchar d);
    loop b (i + used) buf
  in
  loop (Bytes.unsafe_of_string s) 0 b

let dfa_trip s b =
  let rec loop b i buf =
    if i >= Utf_x_dfa.Bytes.length b then () else
    let d = Utf_x_dfa.Bytes.get_utf_8_uchar b i in
    let used = Utf_x_dfa.Uchar.utf_decode_length d in
    Buffer.add_utf_8_uchar buf (Utf_x_dfa.Uchar.utf_decode_uchar d);
    loop b (i + used) buf
  in
  loop (Bytes.unsafe_of_string s) 0 b

let if_trip s b =
  let rec loop b i buf =
    if i >= Utf_x_if.Bytes.length b then () else
    let d = Utf_x_if.Bytes.get_utf_8_uchar b i in
    let used = Utf_x_if.Uchar.utf_decode_length d in
    Buffer.add_utf_8_uchar buf (Utf_x_if.Uchar.utf_decode_uchar d);
    loop b (i + used) buf
  in
  loop (Bytes.unsafe_of_string s) 0 b

let pat_trip s b =
  let rec loop b i buf =
    if i >= Utf_x_pat.Bytes.length b then () else
    let d = Utf_x_pat.Bytes.get_utf_8_uchar b i in
    let used = Utf_x_pat.Uchar.utf_decode_length d in
    Buffer.add_utf_8_uchar buf (Utf_x_pat.Uchar.utf_decode_uchar d);
    loop b (i + used) buf
  in
  loop (Bytes.unsafe_of_string s) 0 b

let uutf_trip s b =
  let f b _ = function
  | `Uchar u -> Buffer.add_utf_8_uchar b u; b
  | `Malformed u -> Buffer.add_utf_8_uchar b Uchar.rep; b
  in
  ignore (Uutf.String.fold_utf_8 f b s)

let trip impl file =
  let s = string_of_file file in
  let b = Buffer.create (String.length s) in
  let () = match impl with
  | `Adhoc -> adhoc_trip s b
  | `Dfa -> dfa_trip s b
  | `If -> if_trip s b
  | `Pat -> pat_trip s b
  | `Uutf -> uutf_trip s b
  in
  print_string (Buffer.contents b)

(* Command line interface *)

let main () =
  let usage = "Usage: trip8 [--adhoc | --dfa | --if | --pat | --uutf] FILE" in
  let impl = ref `Adhoc in
  let args =
    [ "--adhoc", Arg.Unit (fun () -> impl := `Adhoc),
      "Use the adhoc implementation (default).";
      "--dfa", Arg.Unit (fun () -> impl := `Dfa),
      "Use the DFA implementation";
      "--if", Arg.Unit (fun () -> impl := `If),
      "Use the if branches implementation.";
      "--pat", Arg.Unit (fun () -> impl := `Pat),
      "Use the pattern implementation.";
      "--uutf", Arg.Unit (fun () -> impl := `Uutf),
      "Use the Uutf implementation." ]
  in
  let file = ref None in
  let set_file s = file := Some s in
  Arg.parse args set_file usage;
  match !file with
  | None -> Printf.eprintf "trip8: missing FILE argument\n%s\n" usage; exit 1
  | Some file -> trip !impl file

let () = if !Sys.interactive then () else main ()
