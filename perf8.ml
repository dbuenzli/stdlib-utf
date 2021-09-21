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

let impl_to_string = function
| `Uutf -> "UUTF" | `Dfa -> "DFA" | `Adhoc -> "ADHOC"

(* Data preparation. We take any file read it and duplicate
   its content until we get at least a 1GB string. *)

let string_of_file file =
  let ic = open_in_bin file in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len; close_in ic; Bytes.unsafe_to_string buf

let make_it_approx_1GB s =
  let one_gb = 1024 * 1024 * 1024 in
  let len = String.length s in
  if len >= 1024 * 1024 * 1024 then s else
  let count = one_gb / len in
  let rec rep acc n = if n <= 0 then acc else rep (s :: acc) (n - 1) in
  String.concat "" (rep [] count)

let get_data file =
  log "Preparing data with %s" file;
  let s = make_it_approx_1GB (string_of_file file) in
  let size_gb = float (String.length s) /. 1024. /. 1024. /. 1024. in
  s, size_gb

(* Best-effort decode *)

let dfa_best_effort_decode s =
  let rec loop b i =
    if i >= Utf_x_dfa.Bytes.length b then () else
    let d = Utf_x_dfa.Bytes.get_utf_8_uchar b i in
    let used = Utf_x_dfa.Bytes.utf_x_decode_used_bytes d in
    let u = Utf_x_dfa.Bytes.utf_x_decode_uchar d in
    Sys.opaque_identity @@ (ignore u);
    loop b (i + used)
  in
  loop (Bytes.unsafe_of_string s) 0

let adhoc_best_effort_decode s =
  let rec loop b i =
    if i >= Utf_x_adhoc.Bytes.length b then () else
    let d = Utf_x_adhoc.Bytes.get_utf_8_uchar b i in
    let used = Utf_x_adhoc.Bytes.utf_x_decode_used_bytes d in
    let u = Utf_x_adhoc.Bytes.utf_x_decode_uchar d in
    Sys.opaque_identity @@ (ignore u);
    loop b (i + used)
  in
  loop (Bytes.unsafe_of_string s) 0

let uutf_best_effort_decode s =
  let f () _ = function
  | `Uchar u -> Sys.opaque_identity @@ ignore u
  | `Malformed u -> Sys.opaque_identity @@ ignore Uutf.u_rep
  in
  Uutf.String.fold_utf_8 f () s

let best_effort_decode impl file =
  let s, size_gb = get_data file in
  Gc.full_major ();
  log "Decoding %.2fGB of data with %s" size_gb (impl_to_string impl);
  match impl with
  | `Dfa -> dfa_best_effort_decode s
  | `Adhoc -> adhoc_best_effort_decode s
  | `Uutf -> uutf_best_effort_decode s

(* Validate *)

let uutf_validate s =
  let f () _ = function
  | `Uchar u -> Sys.opaque_identity @@ ignore u
  | `Malformed u -> raise Exit
  in
  try Uutf.String.fold_utf_8 f () s; true with
  | Exit -> false

let dfa_validate = Utf_x_dfa.Bytes.is_valid_utf_8
let adhoc_validate = Utf_x_adhoc.Bytes.is_valid_utf_8

let validate impl file =
  let s, size_gb = get_data file in
  log "Validating %.2fGB of data with %s" size_gb (impl_to_string impl);
  let valid = match impl with
  | `Uutf -> uutf_validate s
  | `Dfa -> dfa_validate (Bytes.unsafe_of_string s)
  | `Adhoc -> adhoc_validate (Bytes.unsafe_of_string s)
  in
  log "Valid: %b" valid

(* Recode *)

let dfa_recode s =
  let rec loop b i b' j =
    if i >= Utf_x_dfa.Bytes.length b then (Bytes.unsafe_to_string b') else
    let d = Utf_x_dfa.Bytes.get_utf_8_uchar b i in
    match Utf_x_dfa.Bytes.utf_x_decode_valid d with
    | false -> raise Exit
    | true ->
        let used = Utf_x_dfa.Bytes.utf_x_decode_used_bytes d in
        let u = Utf_x_dfa.Bytes.utf_x_decode_uchar d in
        let used' = Utf_x_dfa.Bytes.set_utf_8_uchar b' j u in
        if used' < 0 then raise Exit else
        loop b (i + used) b' (j + used')
  in
  try
    let b' = Bytes.create (String.length s) in
    Some (loop (Bytes.unsafe_of_string s) 0 b' 0)
  with Exit -> None

let adhoc_recode s =
  let rec loop b i b' j =
    if i >= Utf_x_adhoc.Bytes.length b then (Bytes.unsafe_to_string b') else
    let d = Utf_x_adhoc.Bytes.get_utf_8_uchar b i in
    match Utf_x_adhoc.Bytes.utf_x_decode_valid d with
    | false -> raise Exit
    | true ->
        let used = Utf_x_adhoc.Bytes.utf_x_decode_used_bytes d in
        let u = Utf_x_adhoc.Bytes.utf_x_decode_uchar d in
        let used' = Utf_x_adhoc.Bytes.set_utf_8_uchar b' j u in
        if used' < 0 then raise Exit else
        loop b (i + used) b' (j + used')
  in
  try
    let b' = Bytes.create (String.length s) in
    Some (loop (Bytes.unsafe_of_string s) 0 b' 0)
  with Exit -> None

let uutf_recode s =
  let f b _ = function
  | `Uchar u -> Buffer.add_utf_8_uchar b u; b
  | `Malformed u -> raise Exit
  in
  try
    let b = Buffer.create (String.length s) in
    let b = Uutf.String.fold_utf_8 f b s in
    Some (Buffer.contents b)
  with Exit -> None

let recode impl file =
  let s, size_gb = get_data file in
  Gc.full_major ();
  log "Recoding %.2fGB of data with %s decode" size_gb (impl_to_string impl);
  let s' = match impl with
  | `Dfa -> dfa_recode s
  | `Adhoc -> adhoc_recode s
  | `Uutf -> uutf_recode s
  in
  match s' with
  | None -> log "Recode failure, non valid input data ?"
  | Some s' when String.equal s s' -> log "Recode sucess!"
  | Some _ -> assert false

(* Command line interface *)

let do_cmd cmd impl file = match cmd with
| `Decode -> best_effort_decode impl file
| `Validate -> validate impl file
| `Recode -> recode impl file

let main () =
  let usage = "Usage: perf8 [--dfa | --adhoc | --uutf] FILE" in
  let impl = ref `Adhoc in
  let cmd = ref `Decode in
  let args =
    [ "--dfa", Arg.Unit (fun () -> impl := `Dfa),
      "Test the DFA implementation";
      "--adhoc", Arg.Unit (fun () -> impl := `Adhoc),
      "Test the adhoc implementation (default).";
      "--uutf", Arg.Unit (fun () -> impl := `Uutf),
      "Test the Uutf implementation.";
      "--decode", Arg.Unit (fun () -> cmd := `Decode),
      "Decode the UTF-8 data.";
      "--validate", Arg.Unit (fun () -> cmd := `Validate),
      "Validate the UTF-8 data.";
      "--recode", Arg.Unit (fun () -> cmd := `Recode),
      "Decode and recode the UTF-8 data."]
  in
  let file = ref None in
  let set_file s = file := Some s in
  Arg.parse args set_file usage;
  match !file with
  | None -> Printf.eprintf "perf8: missing FILE argument\n%s\n" usage; exit 1
  | Some file -> do_cmd !cmd !impl file

let () = if !Sys.interactive then () else main ()
