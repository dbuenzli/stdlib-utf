open B0_kit.V000
open B00_std

let uutf = B0_ocaml.libname "uutf"

let file v = `File (Fpath.v v)
let mod_srcs n = file (n ^ ".mli"), file (n ^ ".ml")

let iface = file "utf_x.mli"
let uchar_mli, uchar_ml = mod_srcs "utf_uchar"
let utf_16_ml = file "utf_16.ml"
let adhoc_mli, adhoc_ml = mod_srcs "utf_x_adhoc"
let if_mli, if_ml = mod_srcs "utf_x_if"
let dfa_mli, dfa_ml = mod_srcs "utf_x_dfa"
let pat_mli, pat_ml = mod_srcs "utf_x_pat"

let base = [iface; uchar_mli; uchar_ml; utf_16_ml]
let all_impls =
  [adhoc_mli; adhoc_ml; if_mli; if_ml; dfa_mli; dfa_ml; pat_mli; pat_ml ] @
  base

let examples =
  let srcs = [adhoc_mli; adhoc_ml; file "examples.ml"] @ base in
  B0_ocaml.exe "examples" ~doc:"Sample code" ~srcs

let test =
  let srcs = (file "test.ml" :: all_impls) in
  B0_ocaml.exe "test" ~doc:"Test suite" ~srcs

let test8_long =
  let srcs = (file "test8_long.ml" :: all_impls) in
  B0_ocaml.exe "test8_long" ~doc:"Long UTF-8 conformance test" ~srcs

let perf8 =
  let srcs = (file "perf8.ml" :: all_impls) in
  let requires = [uutf] in
  B0_ocaml.exe "perf8" ~doc:"Performance test tool for UTF-8" ~requires ~srcs

let trip8 =
  let srcs = (file "trip8.ml" :: all_impls) in
  let requires = [uutf] in
  B0_ocaml.exe "trip8" ~doc:"Best-effort UTF-8 recode" ~requires ~srcs
