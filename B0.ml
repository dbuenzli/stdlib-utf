open B0_kit.V000
open B00_std

let uutf = B0_ocaml.libname "uutf"

let file v = `File (Fpath.v v)

let iface = file "utf_x.mli"
let adhoc_mli, adhoc_ml = file "utf_x_adhoc.mli", file "utf_x_adhoc.ml"
let dfa_mli, dfa_ml = file "utf_x_dfa.mli", file "utf_x_dfa.ml"
let if_mli, if_ml = file "utf_x_if.mli", file "utf_x_if.ml"

let all_impls = [iface; adhoc_mli; adhoc_ml; if_mli; if_ml; dfa_mli; dfa_ml; ]

let examples =
  let srcs = [iface; adhoc_mli; adhoc_ml; file "examples.ml"] in
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
