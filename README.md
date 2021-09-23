Adding UTF-X decoding support to the OCaml Stdlib 
=================================================

[Upstream issue][upstream]. 

Plays around improving [this old proposal][old-proposal]. Especially
on [Alain's comment][alain-comment] to avoid exceptions. 

For now we simply focus on providing an API in `Bytes`, the rest can
follow easily and/or later. 

We introduce the abstract type `Bytes.utf_x_decode` for decode results
which is just an `int` value that has the decoded `Uchar.t` (or
`Uchar.rep` in case of decode error) in the lower bits and information
about the decode in bits above `Uchar.max`. The scheme is compatible
with both 32-bit and 64-bit platforms, see the implementation for more
details. No exception have to be introduced and the API does not
allocate on decodes.

The [resulting API](utf_x.mli) seems nice to
use. [`examples.ml`](examples.ml) show how [`Uutf`-like
folders][uutf-fold] and `Seq` based iterators can easily be derived in
a few lines.

It is also fool proof w.r.t. to best-effort decodes, `Uchar.rep` will
be appropriately added on decoding errors as it should be for security
reasons. See the `Seq` iterator example and more information on this
below.

A few implementation are provided which differ only in their UTF-8
treatment:

1. [`utf_x_adhoc.ml`](utf_x_adhoc.ml), uses a 256 bytes string to index
   the cases mentionned in [TUS Table 3.7][tus] and then performs some
   ugly ad-hoc decoding. This ugly code is mine :-)
2. [`utf_x_if.ml`](utf_x_if.ml), is similar to adhoc, but uses `if`
   branches instead of a table, the result is a bit harder to comprehend.
3. [`utf_x_pat.ml`](utf_x_pat.ml), is similar to adhoc but lets my
   favourite compiler work out the dispatch table itself by using pattern 
   matching on byte ranges. Yay !
4. [`utf_x_dfa.ml`](utf_x_dfa.ml), uses the UTF-8 decoding MIT-licensed
   DFA devised by Bjoern Hoehrmann [here][dfa]. This uses a 364 bytes
   string for the DFA and makes the decoding code much more elegant (if 
   I hadn't to rewrite it to an imperative loop to make it more 
   efficient).
   
Rough benchmarks made with [`perf8.ml`](perf8.ml) and measured via
`time` on my machine seems to indicate we have, ordered by most
performant:  `pat`, `if`, `adhoc` and `dfa`.

The first three being quite close to each other (within
10%). Comparing the fastest to the slowest, the `pat` version takes
the following percentage of the time taken by the `dfa` version.

* 62% [Markus Kuhn's UTF-8 demo][kuhn-utf-8]. A mixture of 
  things.
* 62% [Hindi wikipedia articles multistream][hindi-wiki] dataset.
  Exercises ASCII and [Devanagari][devanagari] decodes which are 
  three bytes long in UTF-8.
* 58% [Croatian wikipedia articles multistream][hr-wiki] dataset.
  Exercises ASCII and [Latin Extended-A][latin-ext-A] decodes which
  are two bytes long in UTF-8.
* 56% on this README, mainly ASCII.

So I think the best candidate for upstreaming would be `pat`.

## Best-effort decodes and U+FFFD replacements

The API also tries to take into account recommended strategies for
U+FFFD replacement on best-effort decoding. For a discussion about
these see [here][how-many-urep].

If nothing special is done, e.g. like the `Seq` iterator in
[`examples.ml`](examples.ml) the resulting API should behave like the
"state machine view". Supposedly this is what is mandated by the
WHATWG [Encoding][whatwg-encoding] standard, if you can infer it
through their crazy prose code specifications – TUS has a description
of it on page 126 of the [version 14.0.0 text][tus] if you can
remember all the precise jargon definitions :-) I believe that a short and
concise spec of all that is:

> In case of decoding error return an U+FFFE, if you error on the
> first byte, consume the byte, otherwise consume the bytes preceeding
> the erroring byte.

The API provides enough information on decodes to implement other
replacement modes like one `Uchar.rep` per bogus byte.

The [`urep-test.html`](data/urep-test.html) and
[`urep-spec.html`](data/urep-spec.html) files taken from the above
mentioned page can be used to test the WHATWG encoding standard
behaviour (note that `Uutf` does not respect this).


[upstream]: https://github.com/ocaml/ocaml/issues/10660
[old-proposal]: https://gist.github.com/dbuenzli/211e1fb4d8dfce0d22c6d6616260cdd9
[alain-comment]: https://gist.github.com/dbuenzli/211e1fb4d8dfce0d22c6d6616260cdd9#gistcomment-2574875
[how-many-urep]: https://hsivonen.fi/broken-utf-8/
[tus]: http://www.unicode.org/versions/Unicode14.0.0/ch03.pdf
[whatwg-encoding]: https://encoding.spec.whatwg.org/
[dfa]: http://bjoern.hoehrmann.de/utf-8/decoder/dfa/#variations
[hindi-wiki]: https://dumps.wikimedia.org/hiwiki/20210920/
[hr-wiki]: https://dumps.wikimedia.org/hrwiki/20210920/
[kuhn-utf-8]: https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-demo.txt
[uutf-fold]: https://erratique.ch/software/uutf/doc/Uutf.String.html#1_Stringfolders
[devanagari]: http://www.unicode.org/charts/PDF/U0900.pdf
[latin-ext-a]: https://www.unicode.org/charts/PDF/U0100.pdf
