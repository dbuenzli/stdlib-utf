Running stuff.

```
opam install uutf
opam pin add b0 --dev
curl -L https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-demo.txt \
        > data/UTF-8-demo.txt 
time b0 -a perf8 -- --adhoc data/UTF-8-demo.txt 
time b0 -a perf8 -- --dfa data/UTF-8-demo.txt 
time b0 -a perf8 -- --validate --adhoc data/UTF-8-demo.txt

# If you are interested in absolute numbers avoid timing the build

b0
time $(b0 unit build-dir perf8)/perf8 ...
```

