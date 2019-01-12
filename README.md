Simple feed aggregator, reads one URL per line and outputs HTML.
Supports RSS2 and Atom.

Build and run:
```
$ dune build cadour.exe
$ _build/default/cadour.exe < sources.list | cat header.html - > index.html
```
