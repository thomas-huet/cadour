# Cadour

Simple feed aggregator, reads one URL per line and outputs HTML.
Supports RSS2 and Atom.

## Build and run

```
$ dune build cadour.exe
$ _build/default/cadour.exe < sources.list | cat header.html - > index.html
```

## Input format

- Lines starting with `#` are ignored.
- All other lines must contain one URL of a RSS or Atom feed.
- The URL may optionally be followed by ` regex`to only include entries that contain `regex` or by ` -regex` to exclude entries that contain `regex`.
