# Reading the macOS Finder's .DS_Store files

This is an Emacs Lisp library for reading
[.DS_Store](https://en.wikipedia.org/wiki/.DS_Store) files. The code is 1:1
ported from https://docs.racket-lang.org/ds-store/index.html.

## API

## `(ds-store-read-file FILE &optional VERBOSE)`

Read .DS_Store FILE and return a list of records.
Optional argument VERBOSE indicates that we should print debug
message.

## Requires

- Emacs 25.1
