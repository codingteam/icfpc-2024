icfpc-y2024
===========
Contest page: https://icfpcontest2024.github.io/

Prerequisites
-------------
Install [Stack][stack] v2.15.7.

Compilation
-----------
```console
$ stack build
```

Configuring
-----------
Prepare file `TOKEN.txt` with the API key (in form of `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx`, without `Bearer`).

Running
-------
```console
$ stack run icfpc-y2024-exe [args]
```

where `args` are:
- `http <request>` will perform a HTTP request, e.g. `http "get index"`.

Testing
-------
```console
$ stack test
```

[stack]: https://docs.haskellstack.org/en/stable/
