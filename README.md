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

ICFP Language
-------------

See: https://icfpcontest2024.github.io/icfp.html

Known Q&A
---------
- Only one command per request. They cannot be combined together like: `Sget index` `Sget index`.
- Q:
  ```text
  get index
  ```
  A:
  ```text
  Hello and welcome to the School of the Bound Variable!


  Before taking a course, we suggest that you have a look around.
  You're now looking at the [index].
  To practice your communication skills, you can use our [echo] service.
  Furthermore, to know how you and other students are doing, you can look at the [scoreboard].


  After looking around, you may be admitted to your first courses, so make sure to check this page from time to time.
  In the meantime, if you want to practice more advanced communication skills, you may also take our [language_test].
  ```

- Q:
  ```text
  get echo
  ```
  A:
  ```text
  The School of the Bound Variable provides a special echo service for you. If you send an ICFP expression evaluating to:

  ###
  echo <some text>
  ###

  it will respond with `<some text>`.

  Hint: you can use this to validate the Macroware Insight evaluator has the expected behavior. Of course the usual limitations still apply.
  ```
