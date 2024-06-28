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

Usage
-----
- Request generation:
  ```console
  stack run fileToGalaxy in.txt
  ```
  Note, it will not generate leading `S`!
- Submit request:
  ```console
  ./communicate.sh file.txt > out.txt
  ```
  Is must be a valid request.
- Get human-readable answer:
  ```console
  stack run fileFromGalaxy out.txt
  ```

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

- Q:
  ```text
  echo <sometext>"
  ```
  A:
  ```text
  &lt;sometext&gt;

  You scored some points for using the echo service!
  ```

- Q:
  ```text
  get lambdaman
  ```
  A:
  Welcome to the Lambda-Man course.

  It was the year 2014, and many members of our community worked hard to control Lambda-Man. Now, ten years later, this wonderful event is still memorized by holding a small Lambda-Man competition.

  This course will teach you how to optimally control Lambda-Man to eat all pills. There is no fruit involved (neither low-hanging nor high-hanging), and even better: no ghosts! The input to each problem is a simple rectangular grid such as the following:

  ```
  ###.#...
  ...L..##
  .#######
  ```

  The grid contains exactly one `L` character, which is the starting position of Lambda-Man. There will be one or more `.` characters indicating the locations of pills to be eaten, and `#` characters are walls. The outside boundary of the grid is considered to consist of walls as well.

  A solution should be a string of `U`, `R`, `D` and `L` characters (up, right, down, left, respectively) indicating the path to take. For example, a possible solution to the above example grid is the following path:
  ```
  LLLDURRRUDRRURR
  ```
  When Lambda-Man is instructed to move into a square containing a wall, nothing happens and the instruction is skipped. Your solution may consist of at most `1,000,000` characters.

  The following levels are available:
  * [lambdaman1] Best score: 33.
  * [lambdaman2] Best score: 44.
  * [lambdaman3] Best score: 58.
  * [lambdaman4] Best score: 372.
  * [lambdaman5] Best score: 161.
  * [lambdaman6] Best score: 78.
  * [lambdaman7] Best score: 428.
  * [lambdaman8] Best score: 321.
  * [lambdaman9]
  * [lambdaman10]
  * [lambdaman11] Best score: 9997.
  * [lambdaman12] Best score: 10003.
  * [lambdaman13] Best score: 9993.
  * [lambdaman14] Best score: 10011.
  * [lambdaman15] Best score: 9993.
  * [lambdaman16] Best score: 8209.
  * [lambdaman17] Best score: 2827.
  * [lambdaman18] Best score: 13871.
  * [lambdaman19] Best score: 16364.
  * [lambdaman20] Best score: 21934.
  * [lambdaman21]

  To submit a solution, send an ICFP expression that evaluates to:

  ```
  solve lambdamanX path
  ```

  Your score is number of bytes that the ICFP expressions consists of (i.e. the size of the POST body), so a lower score is better.
