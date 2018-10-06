# OCaml MOOC

## Resources

* [Introduction to Functional Programming in OCaml MOOC](fun-mooc.fr/courses/course-v1:parisdiderot+56002+session03/about)
* [smeruelo/mooc-ocaml](https://github.com/smeruelo/mooc-ocaml)

## Notes

* has a REPL, [`utop`](https://github.com/ocaml-community/utop) is like IPython
  * `#use "yourfile.ml"`
  * `#trace function` to see how things are used
* package manager is opam (pyenv + pip / nvm + node)
* strongly, statically-typed
* uses type infernce
  * side-effect: functions can't have overloaded definitions `+` is for int `+.` is for floats
  * have to make explicit conversion: `float_of_int`, `char_of_float`, etc
* libraries in `/usr/lib/ocaml/`
* `;;` is only used in `toplevel`

```ocaml
(* all functions are defined this way *)
let name param1 param2 = x ;;
 ```

* can't iterate, need to use recursion
* `Array.init [size] function`
* `begin` and `end` can allow for multiline expressions, also parentheses
* `let rec` for recursive function

## Notes on Functional Programmming

[Source](https://sites.ualberta.ca/~jhoover/325/CourseNotes/section/Scheme_2.htm)

We now begin looking at various idioms for functional programming. The basic strategy of all programming is of course:

* Decompose - break the problem into smaller pieces.
* Abstract - identify the operations you need and build small functions and data structures to perform them.

Once you have broken your problem into smaller pieces, the following general guidelines hold for approaching the problem in a functional way:

1. If the problem can be expressed as a bunch of independent parallel subproblems, then consider map.
1. If the problem can be expressed as a traversal over the data while accumulating the final result, consider fold/reduce.
1. If the problem can be expressed in terms of smaller instances of the same problem, then consider recursion.

## Tail Recursion

(From [ThinkOCaml](http://greenteapress.com/thinkocaml/))

The answer is called tail-end recursion. The general idea is to write your recursive function such that the value returned by the recursive call is what’s returned by your function, i.e., there’s no pending operation in the function waiting for the value returned by the recursive call. That way, the function can say, ”Don’t bother with me anymore, just take the answer from my recursive call as the result. You can just forget all of my state information.”

Generally, in order to implement tail-end recursion we need an extra argument that accumulates the result along the way, so the inner-most recursive call has all the information from the steps before it.

```ocaml
(* Regular Recursion *)
let rec cum_sum n =
  if n = 1 then 1 else n + cum_sum(n - 1);;

(* Tail-end Recrusion *)
let rec cum_sum_helper accum n =
  if n = 1 then accum + 1 else cum_sum_helper (accum + n) (n - 1);;

let cum_sum = cum_sum_helper 0;;
```

* two separate functions: **helper function** and **call function**

Tail-recursion is generally faster and requires less memory than standard recursion. However, it can be hard to see a tail-recursive solution the first time writing an algorithm, so it’s often easier to write a standard recursion solution and then figure out a tail-recursive algorithm based on that code. Also, tail-recursive solutions are usually much less clear than standard recursion, so in cases where other people will be reading your code and you don’t need to worry about the memory used, it’s often better to just use standard recursion. Finally, there might not always be an obvious tail-recursive solution and it might not be the best use of your time to find an algorithm if it won’t significantly improve your code’s performance.
