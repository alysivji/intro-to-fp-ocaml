# OCaml Notes

* has a REPL, [`utop`](https://github.com/ocaml-community/utop) is like IPython
* package manager is opam (pyenv + pip / nvm + node)
* strongly, statically-typed
* uses type infernce
  * side-effect: functions can't have overloaded definitions `+` is for int `+.` is for floats
  * have to make explicit conversion: `float_of_int`, `char_of_float`, etc
* libraries in `/usr/lib/ocaml/`
* `;;` is only used in `toplevel`

```ocaml
(* all functions are defined this way *)
 let name [parameters] = expression ;;
 ```

* can't iterate, need to use recursion
