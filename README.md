# pryr (rhymes with pry bar)

Tools to pry back the surface of R and dig into the details. 

The easiest way to install `pryr` is with devtools:

```R
library(devtools)
install_github("pryr")
```

## Tools

`pryr` includes tools to better understand:

* `promises`: `uneval`, `is_promise`, `promise_info`
* scoping and environments: `where`, `rls`
* closures: `unenclose`
* calls and expressions: `call_tree`

And tools to make it easier to compute on the language:

* Alternative ways to make functions: `make_function`, `f`
* `substitute2`
* `modify_lang`
* `subs`
* `curry`
* `find_funs`

And to use existing R tools more easily:

* `%<d-%` and `%<a-%` for creating delayed or active bindings
* `%<c-%` for creating constants (locked bindings)
* `rebind` as a more user friendly version of `<<-`