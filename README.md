PyonR
=====

PyonR (pronounced "Pioneer") is an implementation of the Python programming language for the Racket platform.

## Installation:

Before installing PyonR, you will need to have [Racket](http://www.racket-lang.org/) v6.01 or above and optionally [Python](https://www.python.org/) 2.7.

You can install PyonR from DrRacket's Install Package dialog box with `git://github.com/pedropramos/PyonR` as the package source. Make sure you change the package name to `python`.

Alternatively, you can install it with the `raco` tool by running:

> `raco pkg install -n python git://github.com/pedropramos/PyonR`



## Updating:

You can update the currently installed version with DrRacket's Package Manager, or alternatively with the following raco command:

> `raco pkg update python`



## Use:

To use PyonR with DrRacket, simply replace `#lang racket` with `#lang python`.




## Limitations:

PyonR currently supports most of the Python language as specified by version 2.7, except for a few constructs (most notably the `del` and `with` statements, decorators and the `super` function for constructors) and some missing methods on built-in types.



## Extensions to the Python language:

### Importing modules from Python 2.7

If you have Python 2.7 installed on your system, you can import its modules using Python's importing statements, but replacing the `import` keyword by `cpyimport`. This allows you to access Python's full standard library as well any third-party libraries you have installed, such as NumPy or SciPy.

For a demonstration, check out the examples at `examples/numpy_arrays` and `examples/datetime`.


### Importing modules from Racket:

You can import Racket modules by using Python's `import` statements and specifying the module's require specification withing string quotes. For instance, to access Racket's `cons`, `car` and `cdr` functions from Python, one could type one of the following:

* `import "racket" as rkt`
* `from "racket" import cons, car, cdr` 
* `from "racket" import *`

Keep in mind that Python's rules for identifier names are stricter than Racket's. Therefore, PyonR resorts to name mangling for identifiers whose names are not allowed in Python. For instance, the `number->string` function would have to be imported as:

* `from "racket" import number_TO_string`

We provide a builtin Python module called `name_mangling` with the function `mangle` which returns a Python mangled name, given the original Racket name as argument.

### Predicate dispatching

PyonR supports an extensible predicate dispatch mechanism
to map Racket predicates to Python types. This is particularly aimed for giving a Python feel to imported Racket datatypes, since their functionality is typically spread among predicates, constructors, getters, setters and assorted functions. This way, you can map a datatype's predicate to a user-defined Python class such that getters and setters are available as properties and other functions are available as methods.

This functionality is provided by the builtin Python module `predicates` which defines the following functions:

* `set_predicate(pred, type)` - sets a mapping between the `pred` predicate and `type`.
* `set_predicate(pred)` - removes `pred`'s mapping.
* `define_subtype(pred, parent)` - defines the `pred` predicate as a subtype of `parent` predicate, so that pred is dispatched before parent.

For a demonstration, check out the example at `examples/rosetta`.