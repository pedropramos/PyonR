PyonR
=====

PyonR (pronounced "Pioneer") is an implementation of the Python programming language for the Racket platform.

## Installation:

Before you install PyonR, you will need to have Racket v5.92 or above (download it from http://www.racket-lang.org/) and optionally Python 2.7.x (download it from https://www.python.org/).

You can install PyonR using DrRacket's Install Package dialog box with `git://github.com/pedropramos/PyonR` as the package source. Make sure you change the package's name to `python`.

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

You can import Racket libraries using a modified syntax for Python's import statements, simply by specifying the library's module path withing string quotes. 


Examples in the examples folder