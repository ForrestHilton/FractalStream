# `fractalstream-core` developer's guide

## What *isn't* in `fractalstream-core`?

`fractalstream-core` defines the core capabilities of FractalStream--things like
parsing and AST processing, handling of configuration values, parallelized
task execution, and project structure. It *doesn't* contain things like
compiler backends or the user interface code. The idea is that everything
FractalStream needs to do is represented in `fractalstream-core`, but specific
details like how to execute scripts and how to interact with the user are
meant to be handled elsewhere. This division makes it easier to do things like
target FractalStream to platforms where LLVM is not available, or to use
different UI libraries. It also is a form of future-proofing: LLVM libraries
and UI libraries change relatively fast, so we split out the more slowly-changing
core capabilities of `FractalStream` into the `fractalstream-core` library.

## User interface

Even though `fractalstream-core` doesn't *implement* the user interface for
FractalStream, it does *define* the structure of the UI and interactions
between the UI layer and FractalStream itself. To actually *implement* a UI,
you must implement the `ToUI` typeclass from the `UI` module.

## Environments

FractalStream is a compiler. That means that when you work on FractalStream,
you have to think about two different compilers: the Haskell compiler building
FractalStream itself, and also the FractalStream compiler and how it builds
FractalStream scripts.

FractalStream scripts have to interact with many different functions. Some of
those functions are defined in the scripts, and others are provided by the
FractalStream program itself or its UI layer. To represent these functions
in FractalStream itself, we use `Environment`s. These are Haskell types that
represent a collection of variables in the running FractalStream script.

`Environment`s are a type-level concept used in the FractalStream code, so
there are no values that represent `Environment`s directly. If you need a
value-level proxy for an `Environment` type, use the `EnvironmentProxy`
datatype defined in `Language.Environment`. For each `env` type of kind
`Environment`, there is a unique value of type `EnvironmentProxy env`.
Make use of `EnvironmentProxy env` when you want to get your hands on
the type-level `Environment` `env`.

An `Environment` is made up of a sequence of `Symbol` / `FSType` pairs. If
you have a way to turn any `Symbol` and `FSType` into some Haskell type,
then you can create a `Context` where [FIXME THIS EXPLANATION IS GOING NOWHERE]

## The `Value` AST

The AST representing values in a FractalStream script is the `Value` datatype,
defined in the `Language.Value` module. Each `Value` has a type parameter that
defines the `FSType` of the value, along with the `Environment` which that
value requires. For example, the expression `z^2 + C` corresponds to an AST
of type `Value '( '[ '("z", 'ComplexT), '("C", 'ComplexT) ], 'ComplexT)`,
meaning "`z^2 + C` is a complex-valued expression that involves two complex-valued
variables called `z` and `C`". The type parameters to `Value` are used to ensure
that no expressions refer to undefined variables, and all subexpressions are
well-typed.

## User interface events

FractalStream has its own model of UI events (e.g. "click", "drag", "pinch", "a button was pressed"), defined in the `Event` module. Each event is annotated with an
`Environment` which indicates the kind of data carried by that event. For example,
the `Timer` event has type `Timer :: Event '[]`, meaning that the timer event
doesn't carry any other data. On the other hand, the window resize event has
type `Resize :: Event '[ '("viewWidth", 'IntegerT), '("viewHeight", 'IntegerT) ]`
indicating that the resize event comes with two integer-valued parameters,
which are called `viewWidth` and `viewHeight` from the perspective of a running
FractalStream script.

## Layouts

The arrangement of UI elements is represented by a `Layout`, defined in
`Actor.Layout`.
## Ensembles and FractalStream script save files

A FractalStream `Ensemble` is a collection of viewers, configuration values, and
setup actions. Each saved FractalStream script is a serialized version of an
ensemble.

## Dynamic values

Many values in FractalStream are *dynamic*, meaning that they can be
updated by the user or by the running script. For example, the value
of `C` in a `z^2 + C` Julia set script is dynamic, because we expect
the user will be able to change the value of `C` to inspect different
Julia sets.

Dynamic values are implemented in code by types implementing the `Dynamic`
typeclass, defined in `Data.DynamicValue`. Dynamic values can be read
and written, but critically they also have "listeners" registered to them.
A listener is just a bit of code that should run whenever the dynamic
value is changed. By using listeners, we can do things like automatically
update a dynamical plane view when `C` has been changed, or update the value
of a text entry field when the user clicks somewhere in the parameter
plane.

## Concurrency

### Parallelized for-loops

### Synchronized resources


## Recursive datatypes

## *Indexed* recursive datatypes

## First-class families
