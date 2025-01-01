# Archon VM

Low level IR and runtime for [Archon](https://github.com/tgeng/archon).

## What is this?

Archon is a work-in-progress experimental language that combines dependent types, lexical algebraic effects and
handlers, and quantitative types. Since Archon has algebraic effects and handlers, which supports multi-shot
continuations, compiling it to machine code is not straightforward. The low level IR in this project is close enough to
the core type theory of Archon so that compiling Archon to this low level IR should be straightforward: basically just
erase unneeded information.

## Some Design Choices

* **Selective CPS transformation** Functions with complex effects (e.g. effects whose handlers may capture the
  continuation and manipulate it) are compiled into state machines with CPS transformation. Delimited continuation is
  then represented as a singly linked list of continuation objects. Functions that only performs simple effects (e.g.
  `State`, `IO`, non-resumable exception) are not CPS-transformed and hence can run as fast as normal functions with
  little additional overhead.
* **Uniform value representation** All values have a uniform representation so that everything has the same size. That
  is, larger values that cannot fit 8 bytes are represented as pointers. Pointers are 4 byte-aligned and the lowest two
  bits are used to tag the value. Native integer is 63-bit because the last bit is used to tag the value as integer.
* **Conservative GC** A conservative GC is used for simplicity of implementation and also flexibility of calling foreign
  functions.
* **Typeless** The low level IR has no type information and no type checking is done on it. This is because Archon is
  already type-checked and there is no need to do type checking in low level IR again. In fact, adding type checking to
  the low level IR is not trivial because Archon supports dependent types, which means one can easily create a function
  whose argument can have any types.

## Getting Started

`cargo build` to build the project. `cargo test` to run tests.

See `src/ast/term.rs` and `src/ast/signature.rs` for the low level IR. One can create a `Signature` and then compile it
with `src/backend/compiler.rs` to machine code. Currently only ARM64 macOS is supported. Also, there is no plan to
support 32-bit architectures.

A simpler frontend IR and parser is also provided in `src/frontend/f_term.rs` and it's used in tests.
See `resources/frontend/transpiler_tests/*.input.txt` for examples.

## TODOs

* Integrate Boehm's GC for memory management.
* Support more OS/architectures.
* Add some FFI support.
* Add more built-in types in addition to 63-bit integers.
* Add more built-in functions for primitive manipulations.
