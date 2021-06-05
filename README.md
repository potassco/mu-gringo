# Prototypical Grounder Implementation

This project implements the grounding algorithms in the paper "On the
Foundations of Grounding in Answer Set Programming". Unlike a system like
clingo, this implementation is not meant to be used in practice. It is very
inefficient, does not translate into a format accepted by existing ASP solvers,
and only considers a very restricted input language. Instead, the algorithms
here are implemented as simple and straight-forward as possible with the goal
to give an idea how a grounder proceeds.

## Usage

The algorithms are written in [rust], which has to be installed to compile the
algorithms.

    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

Afterward, the examples can be grounded using

    cargo run -- [files]

If no files are given, the grounder reads from standard input. Furthemore,
option `-v` can be used to print a lot of status information about what is
happening during grounding.

## Example Calls

    cargo run -- -v examples/ex01.lp
    cargo run -- -v examples/ex02.lp
    ...
    cargo run -- -v examples/ex11.lp

[rust]: https://www.rust-lang.org/
