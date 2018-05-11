# pi

A π-calculus implementation in Haskell.

### Installation

Assuming you have [Stack](https://www.haskellstack.org/) installed, clone this
repository, `cd` into it and run `stack install`.

### Usage

Once installed, you can invoke `pi` with the command:

    $ pi "P"

Where `P` is a π-calculus expression. The syntax of expressions is as follows:

    P ::=   P | P       Parallel composition
       |    x/x.P       Output
       |    x(x).P      Input
       |    !P          Replication
       |    (@x)P       Restriction
       |    0           Inaction

The input expression will then be evaluated using Turner machine [1] semantics,
and the result printed to the command-line.

[1] David N. Turner - *The Polymorphic Pi-calculus: Theory and Implementation*.
Ph.D Thesis, University of Edinburgh, 1995.
