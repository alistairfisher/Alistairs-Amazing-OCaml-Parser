# Alistairs-Amazing-OCaml-Parser

An OCaml parser designed to flag important warnings from Verilator's linting.

Currently it flags errors of the form 'the number of expected bits is greater than the number of generated bits', but it is easily extensible to other forms of errors.

It reads from stdin and outputs to a preset file.
