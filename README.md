# Counterpoint

Experiments in synthesis of musical counterpoint using an SMT solver.

Uses the `SBV` (for SMT) and `HCodecs` (for MIDI) libraries. Also requires `z3` be installed and in the path.
I am using GHC 9.0.1 but earlier versions may also work. `SBV` does not yet work with GHC 9.2.

To build the program run `cabal build` in the top level directory. To run use `cabal run`.
