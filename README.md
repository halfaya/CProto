# CProto

Protoype for synthesis of musical counterpoint using an SMT solver in Haskell. The work was ported to Agda (https://github.com/halfaya/MusicTools) and development of this version is no longer active.

Uses the `SBV` (for SMT) and `HCodecs` (for MIDI) libraries. Also requires `z3` be installed and in the path.
I am using GHC 9.2.2 but earlier versions may also work.

To build the program run `cabal build` in the top level directory. To run use `cabal run`.
