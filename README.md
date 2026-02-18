Erlang Distributed Systems Project

This repository contains the source code for a university project (Distributed Systems course). It is written in Erlang and demonstrates multiple components for replicated services, message passing and simple fault scenarios.

Files kept in the repository:
- Erlang source files (`*.erl`)
- Configuration files (`*.cfg`)
- The main project report: `Aufgabe_HA.pdf` (German)
- Supporting notes: `Fragen.txt`, `Readme.txt`

Quick start
1. Compile all sources:

   erl -make

   or

   erlc *.erl

2. Launch an Erlang shell and start the modules you want to test.

Notes
- I removed compiled artifacts (`*.beam`), runtime logs and the crash dump to keep the repository clean. Add them back only if you need binary artifacts.
- The primary documentation (`Aufgabe_HA.pdf`) is written in German.

If you'd like, I can:
- Add a `Makefile` or `rebar.config` for reproducible builds
- Add example run scripts or GitHub Actions to build on push
