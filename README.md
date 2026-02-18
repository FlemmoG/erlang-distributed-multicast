Erlang Distributed Systems Project

This repository contains the source code for a university project (Distributed Systems course). It is written in Erlang and demonstrates multiple components for replicated services, message passing and simple fault scenarios.

Files kept in the repository:
- Erlang source files (`*.erl`)
- Configuration files (`*.cfg`)
- The main project report: `Aufgabe_HA.pdf` (German)

Quick start
1. Compile all sources:

   erl -make

   or

   erlc *.erl

2. Launch an Erlang shell and start the modules you want to test.

Notes
- The primary documentation (`Aufgabe_HA.pdf`) is written in German.

Repository structure
- `src/`: Erlang source files (`*.erl`)
- `cfg/`: configuration files (`*.cfg`)
- `docs/`: project report and notes (PDF and text files)

Example: compile and run
1. From repository root, compile all sources in `src/`:

```bash
cd src
erl -make
```

2. Start an Erlang shell (examples assume you use the same cookie as in configs):

```bash
erl -sname mynode -setcookie zummsel
% then in the shell, for example:
% c(cbCast).  % compile if needed
% towerClock:init().
% towerCBC:init(auto).
```

Detailed start instructions

Build
- The package normally produces the compiled files: `towerClock.beam`, `towerCBC.beam`, `vectorC.beam`, `cbCast.beam`, `util.beam`, `vsutil.beam`.
- Also included: the original `Readme.txt`, `towerCBC.cfg`, and `towerClock.cfg`.
- NOTE: `tower*.cfg` files are only used by `vectorC` and `cbCast`.

Start Erlang nodes
- Example commands to start named nodes (use the same cookie as in the configs):

```bash
erl -sname <towerCBCNode> -setcookie zummsel
erl -sname <towerClockNode> -setcookie zummsel
erl -sname <cbCastNode> -setcookie zummsel
```

Start the tower modules (on different nodes)
- In the shell for the clock node:

```erlang
towerClock:init().
```

`towerClock.cfg` expects entries like:

```erlang
{servername, <name_on_node>}.
{servernode, <node_of_tower>}.
```

- On the CBC node, start the tower control:

```erlang
towerCBC:init(manu).  % or towerCBC:init(auto).
```

`towerCBC.cfg` expects similar `{servername, ...}` and `{servernode, ...}` entries.

Start communication units (on separate nodes)
- Start a communication unit with:

```erlang
cbCast:init().
```

This reads `towerCBC.cfg`; via the `vectorC` ADT it also uses `towerClock.cfg`.

Shutdown
- To stop the system, quit the Erlang shells (e.g. `q` or `Ctrl-G` then `q`).

Running tests
- On the `towerCBC` node run the tests. All other nodes must be started (but not running programs) according to `test.cfg`:

```erlang
testCBC:testADT().
% maybe restart nodes
testCBC:test().
```
