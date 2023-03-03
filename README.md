# README #

This project contains examples of using Haskell's conduit library.

### What is this repository for? ###

* These code examples provide iterative examples of building up different uses of conduit (mostly) one step at a time
* Version 0.2
* Conduit usage upgraded to that of version 1.3.4.3

### How do I get set up? ###

Prerequisites:

* Have GHC version >= 7.8 installed (examples tested against 7.8.3 and 8.10.7)
* Have Cabal version >= 1.20 installed (examples tested against 1.20.0.2 and 3.8.1.0)
* Tested on Windows 8.1, Amazon Linux AMI and Ubuntu 22.04
* `telnet` or an equivalent tool must be available on the system (for running the examples)

Installing:

* Run `cabal sandbox init` first
* Run `cabal configure`; install any missing libraries with `cabal install library-name`
* Run `cabal build`
* Everything should _just work_ from here, assuming your environment is not too crazy/different from the test environments listed above
* On *nix, you might not see output on the network examples until you Ctrl+C the server

### How do I run the examples? ###

conduit-101-01:

* Just run the binary built by `cabal build`; this should be at `dist/build/conduit-101-01/conduit-101-01`
* The user prompts should take you the rest of the way

conduit-101-02:

* Run the binary built by `cabal build`; this should be at `dist/build/conduit-101-02/conduit-101-02`
* From another shell terminal, run `telnet 127.0.0.1 4000` (or an equivalent tool to communicate to localhost on port 4000)
* Type some words; the server output should split each word to a new line until a newline character is read

conduit-101-03:

* Run the binary built by `cabal build`; this should be at `dist/build/conduit-101-03/conduit-101-03`
* Run the server binary from `conduit-101-02`
* Press ENTER for the program to immediately write some output to the server

conduit-101-04:

* Run the binary built by `cabal build`; this should be at `dist/build/conduit-101-04/conduit-101-04`
* Run the server binary from `conduit-101-02`
* Press ENTER to continue to the next setup step
* From another shell terminal, run `telnet 127.0.0.1 4002` (or an equivalent tool to communicate to localhost on port 4002)
* Press ENTER to move to the "execution" step
* Now, type some text into the telnet terminal; you should see the same output run through this server as well as the `conduit-101-02` server
