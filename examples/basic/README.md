# Simple example with an eunit test

This is intended to be a minimal example. A single, non-umbrella application, with a single eunit test.

## Standard workflow

Build and run the tests with: `bazel test //...`

## Interactive in the Erlang shell
Start a shell where you can interactively run the tests, and recompile files as needed:
1. `bazel run repl -c dbg`

Then in the erlang shell:
1. `eunit:test(basic).`
2. `c(basic_tests).`
3. See #1
