# bazel_erlang

Bazel rules for Erlang sources

## Example

```starlark
load("@bazel_erlang//:beam.bzl", "beam")
load("@bazel_erlang//:ct.bzl", "ct_test")
load("@bazel_erlang//:escript.bzl", "escript")

beam(
    name = "compiled_srcs",
    srcs = glob(["src/*.erl"]),
)

beam(
    name = "compiled_suites",
    srcs = glob(["test/*_SUITE.erl"]),
)

ct_test(
    name = "all",
    compiled_suites = [":compiled_suites"],
    compiled_srcs = [":compiled_srcs"],
)

escript(
    name = "cli",
    compiled_srcs = [":compiled_srcs"],
)
```