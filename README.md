# bazel_erlang

Bazel rules for Erlang sources

## Example

```starlark
load("@bazel_erlang//:ct.bzl", "ct_test")
load("@bazel_erlang//:escript.bzl", "escript")

bazel_erlang_lib(
    name = "compiled_srcs",
    srcs = glob(["src/*.erl"]),
)

ct_test(
    name = "all",
    suites = glob(["test/*_SUITE.erl"]),
    deps = [":compiled_srcs"],
)

escript(
    name = "cli",
    deps = [":compiled_srcs"],
)
```