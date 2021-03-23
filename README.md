# bazel_erlang

Bazel rules for Erlang sources

## Assumptions

`erlang_lib` and `ct_suite` macros require the standard otp layout, relative to the bazel package (to some degree abitrary layout can be handled with with the `erlc`, `app_file`, `bazel_erlang_lib` & `ct_test` rules which those macros utilize). For an erlang application named `my_erlang_app` this means:

```
my_erlang_app
├── BUILD.bazel
├── include
│   ├── ...
│   └── my_header.hrl
├── priv
│   └── schema
├── src
│   ├── ...
│   └── my_erlang_app.erl
└── test
    ├── ...
    └── unit_SUITE.erl
```

And that the convention is followed where, using the `dest` attribute of the `erlc` rule:
1. Compiled production bytecode is placed in `ebin`
2. Compiled test bytecode is placed in `src`
3. Compiled test suite and test helper bytecode is placed in `test`

The example below follows this convention.

## Minimal Example

```starlark
load("@bazel-erlang//:bazel_erlang_lib.bzl", "bazel_erlang_lib", "erlang_lib", "erlc")
load("@bazel-erlang//:ct.bzl", "ct_suite")

APP_NAME = "my_cool_app"
APP_VERSION = "0.1.0

erlang_lib(
    app_name = APP_NAME,
    app_version = APP_VERSION,
)

erlc(
    name = "test_beam_files",
    hdrs = glob(["include/*.hrl", "src/*.hrl"]),
    srcs = glob(["src/*.erl"]),
    erlc_opts = ["-DTEST"],
    dest = "src",
)

bazel_erlang_lib(
    name = "test_bazel_erlang_lib",
    app_name = APP_NAME,
    hdrs = glob(["include/*.hrl"]),
    app = ":app_file",
    beam = [":test_beam_files"],

ct_suite(
    name = "unit_SUITE",
)
```

```shell
bazel test //... \
    --@bazel-erlang//:erlang_home=/path/to/erlang \
    --@bazel-erlang//:erlang_version=23.2
```
