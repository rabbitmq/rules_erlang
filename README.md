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

### `WORKSPACE` file

```starlark
http_archive(
    name = "bazel-erlang",
    sha256 = "422a9222522216f59a01703a13f578c601d6bddf5617bee8da3c43e3b299fc4e",
    strip_prefix = "bazel-erlang-1.1.0",
    urls = ["https://github.com/rabbitmq/bazel-erlang/archive/refs/tags/1.1.0.zip"],
)

load("@bazel-erlang//:bazel_erlang.bzl", "bazel_erlang_deps")

bazel_erlang_deps()
```

### `BUILD` file

```starlark
load("@bazel-erlang//:bazel_erlang_lib.bzl", "erlang_lib", "test_erlang_lib")
load("@bazel-erlang//:xref.bzl", "xref")
load("@bazel-erlang//:dialyze.bzl", "dialyze", "plt")
load("@bazel-erlang//:ct_sharded.bzl", "ct_suite")

APP_NAME = "my_cool_app"
APP_VERSION = "0.1.0

erlang_lib(
    app_name = APP_NAME,
    app_version = APP_VERSION,
)

test_erlang_lib(
    app_name = APP_NAME,
    app_version = APP_VERSION,
)

xref()

dialyze()

ct_suite(
    name = "unit_SUITE",
)
```

## Compile and run all tests

```shell
bazel test //... \
    --@bazel-erlang//:erlang_home=/path/to/erlang \
    --@bazel-erlang//:erlang_version=23.2
```

## Run the unit suite only

```shell
bazel test //:unit_SUITE \
    --@bazel-erlang//:erlang_home=/path/to/erlang \
    --@bazel-erlang//:erlang_version=23.2
```

## Run a single test case

```shell
bazel test //:unit_SUITE \
    --@bazel-erlang//:erlang_home=/path/to/erlang \
    --@bazel-erlang//:erlang_version=23.2 \
    --test_env FOCUS="-group my_group -case my_case"
```

## Copyright and License

(c) 2020-2021, VMware Inc or its affiliates.

Dual licensed under the Apache License Version 2.0 and
Mozilla Public License Version 2.0.

This means that the user can consider the library to be licensed under
**any of the licenses from the list** above. For example, you may
choose the Apache Public License 2.0 and include this library into a
commercial product.

See [LICENSE](./LICENSE) for details.
