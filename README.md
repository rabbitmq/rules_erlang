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
load("@bazel-erlang//:bazel_erlang_lib.bzl", "erlang_lib", "test_erlang_lib")
load("@bazel-erlang//:ct.bzl", "ct_suite")

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

ct_suite(
    name = "unit_SUITE",
)
```

```shell
bazel test //... \
    --@bazel-erlang//:erlang_home=/path/to/erlang \
    --@bazel-erlang//:erlang_version=23.2
```

## Run a single test case

```shell
bazel test //:unit_SUITE \
    --@bazel-erlang//:erlang_home=/path/to/erlang \
    --@bazel-erlang//:erlang_version=23.2 \
    --test_env FOCUS="-suite unit_SUITE -group my_group -case my_case"
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
