# rules_erlang

Bazel rules for Erlang sources

## Assumptions

`erlang_app` and `ct_suite` macros require the standard otp layout, relative to the bazel package (to some degree abitrary layout can be handled with with the `erlc`, `app_file`, `erlang_app_info` & `ct_test` rules which those macros utilize). For an erlang application named `my_erlang_app` this means:

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
    name = "rules_erlang",
    sha256 = "7fd0564918537eb72294d17f5f8dc663907b16d712773b1c006e83194746a1c0",
    strip_prefix = "rules_erlang-2.0.0",
    urls = ["https://github.com/rabbitmq/rules_erlang/archive/refs/tags/2.0.0.zip"],
)

load("@rules_erlang//:rules_erlang.bzl", "rules_erlang_dependencies")

rules_erlang_dependencies()
```

### `BUILD` file

```starlark
load("@rules_erlang//:erlang_app.bzl", "erlang_app", "test_erlang_app")
load("@rules_erlang//:xref.bzl", "xref")
load("@rules_erlang//:dialyze.bzl", "dialyze", "plt")
load("@rules_erlang//:ct_sharded.bzl", "ct_suite", "assert_suites")

APP_NAME = "my_cool_app"
APP_VERSION = "0.1.0

erlang_app(
    app_name = APP_NAME,
    app_version = APP_VERSION,
)

test_erlang_app(
    app_name = APP_NAME,
    app_version = APP_VERSION,
)

xref()

dialyze()

assert_suites([
    ct_suite(
        name = "unit_SUITE",
    ),
])
```

## Compile and run all tests

```shell
bazel test //... \
    --@rules_erlang//:erlang_home=/path/to/erlang \
    --@rules_erlang//:erlang_version=23.2
```

## Run the unit suite only

```shell
bazel test //:unit_SUITE \
    --@rules_erlang//:erlang_home=/path/to/erlang \
    --@rules_erlang//:erlang_version=23.2
```

## Run a single test case

```shell
bazel test //:unit_SUITE \
    --@rules_erlang//:erlang_home=/path/to/erlang \
    --@rules_erlang//:erlang_version=23.2 \
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
