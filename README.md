# rules_erlang

Bazel rules for Erlang sources

## Minimal Example

### `WORKSPACE` file

```starlark
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "bazel_skylib",
    sha256 = "af87959afe497dc8dfd4c6cb66e1279cb98ccc84284619ebfec27d9c09a903de",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.2.0/bazel-skylib-1.2.0.tar.gz",
        "https://github.com/bazelbuild/bazel-skylib/releases/download/1.2.0/bazel-skylib-1.2.0.tar.gz",
    ],
)

load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")

bazel_skylib_workspace()

http_archive(
    name = "rules_erlang",
    sha256 = "ce4b3b028c8d77e2175767c19d69636d78fa13fdb11279d70f3c2ab01bc32953",
    strip_prefix = "rules_erlang-2.5.2",
    urls = ["https://github.com/rabbitmq/rules_erlang/archive/refs/tags/2.5.2.zip"],
)

load("@rules_erlang//:rules_erlang.bzl", "rules_erlang_dependencies")

rules_erlang_dependencies()

register_toolchains(
    "@rules_erlang//:erlang_toolchain_external",
)
```

### `BUILD` file

```starlark
load("@rules_erlang//:erlang_app.bzl", "erlang_app", "test_erlang_app")
load("@rules_erlang//:xref.bzl", "xref")
load("@rules_erlang//:dialyze.bzl", "dialyze", "plt")
load("@rules_erlang//:ct.bzl", "ct_suite", "assert_suites")

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

### `.bazelrc` file

```shell
build --@rules_erlang//:erlang_home=/path/to/erlang
build --@rules_erlang//:erlang_version=23.2

build --platforms=@rules_erlang//platforms:erlang_external_platform
build --extra_execution_platforms=@rules_erlang//platforms:erlang_external_platform
```

### Compile and run all tests

```shell
bazel test //...
```

### Run the unit suite only

```shell
bazel test //:unit_SUITE
```

### Run a single test case

```shell
bazel test //:unit_SUITE \
    --test_env FOCUS="-group my_group -case my_case"
```

## Assumptions

`erlang_app` and `ct_suite` macros require the standard otp layout, relative to the bazel package (to some degree abitrary layout can be handled with with the `erlang_bytecode`, `app_file`, `erlang_app_info` & `ct_test` rules which those macros utilize). For an erlang application named `my_erlang_app` this means:

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

The example above follows these conventions.

## Copyright and License

(c) 2020-2022, VMware Inc or its affiliates.

Dual licensed under the Apache License Version 2.0 and
Mozilla Public License Version 2.0.

This means that the user can consider the library to be licensed under
**any of the licenses from the list** above. For example, you may
choose the Apache Public License 2.0 and include this library into a
commercial product.

See [LICENSE](./LICENSE) for details.
