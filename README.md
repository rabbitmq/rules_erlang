# rules_erlang

Bazel rules for Erlang sources

## Minimal Example

### `WORKSPACE` file

```starlark
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "bazel_skylib",
    sha256 = "66ffd9315665bfaafc96b52278f57c7e2dd09f5ede279ea6d39b2be471e7e3aa",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.4.2/bazel-skylib-1.4.2.tar.gz",
        "https://github.com/bazelbuild/bazel-skylib/releases/download/1.4.2/bazel-skylib-1.4.2.tar.gz",
    ],
)

load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")

bazel_skylib_workspace()

http_archive(
    name = "rules_erlang",
    strip_prefix = "rules_erlang-4.0.0-beta.1",
    urls = ["https://github.com/rabbitmq/rules_erlang/archive/refs/tags/4.0.0-beta.1.zip"],
)

load(
    "@rules_erlang//:rules_erlang.bzl",
    "erlang_config",
    "rules_erlang_dependencies",
)

erlang_config()

rules_erlang_dependencies()

load("@erlang_config//:defaults.bzl", "register_defaults")

register_defaults()

```

### `BUILD` file

```starlark
load("@rules_erlang//:compile_many.bzl", "compile_many")
load("@rules_erlang//:dialyze.bzl", "DEFAULT_PLT_APPS", "dialyze", "plt")
load("@rules_erlang//:erlang_app_sources.bzl", "erlang_app_sources")
load("@rules_erlang//:erlc_opts_file.bzl", "erlc_opts_file")
load("@rules_erlang//:eunit2.bzl", "eunit")
load("@rules_erlang//:xref.bzl", "xref")
load("@rules_erlang//private:extract_app.bzl", "extract_app")

APP_NAME = "my_cool_app"

erlc_opts_file(
    name = "erlc_opts_file",
    values = select({
        "@rules_erlang//:debug_build": [
            "+debug_info",
            "+recv_opt_info",
            "+warn_export_vars",
            "+warn_shadow_vars",
            "+warn_obsolete_guard",
        ],
        "//conditions:default": [
            "+debug_info",
            "+recv_opt_info",
            "+warn_export_vars",
            "+warn_shadow_vars",
            "+warn_obsolete_guard",
            "+deterministic",
        ],
    }),
    out = "erlc_opts_file",
)

erlc_opts_file(
    name = "test_erlc_opts_file",
    values = select({
        "@rules_erlang//:debug_build": [
            "+debug_info",
            "-DTEST=1",
        ],
        "//conditions:default": [
            "+debug_info",
            "-DTEST=1",
            "+deterministic",
        ],
    }),
    out = "test_erlc_opts_file",
)

erlang_app_sources(
    name = "%s_srcs" % APP_NAME,
    app_name = APP_NAME,
    app_src = ":app_src",
    erlc_opts_file = ":erlc_opts_file",
    visibility = ["//visibility:public"],
)

erlang_app_sources(
    name = "test_%s_srcs" % APP_NAME,
    app_name = APP_NAME,
    app_src = ":app_src",
    erlc_opts_file = ":test_erlc_opts_file",
    visibility = ["//visibility:public"],
)

compile_many(
    name = "apps",
    apps = [
        ":%s_srcs" % APP_NAME,
    ],
)

compile_many(
    name = "test_apps",
    apps = [
        ":test_%s_srcs" % APP_NAME,
    ],
    testonly = True,
)

extract_app(
    name = "erlang_app",
    erl_libs = ":apps",
    app_name = APP_NAME,
    visibility = ["//visibility:public"],
)

extract_app(
    name = "test_erlang_app",
    erl_libs = ":test_apps",
    app_name = APP_NAME,
    beam_dest = "test",
    testonly = True,
)

xref(name = "xref")

plt(
    name = "deps_plt",
    apps = DEFAULT_PLT_APPS,
    for_target = ":erlang_app",
)

dialyze(
    name = "dialyze",
    plt = ":deps_plt",
)

eunit(name = "xref")

ct_suite(
    name = "unit_SUITE",
)

assert_suites2()
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

And that the convention is followed where, using the `dest` attribute of the `erlang_bytecode` rule:
1. Compiled production bytecode is placed in `ebin`
2. Compiled test bytecode is placed in `src`
3. Compiled test suite and test helper bytecode is placed in `test`

The example above follows these conventions.

## Additional examples

- https://github.com/rabbitmq/lz4-erlang uses rules_erlang with bzlmod and compiles native extensions
- https://github.com/rabbitmq/rabbitmq-server provides a complex example of usage

## Copyright and License

(c) 2020-2023 Broadcom. All Rights Reserved. The term “Broadcom” refers to Broadcom Inc. and/or its subsidiaries.  All rights reserved.

Dual licensed under the Apache License Version 2.0 and
Mozilla Public License Version 2.0.

This means that the user can consider the library to be licensed under
**any of the licenses from the list** above. For example, you may
choose the Apache Public License 2.0 and include this library into a
commercial product.

See [LICENSE](./LICENSE) for details.
