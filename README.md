# bazel_erlang

Bazel rules for Erlang sources

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
    app_version = APP_VERSION,
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
