# Erlang Gazelle plugin

This directory contains a plugin for [Gazelle](https://github.com/bazelbuild/bazel-gazelle) that generates and updates BUILD files for erlang code.

This plugin is currently based on the [rules_python implementation](https://github.com/bazelbuild/rules_python/tree/main/gazelle).

## To use

Create a WORKSPACE file with:

```starlark
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "099a9fb96a376ccbbb7d291ed4ecbdfd42f6bc822ab77ae6f1b5cb9e914e94fa",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.35.0/rules_go-v0.35.0.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.35.0/rules_go-v0.35.0.zip",
    ],
)

http_archive(
    name = "bazel_gazelle",
    sha256 = "efbbba6ac1a4fd342d5122cbdfdb82aeb2cf2862e35022c752eaddffada7c3f3",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.27.0/bazel-gazelle-v0.27.0.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.27.0/bazel-gazelle-v0.27.0.tar.gz",
    ],
)


load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")
load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies", "go_repository")

############################################################
# Define your own dependencies here using go_repository.
# Else, dependencies declared by rules_go/gazelle will be used.
# The first declaration of an external repository "wins".
############################################################

go_rules_dependencies()

go_register_toolchains(go_version = "1.18.3")

gazelle_dependencies()

git_repository(
    name = "rules_erlang",
    branch = "experimental-gazelle-extension",
    remote = "https://github.com/rabbitmq/rules_erlang.git",
)

load(
    "@rules_erlang//gazelle:deps.bzl",
    _erlang_gazelle_deps = "gazelle_deps",
)

_erlang_gazelle_deps()

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

And a `BUILD` file with:

```starlark
load("@bazel_gazelle//:def.bzl", "gazelle")
load("@rules_erlang//gazelle:def.bzl", "GAZELLE_ERLANG_RUNTIME_DEPS")

gazelle(
    name = "gazelle",
    data = GAZELLE_ERLANG_RUNTIME_DEPS,
    gazelle = "@rules_erlang//gazelle:gazelle_erlang_binary",
)
```

Then run `bazel run //:gazelle`
