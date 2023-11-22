load("@rules_erlang//:compile_many.bzl", "compile_many")

compile_many(
    name = "deps",
    apps = %{APPS},
)

compile_many(
    name = "test_deps",
    apps = %{TEST_APPS},
    testonly = True,
)
