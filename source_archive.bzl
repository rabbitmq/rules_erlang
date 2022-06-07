load("@rules_pkg//:pkg.bzl", "pkg_tar")
load(
    "//private:source_tree.bzl",
    "source_tree",
)

def source_archive():
    source_tree(
        name = "source_tree",
        deps = [":erlang_app"],
    )

    pkg_tar(
        name = "source_archive",
        srcs = [":source_tree"],
        package_dir = "deps",
        strip_prefix = "source_tree",
        visibility = ["//visibility:public"],
    )
