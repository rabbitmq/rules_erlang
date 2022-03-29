load("//:rules_erlang.bzl", "rules_erlang_dependencies")

def _download_xrefr(ctx):
    rules_erlang_dependencies()

download_xrefr = module_extension(
    implementation = _download_xrefr,
)
