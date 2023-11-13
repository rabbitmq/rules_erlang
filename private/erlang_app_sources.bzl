load(":erlang_bytecode2.bzl", "ErlcOptsInfo")
load(":erl_analyze.bzl", "ErlAnalyzeInfo")

ErlangAppSourcesInfo = provider(
    doc = "Produced by the erlang_app_sources rule",
    fields = {
        "name": "name of the application in erlang",
        "srcs": "source files",
    }
)

def _impl(ctx):
    return [
        ctx.attr.analysis[ErlcOptsInfo],
        ErlangAppSourcesInfo(
            name = ctx.attr.app_name,
            srcs = ctx.files.srcs,
        ),
        DefaultInfo(
            files = depset(ctx.files.srcs + ctx.files.analysis),
        )
    ]

erlang_app_sources = rule(
    implementation = _impl,
    attrs = {
        "app_name": attr.string(
            mandatory = True,
        ),
        "srcs": attr.label_list(
            mandatory = True,
            allow_files = True,
        ),
        "analysis": attr.label(
            mandatory = True,
            providers = [ErlcOptsInfo, ErlAnalyzeInfo],
        )
    },
    provides = [ErlcOptsInfo, ErlangAppSourcesInfo]
)
