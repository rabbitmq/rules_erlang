load(
    ":erlang_home.bzl",
    "ErlangHomeProvider",
    "ErlangVersionProvider",
)
load(":erlang_app_info.bzl", "ErlangAppInfo")
load(":ct.bzl", "code_paths")

DEFAULT_PLT_APPS = ["erts", "kernel", "stdlib"]

def _plt_impl(ctx):
    args = ctx.actions.args()
    if len(ctx.attr.apps) > 0:
        args.add_all('--apps', ctx.attr.apps)

    if ctx.attr.plt == None:
        args.add('--build_plt')
    else:
        args.add('--plt', ctx.file.plt.path)
        args.add('--add_to_plt')

    args.add('--output_plt', ctx.outputs.plt.path)

    ctx.actions.run(
        inputs = [ctx.file.plt] if ctx.file.plt != None else [],
        outputs = [ctx.outputs.plt],
        executable = 'dialyzer',
        arguments = [args],
        mnemonic = 'DIALYZER',
        use_default_shell_env=True,
    )

plt = rule(
    implementation = _plt_impl,
    attrs = {
        "_erlang_home": attr.label(
            default = Label("//:erlang_home"),
        ),
        "_erlang_version": attr.label(
            default = Label("//:erlang_version"),
        ),
        "plt": attr.label(
            allow_single_file = [".plt"],
        ),
        "apps": attr.string_list(
            default = DEFAULT_PLT_APPS,
        ),
    },
    outputs = {
        "plt": ".%{name}.plt",
    },
)

def _dialyze_impl(ctx):
    erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version

    lib_info = ctx.attr.target[ErlangAppInfo]

    if lib_info.erlang_version != erlang_version:
        fail("Erlang version mismatch ({} != {})".format(
            lib_info.erlang_version,
            ctx.attr._erlang_version,
        ))

    args = ctx.actions.args()
    if len(ctx.attr.plt_apps) > 0:
        args.add_all('--apps', ctx.attr.plt_apps)

    if ctx.attr.plt == None:
        args.add('--build_plt')
    else:
        args.add('--plt', ctx.file.plt.short_path)

    dirs = code_paths(ctx, ctx.attr.target)
    for dep in lib_info.deps:
        dirs.extend(code_paths(ctx, dep))

    args.add_all('-c', dirs)

    args.add_all(ctx.attr.dialyzer_opts)

#     script = """set -euo pipefail
# export HOME=${{TEST_TMPDIR}}
# set -x
# "{erlang_home}/bin/dialyzer.exe" {apps_args} {plt_args} -c {dirs} {opts} || test $? -eq 2
# """.format(
#         apps_args = apps_args,
#         plt_args = plt_args,
#         name = ctx.label.name,
#         dirs = " ".join(dirs),
#         opts = " ".join(ctx.attr.dialyzer_opts),
#     )

    # ctx.actions.write(
    #     output = ctx.outputs.executable,
    #     content = script,
    # )

    ctx.actions.run(
        outputs = [ctx.outputs.executable],
        executable = 'dialyzer',
        arguments = [args],
        mnemonic = 'DIALYZER',
        use_default_shell_env=True,
    )

    runfiles = ctx.runfiles([ctx.file.plt] if ctx.file.plt != None else [])
    runfiles = runfiles.merge(ctx.attr.target[DefaultInfo].default_runfiles)
    return [DefaultInfo(runfiles = runfiles)]

dialyze_test = rule(
    implementation = _dialyze_impl,
    attrs = {
        "_erlang_home": attr.label(default = Label("//:erlang_home")),
        "_erlang_version": attr.label(default = Label("//:erlang_version")),
        "is_windows": attr.bool(mandatory = True),
        "plt": attr.label(
            allow_single_file = [".plt"],
        ),
        "target": attr.label(
            providers = [ErlangAppInfo],
            mandatory = True,
        ),
        "plt_apps": attr.string_list(),
        "dialyzer_opts": attr.string_list(
            default = [
                "-Werror_handling",
                "-Wrace_conditions",
                "-Wunmatched_returns",
            ],
        ),
    },
    test = True,
)

def dialyze(**kwargs):
    dialyze_test(
        name = "dialyze",
        target = ":erlang_app",
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        **kwargs
    )
