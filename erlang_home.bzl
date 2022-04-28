ErlangHomeProvider = provider(
    fields = ["path"],
)

ErlangVersionProvider = provider(
    fields = ["version"],
)

def _erlang_home_impl(ctx):
    return ErlangHomeProvider(
        path = ctx.build_setting_value,
    )

erlang_home = rule(
    implementation = _erlang_home_impl,
    build_setting = config.string(flag = True),
)

def _erlang_version_impl(ctx):
    return ErlangVersionProvider(
        version = ctx.build_setting_value,
    )

erlang_version = rule(
    implementation = _erlang_version_impl,
    build_setting = config.string(flag = True),
)
