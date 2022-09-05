load(
    "//:util.bzl",
    "path_join",
)

ERLANG_HOME_ENV_VAR = "ERLANG_HOME"
DEFAULT_ERL_PATH = "/usr/bin/erl"

_EXTERNAL_ERLANG_PACKAGE = "external"

def _impl(repository_ctx):
    erlang_installations = {}

    if ERLANG_HOME_ENV_VAR in repository_ctx.os.environ:
        erlang_home = repository_ctx.os.environ[ERLANG_HOME_ENV_VAR]
    else:
        if repository_ctx.os.name.find("windows") > 0:
            erl_path = repository_ctx.which("erl.exe")
        else:
            erl_path = repository_ctx.which("erl")
        if erl_path == None:
            erl_path = repository_ctx.path(DEFAULT_ERL_PATH)
        erlang_home = str(erl_path.dirname.dirname)

    erl_path = path_join(erlang_home, "bin", "erl")
    version = repository_ctx.execute(
        [
            erl_path,
            "-noshell",
            "-eval",
            '{ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:fwrite(Version), halt().',
        ],
        timeout = 10,
    )
    if version.return_code == 0:
        erlang_version = version.stdout.strip("\n")
        (major, _, _) = erlang_version.partition(".")
        erlang_installations[_EXTERNAL_ERLANG_PACKAGE] = major
        repository_ctx.template(
            "{}/BUILD.bazel".format(_EXTERNAL_ERLANG_PACKAGE),
            Label("//repositories:BUILD_external.tpl"),
            {
                "%{ERLANG_HOME}": erlang_home,
                "%{ERLANG_VERSION}": erlang_version,
                "%{ERLANG_MAJOR}": major,
                "%{RULES_ERLANG_WORKSPACE}": repository_ctx.attr.rules_erlang_workspace,
            },
            False,
        )

    for name in repository_ctx.attr.versions.keys():
        if name == _EXTERNAL_ERLANG_PACKAGE:
            fail("'{}' is not allowed as an internal erlang name".format(
                _EXTERNAL_ERLANG_PACKAGE,
            ))

        version = repository_ctx.attr.versions.get(name)
        (major, _, _) = version.partition(".")
        erlang_installations[name] = major

        repository_ctx.template(
            "{}/BUILD.bazel".format(name),
            Label("//repositories:BUILD_internal.tpl"),
            {
                "%{ERLANG_VERSION}": version,
                "%{URL}": repository_ctx.attr.urls.get(name),
                "%{STRIP_PREFIX}": repository_ctx.attr.strip_prefixs.get(name, ""),
                "%{SHA_256}": repository_ctx.attr.sha256s.get(name, ""),
                "%{ERLANG_MAJOR}": major,
                "%{RULES_ERLANG_WORKSPACE}": repository_ctx.attr.rules_erlang_workspace,
            },
            False,
        )

    repository_ctx.file(
        "BUILD.bazel",
        _build_file_content(erlang_installations),
        False,
    )

    toolchains = [
        "@{}//{}:toolchain".format(repository_ctx.name, name)
        for name in erlang_installations.keys()
    ]

    repository_ctx.template(
        "defaults.bzl",
        Label("//repositories:defaults.bzl.tpl"),
        {
            "%{TOOLCHAINS}": "\n".join([
                '        "%s",' % t
                for t in toolchains
            ]),
        },
        False,
    )

erlang_config = repository_rule(
    implementation = _impl,
    attrs = {
        "rules_erlang_workspace": attr.string(),
        "versions": attr.string_dict(),
        "urls": attr.string_dict(),
        "strip_prefixs": attr.string_dict(),
        "sha256s": attr.string_dict(),
    },
    environ = [
        "ERLANG_HOME",
    ],
)

def _build_file_content(erlang_installations):
    if _EXTERNAL_ERLANG_PACKAGE in erlang_installations:
        default_internal_external = "external"
        default_major = erlang_installations[_EXTERNAL_ERLANG_PACKAGE]
    else:
        default_internal_external = "internal"
        default_major = erlang_installations[0]

    build_file_content = """\
package(
    default_visibility = ["//visibility:public"],
)

constraint_setting(
    name = "erlang_internal_external",
    default_constraint_value = ":erlang_{default_internal_external}",
)

constraint_setting(
    name = "erlang_major_version",
    default_constraint_value = ":erlang_{default_major}",
)

""".format(
        default_internal_external = default_internal_external,
        default_major = default_major,
    )

    should_have_external = False
    should_have_internal = False
    if _EXTERNAL_ERLANG_PACKAGE in erlang_installations:
        should_have_external = True
        should_have_internal = len(erlang_installations) > 1
    else:
        should_have_internal = len(erlang_installations) > 0

    if should_have_external:
        build_file_content += """\
constraint_value(
    name = "erlang_external",
    constraint_setting = ":erlang_internal_external",
)

"""

    if should_have_internal:
        build_file_content += """\
constraint_value(
    name = "erlang_internal",
    constraint_setting = ":erlang_internal_external",
)

"""

    unique_majors = {
        v: n
        for (n, v) in erlang_installations.items()
    }.keys()
    for major in unique_majors:
        build_file_content += """\
constraint_value(
    name = "erlang_{major}",
    constraint_setting = ":erlang_major_version",
)

platform(
    name = "erlang_{major}_platform",
    constraint_values = [
        ":erlang_{major}",
    ],
)

""".format(major = major)

    return build_file_content
