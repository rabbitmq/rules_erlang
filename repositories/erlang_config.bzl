load(
    "//:util.bzl",
    "path_join",
)

ERLANG_HOME_ENV_VAR = "ERLANG_HOME"
DEFAULT_ERL_PATH = "/usr/bin/erl"

_DEFAULT_EXTERNAL_ERLANG_PACKAGE_NAME = "external"

INSTALLATION_TYPE_EXTERNAL = "external"
INSTALLATION_TYPE_INTERNAL = "internal"

def _impl(repository_ctx):
    rules_erlang_workspace = repository_ctx.attr.rules_erlang_workspace

    erlang_installations = _default_erlang_dict(repository_ctx)
    for name in repository_ctx.attr.types.keys():
        if name == _DEFAULT_EXTERNAL_ERLANG_PACKAGE_NAME:
            fail("'{}' is reserved as an erlang name".format(
                _DEFAULT_EXTERNAL_ERLANG_PACKAGE_NAME,
            ))
        version = repository_ctx.attr.versions[name]
        (major, _, _) = version.partition(".")
        erlang_installations[name] = struct(
            type = repository_ctx.attr.types[name],
            version = version,
            major = major,
            url = repository_ctx.attr.urls.get(name, None),
            strip_prefix = repository_ctx.attr.strip_prefixs.get(name, None),
            sha256 = repository_ctx.attr.sha256s.get(name, None),
            erlang_home = repository_ctx.attr.erlang_homes.get(name, None),
        )

    for (name, props) in erlang_installations.items():
        if props.type == INSTALLATION_TYPE_EXTERNAL:
            repository_ctx.template(
                "{}/BUILD.bazel".format(name),
                Label("//repositories:BUILD_external.tpl"),
                {
                    "%{ERLANG_HOME}": props.erlang_home,
                    "%{ERLANG_VERSION}": props.version,
                    "%{ERLANG_MAJOR}": props.major,
                    "%{RULES_ERLANG_WORKSPACE}": rules_erlang_workspace,
                },
                False,
            )
        else:
            repository_ctx.template(
                "{}/BUILD.bazel".format(name),
                Label("//repositories:BUILD_internal.tpl"),
                {
                    "%{ERLANG_VERSION}": props.version,
                    "%{URL}": props.url,
                    "%{STRIP_PREFIX}": props.strip_prefix or "",
                    "%{SHA_256}": props.sha256 or "",
                    "%{ERLANG_MAJOR}": props.major,
                    "%{RULES_ERLANG_WORKSPACE}": rules_erlang_workspace,
                },
                False,
            )

    if len(erlang_installations) == 0:
        fail("No erlang installations configured")

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
        "types": attr.string_dict(),
        "versions": attr.string_dict(),
        "urls": attr.string_dict(),
        "strip_prefixs": attr.string_dict(),
        "sha256s": attr.string_dict(),
        "erlang_homes": attr.string_dict(),
    },
    environ = [
        "ERLANG_HOME",
    ],
)

def _default_erlang_dict(repository_ctx):
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
        version = version.stdout.strip("\n")
        (major, _, _) = version.partition(".")
        return {
            _DEFAULT_EXTERNAL_ERLANG_PACKAGE_NAME: struct(
                type = INSTALLATION_TYPE_EXTERNAL,
                version = version,
                major = major,
                erlang_home = erlang_home,
            ),
        }
    else:
        return {}

def _build_file_content(erlang_installations):
    external_installations = {
        name: props
        for (name, props) in erlang_installations.items()
        if props.type == INSTALLATION_TYPE_EXTERNAL
    }

    if _DEFAULT_EXTERNAL_ERLANG_PACKAGE_NAME in erlang_installations:
        default_installation = erlang_installations[_DEFAULT_EXTERNAL_ERLANG_PACKAGE_NAME]
    elif len(external_installations) > 0:
        default_installation = external_installations[0]
    else:
        default_installation = erlang_installations[0]

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
        default_internal_external = default_installation.type,
        default_major = default_installation.major,
    )

    should_have_external = len(external_installations) > 0
    should_have_internal = len(erlang_installations) > len(external_installations)

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
        props.major: name
        for (name, props) in erlang_installations.items()
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
