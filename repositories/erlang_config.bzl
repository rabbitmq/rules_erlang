load(
    "//:util.bzl",
    "path_join",
)

ERLANG_HOME_ENV_VAR = "ERLANG_HOME"

_DEFAULT_EXTERNAL_ERLANG_PACKAGE_NAME = "external"
_ERLANG_VERSION_UNKNOWN = "UNKNOWN"

INSTALLATION_TYPE_EXTERNAL = "external"
INSTALLATION_TYPE_INTERNAL = "internal"

def _parse_maybe_semver(version_string):
    parts = version_string.split(".", 2)
    if len(parts) > 1:
        return (parts[0], parts[1])
    else:
        return (parts[0], None)

def _impl(repository_ctx):
    rules_erlang_workspace = repository_ctx.attr.rules_erlang_workspace

    erlang_installations = _default_erlang_dict(repository_ctx)
    for name in repository_ctx.attr.types.keys():
        if name == _DEFAULT_EXTERNAL_ERLANG_PACKAGE_NAME:
            fail("'{}' is reserved as an erlang name".format(
                _DEFAULT_EXTERNAL_ERLANG_PACKAGE_NAME,
            ))
        version = repository_ctx.attr.versions[name]
        (major, minor) = _parse_maybe_semver(version)
        erlang_installations[name] = struct(
            type = repository_ctx.attr.types[name],
            version = version,
            major = major,
            minor = minor,
            url = repository_ctx.attr.urls.get(name, None),
            strip_prefix = repository_ctx.attr.strip_prefixs.get(name, None),
            sha256 = repository_ctx.attr.sha256s.get(name, None),
            erlang_home = repository_ctx.attr.erlang_homes.get(name, None),
        )

    for (name, props) in erlang_installations.items():
        target_compatible_with = ["//:erlang_{}".format(props.major)]
        if props.minor != None:
            target_compatible_with.append(
                "//:erlang_{}_{}".format(props.major, props.minor),
            )
        target_compatible_with = "".join([
            "\n        \"%s\"," % c
            for c in target_compatible_with
        ])

        if props.type == INSTALLATION_TYPE_EXTERNAL:
            repository_ctx.template(
                "{}/BUILD.bazel".format(name),
                Label("//repositories:BUILD_external.tpl"),
                {
                    "%{ERLANG_HOME}": props.erlang_home,
                    "%{ERLANG_VERSION}": props.version,
                    "%{TARGET_COMPATIBLE_WITH}": target_compatible_with,
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
                    "%{TARGET_COMPATIBLE_WITH}": target_compatible_with,
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

def _erlang_home_from_erl_path(repository_ctx, erl_path):
    ehr = repository_ctx.execute(
        [
            erl_path,
            "-noshell",
            "-eval",
            'io:format("~s",[code:root_dir()]), halt().',
        ],
        timeout = 10,
    )
    if ehr.return_code == 0:
        erlang_home = ehr.stdout.strip("\n")
    else:
        erlang_home = str(erl_path.dirname.dirname)
    return erlang_home

def _is_windows(repository_ctx):
    return repository_ctx.os.name.lower().find("windows") != -1

def _default_erlang_dict(repository_ctx):
    if _is_windows(repository_ctx):
        if ERLANG_HOME_ENV_VAR in repository_ctx.os.environ:
            erlang_home = repository_ctx.os.environ[ERLANG_HOME_ENV_VAR]
            erlang_home = erlang_home.replace("\\", "/")
            erl_path = path_join(erlang_home, "bin", "erl.exe")
        else:
            erl_path = repository_ctx.which("erl.exe")
            if erl_path == None:
                erl_path = repository_ctx.path("C:/Program Files/Erlang OTP/bin/erl.exe")
            erlang_home = _erlang_home_from_erl_path(repository_ctx, erl_path)
    elif ERLANG_HOME_ENV_VAR in repository_ctx.os.environ:
        erlang_home = repository_ctx.os.environ[ERLANG_HOME_ENV_VAR]
        erl_path = path_join(erlang_home, "bin", "erl")
    else:
        erl_path = repository_ctx.which("erl")
        if erl_path == None:
            erl_path = repository_ctx.path("/usr/bin/erl")
        erlang_home = _erlang_home_from_erl_path(repository_ctx, erl_path)

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
        (major, minor) = _parse_maybe_semver(version)
        return {
            _DEFAULT_EXTERNAL_ERLANG_PACKAGE_NAME: struct(
                type = INSTALLATION_TYPE_EXTERNAL,
                version = version,
                major = major,
                minor = minor,
                erlang_home = erlang_home,
            ),
        }
    else:
        repository_ctx.report_progress("Could not determine erlang version for {}: {}".format(
            erl_path,
            version.stderr,
        ))
        return {
            _DEFAULT_EXTERNAL_ERLANG_PACKAGE_NAME: struct(
                type = INSTALLATION_TYPE_EXTERNAL,
                version = _ERLANG_VERSION_UNKNOWN,
                major = _ERLANG_VERSION_UNKNOWN.lower(),
                minor = None,
                erlang_home = erlang_home,
            ),
        }

def _build_file_content(erlang_installations):
    default_installation = erlang_installations[_DEFAULT_EXTERNAL_ERLANG_PACKAGE_NAME]

    build_file_content = """\
load("@bazel_skylib//lib:selects.bzl", "selects")

package(
    default_visibility = ["//visibility:public"],
)

constraint_setting(
    name = "erlang_internal_external",
    default_constraint_value = ":erlang_external",
)

constraint_setting(
    name = "erlang_major_version",
    default_constraint_value = ":erlang_{default_major}",
)

constraint_setting(
    name = "erlang_major_minor_version",
    default_constraint_value = ":erlang_{default_major}_{default_minor}",
)

constraint_value(
    name = "erlang_external",
    constraint_setting = ":erlang_internal_external",
)

""".format(
        default_major = default_installation.major,
        default_minor = default_installation.minor,
    )

    external_installations = {
        name: props
        for (name, props) in erlang_installations.items()
        if props.type == INSTALLATION_TYPE_EXTERNAL
    }
    if len(erlang_installations) > len(external_installations):
        build_file_content += """\
constraint_value(
    name = "erlang_internal",
    constraint_setting = ":erlang_internal_external",
)

"""

    major_by_minors = {}
    for (name, props) in erlang_installations.items():
        minors = major_by_minors.get(props.major, [])
        if props.minor != None and props.minor not in minors:
            minors.append(props.minor)
        major_by_minors[props.major] = minors

    for (major, minors) in major_by_minors.items():
        build_file_content += """\
constraint_value(
    name = "erlang_{major}",
    constraint_setting = ":erlang_major_version",
)

""".format(
            major = major,
        )
        if len(minors) > 0:
            constraints = [
                ":erlang_{}_{}".format(major, minor)
                for minor in minors
            ]

            build_file_content += """\
selects.config_setting_group(
    name = "erlang_{major}_any_minor",
    match_any = {constraints},
)

platform(
    name = "erlang_{major}_platform",
    constraint_values = [
        ":erlang_{major}",
        ":erlang_{major}_any_minor",
    ],
)

""".format(
                major = major,
                constraints = constraints,
            )
        else:
            build_file_content += """\
platform(
    name = "erlang_{major}_platform",
    constraint_values = [
        ":erlang_{major}",
    ],
)

""".format(
                major = major,
            )

        for minor in minors:
            if minor != None:
                build_file_content += """\
constraint_value(
    name = "erlang_{major}_{minor}",
    constraint_setting = ":erlang_major_minor_version",
)

platform(
    name = "erlang_{major}_{minor}_platform",
    constraint_values = [
        ":erlang_{major}",
        ":erlang_{major}_{minor}",
    ],
)

""".format(major = major, minor = minor)

    return build_file_content
