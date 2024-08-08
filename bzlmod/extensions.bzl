load(
    ":erlang_package.bzl",
    "git_package",
    "hex_package",
    "log",
)
load(
    ":semver.bzl",
    "compatible",
    "lt",
    "version_from_string",
)
load(
    "//:rules_erlang.bzl",
    "xref_runner_sources",
)
load(
    "//repositories:erlang_config.bzl",
    "INSTALLATION_TYPE_EXTERNAL",
    "INSTALLATION_TYPE_INTERNAL",
    _erlang_config_rule = "erlang_config",
)
load(
    "//repositories:gmake_config.bzl",
    _gmake_config_rule = "gmake_config",
)
load(
    "//repositories:erlang_packages.bzl",
    "erlang_packages",
)
load(
    "//tools:erlang.bzl",
    "DEFAULT_ERLANG_SHA256",
    "DEFAULT_ERLANG_VERSION",
)

def _erlang_config(ctx):
    types = {}
    versions = {}
    urls = {}
    strip_prefixs = {}
    sha256s = {}
    erlang_homes = {}
    pre_configure_cmdss = {}
    extra_configure_optss = {}
    post_configure_cmdss = {}
    extra_make_optss = {}
    owners_by_name = {}

    for mod in ctx.modules:
        for erlang in mod.tags.external_erlang_from_path:
            if erlang.name in types:
                fail("{} declares an erlang installation named {}, but the name is already used by {}".format(
                    mod.name,
                    erlang.name,
                    owners_by_name[erlang.name].name,
                ))
            types[erlang.name] = INSTALLATION_TYPE_EXTERNAL
            versions[erlang.name] = erlang.version
            erlang_homes[erlang.name] = erlang.erlang_home
            owners_by_name[erlang.name] = mod

        for erlang in mod.tags.internal_erlang_from_http_archive:
            if erlang.name in types:
                fail("{} declares an erlang installation named {}, but the name is already used by {}".format(
                    mod.name,
                    erlang.name,
                    owners_by_name[erlang.name].name,
                ))
            types[erlang.name] = INSTALLATION_TYPE_INTERNAL
            versions[erlang.name] = erlang.version
            urls[erlang.name] = erlang.url
            strip_prefixs[erlang.name] = erlang.strip_prefix
            sha256s[erlang.name] = erlang.sha256
            pre_configure_cmdss[erlang.name] = erlang.pre_configure_cmds
            extra_configure_optss[erlang.name] = erlang.extra_configure_opts
            post_configure_cmdss[erlang.name] = erlang.post_configure_cmds
            extra_make_optss[erlang.name] = erlang.extra_make_opts
            owners_by_name[erlang.name] = mod

        for erlang in mod.tags.internal_erlang_from_github_release:
            if erlang.name in types:
                fail("{} declares an erlang installation named {}, but the name is already used by {}".format(
                    mod.name,
                    erlang.name,
                    owners_by_name[erlang.name].name,
                ))
            url = "https://github.com/erlang/otp/releases/download/OTP-{v}/otp_src_{v}.tar.gz".format(
                v = erlang.version,
            )
            strip_prefix = "otp_src_{}".format(erlang.version)

            if erlang.version == DEFAULT_ERLANG_VERSION and erlang.sha256 == "":
                sha256 = DEFAULT_ERLANG_SHA256
            else:
                sha256 = erlang.sha256

            types[erlang.name] = INSTALLATION_TYPE_INTERNAL
            versions[erlang.name] = erlang.version
            urls[erlang.name] = url
            strip_prefixs[erlang.name] = strip_prefix
            sha256s[erlang.name] = sha256
            pre_configure_cmdss[erlang.name] = erlang.pre_configure_cmds
            extra_configure_optss[erlang.name] = erlang.extra_configure_opts
            post_configure_cmdss[erlang.name] = erlang.post_configure_cmds
            extra_make_optss[erlang.name] = erlang.extra_make_opts
            owners_by_name[erlang.name] = mod

    _erlang_config_rule(
        name = "erlang_config",
        rules_erlang_workspace = "@rules_erlang",
        types = types,
        versions = versions,
        urls = urls,
        strip_prefixs = strip_prefixs,
        sha256s = sha256s,
        erlang_homes = erlang_homes,
        pre_configure_cmdss = pre_configure_cmdss,
        extra_configure_optss = extra_configure_optss,
        post_configure_cmdss = post_configure_cmdss,
        extra_make_optss = extra_make_optss,
    )

external_erlang_from_path = tag_class(attrs = {
    "name": attr.string(),
    "version": attr.string(),
    "erlang_home": attr.string(),
})

internal_erlang_from_http_archive = tag_class(attrs = {
    "name": attr.string(),
    "version": attr.string(),
    "url": attr.string(),
    "strip_prefix": attr.string(),
    "sha256": attr.string(),
    "pre_configure_cmds": attr.string_list(),
    "extra_configure_opts": attr.string_list(),
    "post_configure_cmds": attr.string_list(),
    "extra_make_opts": attr.string_list(),
})

internal_erlang_from_github_release = tag_class(attrs = {
    "name": attr.string(
        default = "internal",
    ),
    "version": attr.string(
        default = DEFAULT_ERLANG_VERSION,
    ),
    "sha256": attr.string(),
    "pre_configure_cmds": attr.string_list(),
    "extra_configure_opts": attr.string_list(),
    "post_configure_cmds": attr.string_list(),
    "extra_make_opts": attr.string_list(),
})

erlang_config = module_extension(
    implementation = _erlang_config,
    tag_classes = {
        "external_erlang_from_path": external_erlang_from_path,
        "internal_erlang_from_http_archive": internal_erlang_from_http_archive,
        "internal_erlang_from_github_release": internal_erlang_from_github_release,
    },
)

GMAKE_DEFAULT_TOOLCHAIN_NAME = "default"

def _gmake_config(module_ctx):
    gmakes = {}

    for mod in module_ctx.modules:
        for gmake in mod.tags.gmake:
            if gmake.name not in gmakes:
                gmakes[gmake.name] = gmake.path
            else:
                module_ctx.report_progress("Ignoring duplicate gmake: {}".format(gmake))

    if "MAKE" in module_ctx.os.environ:
        gmake_path = module_ctx.os.environ["MAKE"]
        gmakes[GMAKE_DEFAULT_TOOLCHAIN_NAME] = gmake_path
        log(module_ctx, "Using gnu make from env var MAKE: {}".for_target(gmake_path))
    elif module_ctx.which("make") != None:
        gmake_path = module_ctx.which("make")
        gmakes[GMAKE_DEFAULT_TOOLCHAIN_NAME] = str(gmake_path)
        log(module_ctx, "Found gnu make at {}".format(gmake_path))
    else:
        gmakes[GMAKE_DEFAULT_TOOLCHAIN_NAME] = "make"

    _gmake_config_rule(
        name = "gmake_config",
        gmakes = gmakes,
    )

    module_ctx.extension_metadata(
        root_module_direct_deps = gmakes.keys(),
        root_module_direct_dev_deps = [],
    )

gmake = tag_class(attrs = {
    "name": attr.string(),
    "path": attr.string(),
})

gmake_config = module_extension(
    implementation = _gmake_config,
    tag_classes = {
        "gmake": gmake,
    },
)

def _newest(a, b):
    if a.version == b.version:
        if a.module.is_root:
            return a
        else:
            return b

    a_version = version_from_string(a.version)
    b_version = version_from_string(b.version)
    if a_version == None or b_version == None:
        fail("Version {dep_name}@{a_version} (required by {a_module}) & {dep_name}@{b_version} (required by {b_module}) cannot be resolved".format(
            dep_name = a.name,
            a_version = a.version,
            a_module = a.module.name,
            b_version = b.version,
            b_module = b.module.name,
        ))
    if not compatible(a_version, b_version):
        fail("Version {dep_name}@{a_version} (required by {a_module}) & {dep_name}@{b_version} (required by {b_module}) cannot be resolved (major version mismatch)".format(
            dep_name = a.name,
            a_version = a.version,
            a_module = a.module.name,
            b_version = b.version,
            b_module = b.module.name,
        ))
    if lt(a_version, b_version):
        return b
    else:
        return a

def _dedupe_by_version(packages):
    by_version = {}
    for p in packages:
        by_version[p.version] = p
    return by_version.values()

def resolve_local(ctx, packages):
    deduped = []
    packages_by_name = {}
    for p in packages:
        if p.name in packages_by_name:
            packages_by_name[p.name].append(p)
        else:
            packages_by_name[p.name] = [p]
    for (name, dupes) in packages_by_name.items():
        p = dupes[0]
        for dupe in dupes[1:]:
            p = _newest(p, dupe)
        deduped.append(p)
        filtered_dupes = _dedupe_by_version(dupes)
        if len(filtered_dupes) > 1:
            log(ctx, "Multiple versions of {} requested ({}), selecting {}".format(
                name,
                ", ".join([p.version for p in filtered_dupes]),
                p.version,
            ))
    return deduped

def _erlang_package(module_ctx):
    xref_runner_sources()

    packages = []
    for mod in module_ctx.modules:
        for dep in mod.tags.hex_package:
            if dep.build_file != None and dep.build_file_content != "":
                fail("build_file and build_file_content cannot be set simultaneously for", dep.name)
            packages.append(hex_package(
                module_ctx,
                module = mod,
                dep = dep,
            ))
        for dep in mod.tags.git_package:
            if dep.build_file != None and dep.build_file_content != "":
                fail("build_file and build_file_content cannot be set simultaneously for", dep.name)
            packages.append(git_package(
                module_ctx,
                module = mod,
                dep = dep,
            ))

    resolved = resolve_local(module_ctx, packages)

    if len(resolved) > 0:
        log(module_ctx, "Final package list:")
    for p in resolved:
        log(module_ctx, "    {}@{}".format(p.name, p.version))

    for p in resolved:
        p.f_fetch(p)

    apps = [
        p.name
        for p in resolved
        if (not p.name == "thoas_rules_erlang") and (not p.testonly)
    ]

    test_apps = [
        p.name
        for p in resolved
        if p.module.is_root and p.testonly
    ]

    # should we make one of these for every module?
    # then we can hide transitive deps...
    erlang_packages(
        name = "erlang_packages",
        apps = sorted(apps),
        test_apps = sorted(test_apps),
    )

hex_package_tag = tag_class(attrs = {
    "name": attr.string(mandatory = True),
    "pkg": attr.string(),
    "version": attr.string(mandatory = True),
    "sha256": attr.string(),
    "build_file": attr.label(),
    "build_file_content": attr.string(),
    "patches": attr.label_list(),
    "patch_args": attr.string_list(
        default = ["-p0"],
    ),
    "patch_cmds": attr.string_list(),
    "testonly": attr.bool(),
})

git_package_tag = tag_class(attrs = {
    "name": attr.string(),
    "remote": attr.string(),
    "repository": attr.string(),
    "branch": attr.string(),
    "tag": attr.string(),
    "commit": attr.string(),
    "build_file": attr.label(),
    "build_file_content": attr.string(),
    "patch_cmds": attr.string_list(),
    "testonly": attr.bool(),
})

erlang_package = module_extension(
    implementation = _erlang_package,
    tag_classes = {
        "hex_package": hex_package_tag,
        "git_package": git_package_tag,
    },
)
