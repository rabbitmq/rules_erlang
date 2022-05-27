load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)
load(
    ":hex_pm.bzl",
    "hex_package_info",
    "newest",
    "satisfies",
)
load(
    ":otp.bzl",
    "OTP_BUILD_FILE_CONTENT",
    "merge_archive",
)
load(
    ":erlang_package.bzl",
    "git_package",
    "hex_package",
    "hex_tree",
    "log",
    "without_requirement",
)
load(
    "//:rules_erlang.bzl",
    "xref_runner_sources",
)
load(
    "//tools:erlang.bzl",
    "DEFAULT_ERLANG_SHA256",
    "DEFAULT_ERLANG_VERSION",
)

_RESOLVE_MAX_PASSES = 500

def _resolve_pass(ctx, packages):
    all_requirements = []
    for p in packages:
        # print("checking reqs for", p.name)
        all_requirements.extend(getattr(p, "requirements", []))
    if len(all_requirements) == 0:
        # no unmet requirements, end recursion
        return (True, packages)
    else:
        # at least one unmet requirement exists...
        name = all_requirements[0]["app"]
        requirements = []
        for r in all_requirements:
            if r["app"] == name:
                requirements.append(r["requirement"])
        requirers = []
        for p in packages:
            for r in getattr(p, "requirements", []):
                if r["app"] == name:
                    requirers.append(p.name)

        # check if it's already in our package list
        for p in packages:
            if p.name == name:
                if not all([satisfies(p.version, r) for r in requirements]):
                    log(ctx, "Ignoring conflicting requirements for {}, {} does not satisfy {} as required by {}".format(
                        name,
                        p.version,
                        requirements,
                        requirers,
                    ))
                return (False, [without_requirement(name, p) for p in packages])

        # check if a version exists on hex
        log(ctx, "Fetching package info for {} from hex.pm".format(name))
        package_info = hex_package_info(ctx, name)
        for release in package_info["releases"]:
            if all([satisfies(release["version"], r) for r in requirements]):
                log(ctx, "Using {}@{} required by {} satisfying {}".format(
                    name,
                    release["version"],
                    requirers,
                    requirements,
                ))
                hp = hex_package(ctx, name, release["version"], "", "")
                return (False, [without_requirement(name, p) for p in packages] + [hp])

        fail("Unable to find a version of {} satisfying".format(name), requirements)

def _resolve_hex_pm(ctx, packages):
    resolved = packages
    for i in range(0, _RESOLVE_MAX_PASSES):
        (done, resolved) = _resolve_pass(ctx, resolved)
        if done:
            return resolved
    fail("Dependencies were not resolved after {} passes.".format(_RESOLVE_MAX_PASSES))

def _resolve_local(packages):
    deduped = []
    packages_by_name = {}
    for p in packages:
        if p.name in packages_by_name:
            packages_by_name[p.name].append(p)
        else:
            packages_by_name[p.name] = [p]
    for (_, dupes) in packages_by_name.items():
        p = dupes[0]
        for dupe in dupes[1:]:
            p = newest(p, dupe)
        deduped.append(p)
    return deduped

def _erlang_package(ctx):
    otp_archives = []
    for mod in ctx.modules:
        for archive in mod.tags.otp_http_archive:
            props = {
                "name": archive.name,
                "url": archive.url,
                "strip_prefix": archive.strip_prefix,
                "sha256": archive.sha256,
                "extra_configure_opts": archive.extra_configure_opts,
                "post_configure_cmds": archive.post_configure_cmds,
            }
            otp_archives = merge_archive(props, otp_archives)
        for archive in mod.tags.otp_default:
            url = "https://github.com/erlang/otp/releases/download/OTP-{v}/otp_src_{v}.tar.gz".format(
                v = DEFAULT_ERLANG_VERSION,
            )
            props = {
                "name": "otp_default",
                "url": url,
                "strip_prefix": "otp_src_{}".format(DEFAULT_ERLANG_VERSION),
                "sha256": DEFAULT_ERLANG_SHA256,
                "extra_configure_opts": [],
                "post_configure_cmds": [
                    "mkdir -p lib/jinterface/ebin",
                ],
            }
            otp_archives = merge_archive(props, otp_archives)
        for release in mod.tags.otp_github_release:
            if release.name != "":
                name = release.name
            else:
                (major, _, _) = release.version.partition(".")
                name = "otp_{}".format(major)
            url = "https://github.com/erlang/otp/releases/download/OTP-{v}/otp_src_{v}.tar.gz".format(
                v = release.version,
            )
            props = {
                "name": name,
                "url": url,
                "strip_prefix": "otp_src_{}".format(release.version),
                "sha256": release.sha256,
                "extra_configure_opts": release.extra_configure_opts,
                "post_configure_cmds": release.post_configure_cmds,
            }
            otp_archives = merge_archive(props, otp_archives)

    if len(otp_archives) > 0:
        log(ctx, "Final OTP list:")
    for props in otp_archives:
        log(ctx, "    {} -> {}".format(props["name"], props["url"]))

    for props in otp_archives:
        extra_configure_opts = props.pop("extra_configure_opts")
        post_configure_cmds = props.pop("post_configure_cmds")
        http_archive(
            build_file_content = OTP_BUILD_FILE_CONTENT.format(
                extra_configure_opts = extra_configure_opts,
                post_configure_cmds = post_configure_cmds,
            ),
            **props
        )

    xref_runner_sources()

    otp_installation_names = [
        props["name"]
        for props in otp_archives
    ]

    packages = []
    for mod in ctx.modules:
        for dep in mod.tags.hex_package_tree:
            packages.append(hex_tree(
                ctx,
                otp_installation_names = otp_installation_names,
                name = dep.name,
                version = dep.version,
            ))
        for dep in mod.tags.hex_package:
            packages.append(hex_package(
                ctx,
                otp_installation_names = otp_installation_names,
                name = dep.name,
                version = dep.version,
                sha256 = dep.sha256,
                build_file_content = dep.build_file_content,
                patch_cmds = dep.patch_cmds,
            ))
        for dep in mod.tags.git_package:
            packages.append(git_package(
                ctx,
                otp_installation_names = otp_installation_names,
                dep = dep,
            ))

    deduped = _resolve_local(packages)

    resolved = _resolve_hex_pm(ctx, deduped)

    if len(resolved) > 0:
        log(ctx, "Final package list:")
    for p in resolved:
        log(ctx, "    {}@{}".format(p.name, p.version))

    for p in resolved:
        p.f_fetch(ctx, p)

otp_default_tag = tag_class()

otp_http_archive_tag = tag_class(attrs = {
    "name": attr.string(),
    "url": attr.string(),
    "strip_prefix": attr.string(),
    "sha256": attr.string(),
    "extra_configure_opts": attr.string_list(),
    "post_configure_cmds": attr.string_list(),
})

otp_github_release_tag = tag_class(attrs = {
    "name": attr.string(),
    "version": attr.string(),
    "sha256": attr.string(),
    "extra_configure_opts": attr.string_list(),
    "post_configure_cmds": attr.string_list(),
})

hex_package_tree_tag = tag_class(attrs = {
    "name": attr.string(mandatory = True),
    "version": attr.string(mandatory = True),
})

hex_package_tag = tag_class(attrs = {
    "name": attr.string(mandatory = True),
    "version": attr.string(mandatory = True),
    "sha256": attr.string(),
    "build_file_content": attr.string(),
    "patch_cmds": attr.string_list(),
})

git_package_tag = tag_class(attrs = {
    "name": attr.string(),
    "remote": attr.string(),
    "repository": attr.string(),
    "branch": attr.string(),
    "tag": attr.string(),
    "commit": attr.string(),
    "build_file_content": attr.string(),
    "patch_cmds": attr.string_list(),
})

erlang_package = module_extension(
    implementation = _erlang_package,
    tag_classes = {
        "otp_default": otp_default_tag,
        "otp_http_archive": otp_http_archive_tag,
        "otp_github_release": otp_github_release_tag,
        "hex_package": hex_package_tag,
        "hex_package_tree": hex_package_tree_tag,
        "git_package": git_package_tag,
    },
)
