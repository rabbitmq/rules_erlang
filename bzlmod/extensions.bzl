load(
    "//:rules_erlang.bzl",
    _rules_erlang_dependencies = "rules_erlang_dependencies",
)
load("//:hex_archive.bzl", "hex_archive")
load(
    ":hex_pm.bzl",
    "hex_package_info",
    "hex_release_info",
    "newest",
    "satisfies",
)
load(
    "@bazel_tools//tools/build_defs/repo:git.bzl",
    "git_repository",
    "new_git_repository",
)

def _rules_erlang_deps(ctx):
    _rules_erlang_dependencies()

rules_erlang_dependencies = module_extension(
    implementation = _rules_erlang_deps,
)

HexPackage = provider(fields = [
    "name",
    "version",
    "sha256",
    "build_file_content",
    "patch_cmds",
    "deps",
    "requirements",
    "f_fetch",
])

GitPackage = provider(fields = [
    "name",
    "version",
    "remote",
    "repository",
    "branch",
    "tag",
    "commit",
    "build_file_content",
    "patch_cmds",
    "f_fetch",
])

_RESOLVE_MAX_PASSES = 500

def _hex_package_repo(ctx, hex_package):
    if hex_package.build_file_content != "":
        hex_archive(
            name = hex_package.name,
            package_name = hex_package.name,
            version = hex_package.version,
            sha256 = hex_package.sha256,
            build_file_content = hex_package.build_file_content,
            patch_cmds = hex_package.patch_cmds,
        )
    else:
        if hex_package.deps != None:
            deps = ["@{}//:erlang_app".format(d) for d in hex_package.deps]
        else:
            deps = ""

        hex_archive(
            name = hex_package.name,
            package_name = hex_package.name,
            version = hex_package.version,
            sha256 = hex_package.sha256,
            patch_cmds = hex_package.patch_cmds + [PATCH_AUTO_BUILD_BAZEL.format(
                name = hex_package.name,
                version = hex_package.version,
                deps = deps,
                make = ctx.which("make"),
            )],
        )

def _git_package_repo(ctx, git_package):
    if git_package.build_file_content != "":
        new_git_repository(
            name = git_package.name,
            remote = git_package.remote,
            branch = git_package.branch,
            tag = git_package.tag,
            commit = git_package.commit,
            build_file_content = git_package.build_file_content,
            patch_cmds = git_package.patch_cmds,
        )
    else:
        git_repository(
            name = git_package.name,
            remote = git_package.remote,
            branch = git_package.branch,
            tag = git_package.tag,
            commit = git_package.commit,
            patch_cmds = git_package.patch_cmds + [PATCH_AUTO_BUILD_BAZEL.format(
                name = git_package.name,
                version = "",
                deps = [],
                make = ctx.which("make"),
            )],
        )

def _hex_tree(ctx, name, version):
    _log(ctx, "Fetching release info for {}@{} from hex.pm".format(name, version))
    release_info = hex_release_info(ctx, name, version)

    sha256 = release_info["checksum"]

    deps = []
    requirements = []
    for (_, props) in release_info["requirements"].items():
        if not props["optional"]:
            deps.append(props["app"])
            requirements.append(props)

    return HexPackage(
        name = name,
        version = version,
        sha256 = sha256,
        build_file_content = "",
        patch_cmds = [],
        deps = deps,
        requirements = requirements,
        f_fetch = _hex_package_repo,
    )

def _hex_package(
        ctx,
        name = None,
        version = None,
        sha256 = None,
        build_file_content = None,
        patch_cmds = None):
    return HexPackage(
        name = name,
        version = version,
        sha256 = sha256,
        build_file_content = build_file_content,
        patch_cmds = patch_cmds,
        deps = None,
        requirements = [],
        f_fetch = _hex_package_repo,
    )

def _infer_app_name(remote):
    (_, _, repo) = remote.rpartition("/")
    if repo == remote:
        fail("Could not extract erlang app name from {}".format(remote))
    if not repo.endswith(".git"):
        fail("Could not extract erlang app name from {}".format(remote))
    return repo[0:-4]

def _git_package(ctx, dep):
    if dep.remote != "" and dep.repository != "":
        fail("'remote' and 'repository' are mutually exclusive options")

    if dep.repository != "":
        remote = "https://github.com/{}.git".format(dep.repository)
    elif dep.remote != "":
        remote = dep.remote
    else:
        fail("either 'remote' or 'repository' are required")

    if dep.name != "":
        name = dep.name
    else:
        name = _infer_app_name(remote)

    if dep.commit != "":
        version = dep.commit
    elif dep.tag != "":
        version = dep.tag
    else:
        version = dep.branch

    return GitPackage(
        name = name,
        version = version,
        remote = remote,
        branch = dep.branch,
        tag = dep.tag,
        commit = dep.commit,
        build_file_content = dep.build_file_content,
        patch_cmds = dep.patch_cmds,
        f_fetch = _git_package_repo,
    )

def _without_requirement(name, package):
    requirements = getattr(package, "requirements", [])
    if len(requirements) == 0:
        return package
    else:
        # currently only HexPackage has "requirements", so we
        # can always return one
        new_requirements = []
        for r in requirements:
            if r["app"] != name:
                new_requirements.append(r)

        return HexPackage(
            name = package.name,
            version = package.version,
            sha256 = package.sha256,
            build_file_content = package.build_file_content,
            patch_cmds = package.patch_cmds,
            deps = package.deps,
            requirements = new_requirements,
            f_fetch = package.f_fetch,
        )

def _log(ctx, msg):
    ctx.execute([ctx.which("echo"), "RULES_ERLANG: " + msg], timeout = 1, quiet = False)

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
                    _log(ctx, "Ignoring conflicting requirements for {}, {} does not satisfy {} as required by {}".format(
                        name,
                        p.version,
                        requirements,
                        requirers,
                    ))
                return (False, [_without_requirement(name, p) for p in packages])

        # check if a version exists on hex
        _log(ctx, "Fetching package info for {} from hex.pm".format(name))
        package_info = hex_package_info(ctx, name)
        for release in package_info["releases"]:
            if all([satisfies(release["version"], r) for r in requirements]):
                _log(ctx, "Using {}@{} required by {} satisfying {}".format(
                    name,
                    release["version"],
                    requirers,
                    requirements,
                ))
                hp = _hex_package(ctx, name, release["version"], "", "")
                return (False, [_without_requirement(name, p) for p in packages] + [hp])

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
    packages = []
    for mod in ctx.modules:
        for dep in mod.tags.hex_tree:
            packages.append(_hex_tree(ctx, dep.name, dep.version))
        for dep in mod.tags.hex:
            packages.append(_hex_package(
                ctx,
                name = dep.name,
                version = dep.version,
                sha256 = dep.sha256,
                build_file_content = dep.build_file_content,
                patch_cmds = dep.patch_cmds,
            ))
        for dep in mod.tags.git:
            packages.append(_git_package(ctx, dep))

    deduped = _resolve_local(packages)

    resolved = _resolve_hex_pm(ctx, deduped)

    _log(ctx, "Final package list:")
    for p in resolved:
        _log(ctx, "    {}@{}".format(p.name, p.version))

    for p in resolved:
        p.f_fetch(ctx, p)

hex_tree = tag_class(attrs = {
    "name": attr.string(mandatory = True),
    "version": attr.string(mandatory = True),
})

hex = tag_class(attrs = {
    "name": attr.string(mandatory = True),
    "version": attr.string(mandatory = True),
    "sha256": attr.string(),
    "build_file_content": attr.string(),
    "patch_cmds": attr.string_list(),
})

git = tag_class(attrs = {
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
        "hex": hex,
        "hex_tree": hex_tree,
        "git": git,
    },
)

PATCH_AUTO_BUILD_BAZEL = """set -euo pipefail

echo "Generating BUILD.bazel for {name}..."

# if there is a Makefile and erlang.mk, use make to infer
# the version and deps, error on name mismatch, and error
# if the deps mismatch
if [ ! -f BUILD.bazel ]; then
    if [ -f Makefile ]; then
        if [ -f erlang.mk ]; then
            if [ -n "{make}" ]; then
                echo "\tAttempting auto-configure from erlang.mk files..."

                cat << 'MK' > bazel-autobuild.mk
define BUILD_FILE_CONTENT
load("@rules_erlang//:erlang_app.bzl", "erlang_app")

erlang_app(
    app_name = "$(PROJECT)",
    app_description = \"""$(PROJECT_DESCRIPTION)\""",
    app_version = "$(PROJECT_VERSION)",
    app_env = \"""$(PROJECT_ENV)\""",
    app_extra = \"""$(PROJECT_APP_EXTRA_KEYS)\""",
    extra_apps = [
$(foreach dep,$(LOCAL_DEPS),        "$(dep)",\n)    ],
    erlc_opts = [
        "+deterministic",
        "+debug_info",
    ],
    build_deps = [
$(foreach dep,$(BUILD_DEPS),        "@$(dep)//:erlang_app",\n)    ],
    deps = [
$(foreach dep,$(DEPS),        "@$(dep)//:erlang_app",\n)    ],
    stamp = 0,
)
endef

export BUILD_FILE_CONTENT

BUILD.bazel:
	echo "$$BUILD_FILE_CONTENT" >> $@
MK
                cat bazel-autobuild.mk
                {make} -f Makefile -f bazel-autobuild.mk BUILD.bazel
            else
                echo "Skipping erlang.mk import as make is unavailable"
            fi
        fi
    fi
fi

# fallback to BUILD file with just the name, version & deps
if [ ! -f BUILD.bazel ]; then
    if [ -n "{version}" ]; then
        if [ -n "{deps}" ]; then
            cat << EOF > BUILD.bazel
load("@rules_erlang//:erlang_app.bzl", "erlang_app")

erlang_app(
    app_name = "{name}",
    app_version = "{version}",
    erlc_opts = [
        "+deterministic",
        "+debug_info",
    ],
    deps = {deps},
    stamp = 0,
)
EOF
        fi
    fi
fi

# fallback to BUILD file with just the name and version
if [ ! -f BUILD.bazel ]; then
    if [ -n "{version}" ]; then
        cat << EOF > BUILD.bazel
load("@rules_erlang//:erlang_app.bzl", "erlang_app")

erlang_app(
    app_name = "{name}",
    app_version = "{version}",
    erlc_opts = [
        "+deterministic",
        "+debug_info",
    ],
    stamp = 0,
)
EOF
    fi
fi

# fallback to BUILD file with just the name
if [ ! -f BUILD.bazel ]; then
    cat << EOF > BUILD.bazel
load("@rules_erlang//:erlang_app.bzl", "erlang_app")

erlang_app(
    app_name = "{name}",
    erlc_opts = [
        "+deterministic",
        "+debug_info",
    ],
    stamp = 0,
)
EOF
fi
"""

PATCH_AUTO_BUILD_BAZEL_WINDOWS = """REM bzlmod+windows dependency autobuild not yet supported
REM you may use 'build_file_content' instead
EXIT /B 1
"""
