load(
    ":hex_pm.bzl",
    "hex_package_info",
    "satisfies",
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
    ":semver.bzl",
    "lt",
    "version_from_string",
)
load(
    "//:rules_erlang.bzl",
    "xref_runner_sources",
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

def _newest(a, b):
    if a.version == b.version:
        return a

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
    elif lt(a_version, b_version):
        return b
    else:
        return a

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
            p = _newest(p, dupe)
        deduped.append(p)
    return deduped

def _erlang_package(ctx):
    xref_runner_sources()

    packages = []
    for mod in ctx.modules:
        for dep in mod.tags.hex_package_tree:
            packages.append(hex_tree(
                ctx,
                module = mod,
                name = dep.name,
                version = dep.version,
            ))
        for dep in mod.tags.hex_package:
            packages.append(hex_package(
                ctx,
                module = mod,
                name = dep.name,
                version = dep.version,
                sha256 = dep.sha256,
                build_file_content = dep.build_file_content,
                patch_cmds = dep.patch_cmds,
            ))
        for dep in mod.tags.git_package:
            packages.append(git_package(
                ctx,
                module = mod,
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
        "hex_package": hex_package_tag,
        "hex_package_tree": hex_package_tree_tag,
        "git_package": git_package_tag,
    },
)
