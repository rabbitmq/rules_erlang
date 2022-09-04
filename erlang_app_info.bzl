load("//private:transitions.bzl", "beam_transition")

ErlangAppInfo = provider(
    doc = "Compiled Erlang Application",
    fields = {
        "app_name": "Name of the erlang application",
        "include": "Public header files",
        "beam": "Compiled bytecode (.beam) files, or a single ebin directory",
        "priv": "Additional files",
        "license_files": "License files",
        "srcs": "Source files",
        "deps": "Runtime dependencies of the compiled sources",
    },
)

def _contains_by_app_name(dep, deps):
    for d in deps:
        if d[ErlangAppInfo].app_name == dep[ErlangAppInfo].app_name:
            # TODO: fail if name matches but they are not identical
            return True
    return False

def flat_deps(list_of_labels_providing_erlang_lib_info):
    deps = []
    for dep in list_of_labels_providing_erlang_lib_info:
        if not _contains_by_app_name(dep, deps):
            deps.append(dep)
            for t in dep[ErlangAppInfo].deps:
                if not _contains_by_app_name(t, deps):
                    deps.append(t)
    return deps

def _impl(ctx):
    compiled_files = ctx.files.app + ctx.files.beam

    deps = flat_deps(ctx.attr.deps)

    runfiles = ctx.runfiles(compiled_files + ctx.files.priv)
    for dep in ctx.attr.deps:
        runfiles = runfiles.merge(dep[DefaultInfo].default_runfiles)

    return [
        ErlangAppInfo(
            app_name = ctx.attr.app_name,
            include = ctx.files.hdrs,
            beam = compiled_files,
            priv = ctx.files.priv,
            license_files = ctx.files.license_files,
            srcs = ctx.files.srcs,
            deps = deps,
        ),
        DefaultInfo(
            files = depset(compiled_files),
            runfiles = runfiles,
        ),
    ]

_CLI_PLATFORMS = "//command_line_option:platforms"
_ORIGINAL_PLATFORMS = str(Label("//private:pre_transition_platforms"))

# this inversion might work, but then again, what's point. Not any beam
# can run anywhere, new bytecode can't run on old beam. And external
# erlang is really just an unknown at analysis time version. So
# --platforms=@erlang_config//:erlang_2x_internal is really what we
# want. What we don't care about is internal vs. external, so _maybe_
# if we brought back the split where internal/external is it's own
# constraint, or didn't have that constraint, it would be enough to
# change the host platform, not the --platforms to control
# internal/external, and --platforms=@erlang_config//:erlang_2x_platform
# would just choose the version. We would have to drop the :erlang_home/
# :erlang_version though, or figure out how to read the setting in
# the repository rule. However, that still presents a problem where if
# we don't somehow select the internal erlang, it will just fail when
# run remotely. We have to know at analysis time and assume internal or
# external based on the host platform for things to work.
# repository rules get run on the local machine, so we can't check at
# automatically at analysis time what erlang version is on the path
# remotely.
# But if we could do that, then maybe a split local/remote would work,
# if internal/external was implied by the host platform, and the target
# platform was only the erlang version. I the past we basically
# required an explict erlang version prefix in all cases, so we could
# keep it, and validate it during the build as always. But it would be
# nice just to be able to read the build setting during analysis.
# What is interesting is that to cross compile on rbe, we set --cpu=k8,
# even though `bazel query @rbe//config:platform --output=build` shows
# `@platforms//cpu:x86_64`, and using the os as the constraint seems
# wrong. This kind of means that for remote execution, we need to hint
# or request internal or external erlang, if we want external to be
# an option. Otherwise we just have to force internal. Though maybe a
# transition on the erlang_app_info rule could drop the internal/external
# contstraint? No, we have to remember that it seems to be resolved in
# reverse, so that is a not care, which we already propose as the target
# platform. So maybe that is it, internal/external is a separate constraint,
# the toolchains repect it, but we don't set it in the target unless we
# want to force it. I guess maybe we could make a child of the remote
# platform that sets the internal constraint, and use that as the host.
# It can default to external for local execution. A child of the local with
# internal could be generated too.
# So let's try that. Internal/external is a host constraint, and the
# erlang major is a target constraint. Now we just need a way to determine
# the external erlang version at analysis time...
#
# We could also set environ on the erlang_config rule, and use env vars
# for ERLANG_HOME and ERLANG_VERSION, instead of the build settings.
# A breaking change, but could be worth it.
#
# Maybe we could use ---experimental_repo_remote_exec and remotable=True,
# to have erlang_config execute remote and detect the remote erlang?

def _conditional_beam_transition(settings, attr):
    _ignore = (settings, attr)
    print(_ignore)
    if attr.platform_independent:
        original_platforms = [str(p) for p in settings[_CLI_PLATFORMS]]
        platforms = ["@erlang_config//:beam"]
    else:
        platforms = settings[_CLI_PLATFORMS]
        original_platforms = settings[_ORIGINAL_PLATFORMS]
    return {
        _CLI_PLATFORMS: platforms,
        _ORIGINAL_PLATFORMS: original_platforms,
    }

conditional_beam_transition = transition(
    implementation = _conditional_beam_transition,
    inputs = [_CLI_PLATFORMS, _ORIGINAL_PLATFORMS],
    outputs = [_CLI_PLATFORMS, _ORIGINAL_PLATFORMS],
)

def _restore_platforms_transition(settings, attr):
    _ignore = (settings, attr)
    print(_ignore)
    if settings[_CLI_PLATFORMS] == ["@erlang_config//:beam"]:
        platforms = settings[_ORIGINAL_PLATFORMS]
        original_platforms = []
    else:
        platforms = settings[_CLI_PLATFORMS]
        original_platforms = settings[_ORIGINAL_PLATFORMS]
    return {
        _CLI_PLATFORMS: platforms,
        _ORIGINAL_PLATFORMS: original_platforms,
    }

restore_platforms_transition = transition(
    implementation = _restore_platforms_transition,
    inputs = [_CLI_PLATFORMS, _ORIGINAL_PLATFORMS],
    outputs = [_CLI_PLATFORMS, _ORIGINAL_PLATFORMS],
)

# so, the transitions haven't worked out as I originally expected.
# have to think about this as a graph. By putting the beam_transition
# on the beam attr, we are saying beam from any platform will do, but
# that basically removes the constraints on toolchain resolution for
# those beam files, and they try to build with the external. Or, if
# we disable the external toolchain, it fails because it can't find a
# toolchain for @erlang_config//:beam, which which happens whether or
# not there is a default for the erlang_major_version constraint.
# So it means beam files actually do need to have a config on them
# matching the host platform (anything with a toolchain does). Things
# in-between maybe don't, but on the other side of them, we would
# need an inversion transition to put the platform back.

# https://github.com/fmeum/rules_jni/blob/42f4f91c364ce545bc57023992baa0a2110483b2/jni/internal/transitions.bzl

erlang_app_info = rule(
    implementation = _impl,
    # cfg = conditional_beam_transition,
    attrs = {
        "app_name": attr.string(mandatory = True),
        "hdrs": attr.label_list(allow_files = [".hrl"]),
        "app": attr.label(allow_files = [".app"]),
        "beam": attr.label_list(
            allow_files = [".beam", ".appup"],
            cfg = restore_platforms_transition,
        ),
        "priv": attr.label_list(allow_files = True),
        "license_files": attr.label_list(allow_files = True),
        "srcs": attr.label_list(allow_files = True),
        "deps": attr.label_list(providers = [ErlangAppInfo]),
        "platform_independent": attr.bool(default = True),
        # This attribute is required to use starlark transitions. It allows
        # allowlisting usage of this rule. For more information, see
        # https://docs.bazel.build/versions/master/skylark/config.html#user-defined-transitions
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist",
        ),
    },
)
