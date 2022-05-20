load(
    "@bazel_skylib//rules:common_settings.bzl",
    "BuildSettingInfo",
)
load(
    "//:util.bzl",
    "BEGINS_WITH_FUN",
    "QUERY_ERL_VERSION",
    "path_join",
)

OtpInfo = provider(
    doc = "A Home directory of a built Erlang/OTP",
    fields = ["release_dir", "erlang_home"],
)

INSTALL_PREFIX = "/tmp/bazel/erlang"

def _find_root(sources):
    dirs = [s.dirname for s in sources]
    root = dirs[0]
    for d in dirs:
        if d == "":
            fail("unexpectedly empty dirname")
        if root.startswith(d):
            root = d
        elif d.startswith(root):
            pass
        else:
            fail("{} and {} do not share a common root".format(d, root))
    return root

def _install_root(install_prefix):
    (root_dir, _, _) = install_prefix.removeprefix("/").partition("/")
    return "/" + root_dir

def _erlang_build_impl(ctx):
    release_dir = ctx.actions.declare_directory(ctx.label.name + "_release")
    build_dir = ctx.actions.declare_directory(ctx.label.name + "_build")
    build_log = ctx.actions.declare_file(ctx.label.name + "_build.log")
    symlinks_log = ctx.actions.declare_file(ctx.label.name + "_symlinks.log")

    extra_env = "".join([
        "{}={}".format(k, v)
        for (k, v) in ctx.attr.extra_env.items()
    ])

    extra_configure_opts = " ".join(ctx.attr.extra_configure_opts)

    if not ctx.attr.install_prefix.startswith("/"):
        fail("install_prefix must be absolute")
    install_path = path_join(ctx.attr.install_prefix, ctx.label.name)
    install_root = _install_root(ctx.attr.install_prefix)

    ctx.actions.run_shell(
        inputs = ctx.files.sources,
        outputs = [release_dir, build_dir, build_log, symlinks_log],
        command = """set -euo pipefail

ABS_BUILD_DIR=$PWD/{build_path}
ABS_RELEASE_DIR=$PWD/{release_path}
ABS_LOG=$PWD/{build_log}
ABS_SYMLINKS=$PWD/{symlinks_log}

cp -rp {source_path}/* $ABS_BUILD_DIR

echo "Building OTP $(cat $ABS_BUILD_DIR/OTP_VERSION) in $ABS_BUILD_DIR"

cd $ABS_BUILD_DIR
./configure --prefix={install_path} >> $ABS_LOG 2>&1
mkdir -p lib/jinterface/ebin
make  >> $ABS_LOG 2>&1
make DESTDIR=$ABS_RELEASE_DIR install >> $ABS_LOG 2>&1

mv $ABS_RELEASE_DIR{install_path}/* $ABS_RELEASE_DIR
rm -d $ABS_RELEASE_DIR{install_path}
rm -rf $ABS_RELEASE_DIR{install_root}

# bazel will not allow a symlink in the output directory with
# --remote_download_minimal, so we remove them
cd $ABS_RELEASE_DIR
find . -type l | xargs ls -ld > $ABS_SYMLINKS
find . -type l | xargs rm
""".format(
            source_path = _find_root(ctx.files.sources),
            build_path = build_dir.path,
            release_path = release_dir.path,
            install_path = install_path,
            install_root = install_root,
            build_log = build_log.path,
            symlinks_log = symlinks_log.path,
            # extra_env = extra_env,
            # extra_configure_opts = extra_configure_opts,
        ),
        mnemonic = "OTP",
        progress_message = "Compiling otp from source",
    )

    return [
        DefaultInfo(
            files = depset([release_dir]),
        ),
        OtpInfo(
            release_dir = release_dir,
            erlang_home = install_path,
        ),
    ]

erlang_build = rule(
    implementation = _erlang_build_impl,
    attrs = {
        "install_prefix": attr.string(default = INSTALL_PREFIX),
        # maybe the url should be here, not the sources. rabbitmq-downloads them even for local
        "sources": attr.label_list(allow_files = True, mandatory = True),
        "extra_env": attr.string_dict(),
        "extra_configure_opts": attr.string_list(),
    },
)

def _erlang_external_impl(ctx):
    erlang_home = ctx.attr._erlang_home[BuildSettingInfo].value
    erlang_version = ctx.attr._erlang_version[BuildSettingInfo].value

    status_file = ctx.actions.declare_file(ctx.label.name + "_status")

    ctx.actions.run_shell(
        inputs = [],
        outputs = [status_file],
        command = """set -euo pipefail

{begins_with_fun}
V=$("{erlang_home}"/bin/{query_erlang_version})
if ! beginswith "{erlang_version}" "$V"; then
echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
exit 1
fi

echo "$V" >> {status_path}
""".format(
            begins_with_fun = BEGINS_WITH_FUN,
            query_erlang_version = QUERY_ERL_VERSION,
            erlang_version = erlang_version,
            erlang_home = erlang_home,
            status_path = status_file.path,
        ),
        mnemonic = "OTP",
        progress_message = "Validating otp at {}".format(erlang_home),
    )

    return [
        DefaultInfo(
            files = depset([status_file]),
        ),
        OtpInfo(
            release_dir = None,
            erlang_home = erlang_home,
        ),
    ]

erlang_external = rule(
    implementation = _erlang_external_impl,
    attrs = {
        "_erlang_home": attr.label(default = Label("//:erlang_home")),
        "_erlang_version": attr.label(default = Label("//:erlang_version")),
    },
)
