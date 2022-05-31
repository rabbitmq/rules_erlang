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
    fields = {
        "release_dir": """Directory containing a built erlang.
If this value is not None, it must be symlinked into
erlang_home and used from there, as erlang installations
are not relocatable.""",
        "erlang_home": """Absolute path to the erlang
installation""",
        "version_file": """A file containing the version of this
erlang, used to correctly invalidate the cache when an
external erlang is used""",
    },
)

INSTALL_PREFIX = "/tmp/bazel/erlang"

def _install_root(install_prefix):
    (root_dir, _, _) = install_prefix.removeprefix("/").partition("/")
    return "/" + root_dir

def _erlang_build_impl(ctx):
    (_, _, filename) = ctx.attr.url.rpartition("/")
    downloaded_archive = ctx.actions.declare_file(filename)

    release_dir = ctx.actions.declare_directory(ctx.label.name + "_release")
    build_dir = ctx.actions.declare_directory(ctx.label.name + "_build")
    build_log = ctx.actions.declare_file(ctx.label.name + "_build.log")
    symlinks_log = ctx.actions.declare_file(ctx.label.name + "_symlinks.log")

    version_file = ctx.actions.declare_file(ctx.label.name + "_version")

    extra_configure_opts = " ".join(ctx.attr.extra_configure_opts)
    post_configure_cmds = "\n".join(ctx.attr.post_configure_cmds)
    extra_make_opts = " ".join(ctx.attr.extra_make_opts)

    if not ctx.attr.install_prefix.startswith("/"):
        # otp installations are not relocatable, so the install_prefix
        # must be absolute to build a predictable location
        fail("install_prefix must be absolute")
    install_path = path_join(ctx.attr.install_prefix, ctx.label.name)
    install_root = _install_root(ctx.attr.install_prefix)

    # At one point this rule recevied the erlang sources as a
    # label_list attribute, which had been fetched with a repository
    # rule. This had the unfortunate side effect of stripping out
    # empty directories which are expected to be present by the
    # otp makefiles. Instead this rule fetches the sources directly,
    # which also avoids unnecessarily fetching sources when they are
    # unused (such as when "external" erlang is used).
    ctx.actions.run_shell(
        inputs = [],
        outputs = [downloaded_archive],
        command = """set -euo pipefail

curl -L "{archive_url}" -o {archive_path}

if [ -n "{sha256}" ]; then
    echo "{sha256} {archive_path}" | sha256sum --check --strict -
fi
""".format(
            archive_url = ctx.attr.url,
            archive_path = downloaded_archive.path,
            sha256 = ctx.attr.sha256,
        ),
        mnemonic = "CURL",
        progress_message = "Downloading {}".format(ctx.attr.url),
    )

    # zipper = ctx.executable._zipper

    strip_prefix = ctx.attr.strip_prefix
    if strip_prefix != "":
        strip_prefix += "\\/"

    ctx.actions.run_shell(
        inputs = [downloaded_archive],
        outputs = [
            release_dir,
            build_dir,
            build_log,
            symlinks_log,
        ],
        # tools = [zipper],
        command = """set -euo pipefail

ABS_BUILD_DIR=$PWD/{build_path}
ABS_RELEASE_DIR=$PWD/{release_path}
ABS_LOG=$PWD/{build_log}
ABS_SYMLINKS=$PWD/{symlinks_log}

# {zipper} x {archive_path} -d $ABS_BUILD_DIR
tar --extract \\
    --transform 's/{strip_prefix}//' \\
    --file {archive_path} \\
    --directory $ABS_BUILD_DIR

echo "Building OTP $(cat $ABS_BUILD_DIR/OTP_VERSION) in $ABS_BUILD_DIR"

cd $ABS_BUILD_DIR
./configure --prefix={install_path} {extra_configure_opts} >> $ABS_LOG 2>&1
{post_configure_cmds}
make {extra_make_opts} >> $ABS_LOG 2>&1
make DESTDIR=$ABS_RELEASE_DIR install >> $ABS_LOG 2>&1

mv $ABS_RELEASE_DIR{install_path}/* $ABS_RELEASE_DIR
rm -d $ABS_RELEASE_DIR{install_path}
rm -rf $ABS_RELEASE_DIR{install_root}

# bazel will not allow a symlink in the output directory with
# --remote_download_minimal, so we remove them
find ${{ABS_RELEASE_DIR}} -type l | xargs ls -ld > $ABS_SYMLINKS
find ${{ABS_RELEASE_DIR}} -type l -delete

# find ${{ABS_BUILD_DIR}} -type l
find ${{ABS_BUILD_DIR}} -type l -delete
""".format(
            archive_path = downloaded_archive.path,
            strip_prefix = strip_prefix,
            zipper = "",  # zipper.path,
            build_path = build_dir.path,
            release_path = release_dir.path,
            install_path = install_path,
            install_root = install_root,
            build_log = build_log.path,
            symlinks_log = symlinks_log.path,
            extra_configure_opts = extra_configure_opts,
            post_configure_cmds = post_configure_cmds,
            extra_make_opts = extra_make_opts,
        ),
        mnemonic = "OTP",
        progress_message = "Compiling otp from source",
    )

    ctx.actions.run_shell(
        inputs = [release_dir],
        outputs = [version_file],
        command = """set -euo pipefail

mkdir -p $(dirname "{erlang_home}")
ln -sf $PWD/{erlang_release_dir} "{erlang_home}"
mkdir -p "{erlang_home}"/bin
ln -sf ../lib/erlang/bin/erl "{erlang_home}"/bin/erl

V=$("{erlang_home}"/bin/{query_erlang_version})

echo "$V" >> {version_file}
""".format(
            query_erlang_version = QUERY_ERL_VERSION,
            erlang_home = install_path,
            erlang_release_dir = release_dir.path,
            version_file = version_file.path,
        ),
    )

    return [
        DefaultInfo(
            files = depset([
                release_dir,
                version_file,
            ]),
        ),
        OtpInfo(
            release_dir = release_dir,
            erlang_home = install_path,
            version_file = version_file,
        ),
    ]

erlang_build = rule(
    implementation = _erlang_build_impl,
    attrs = {
        # "_zipper": attr.label(
        #     default = Label("@bazel_tools//tools/zip:zipper"),
        #     executable = True,
        #     cfg = "exec",
        # ),
        "url": attr.string(mandatory = True),
        "strip_prefix": attr.string(),
        "sha256": attr.string(),
        "install_prefix": attr.string(default = INSTALL_PREFIX),
        "extra_configure_opts": attr.string_list(),
        "post_configure_cmds": attr.string_list(),  # <- hopefully don't need this
        "extra_make_opts": attr.string_list(
            default = ["-j 8"],
        ),
    },
)

def _erlang_external_impl(ctx):
    erlang_home = ctx.attr._erlang_home[BuildSettingInfo].value
    erlang_version = ctx.attr._erlang_version[BuildSettingInfo].value

    version_file = ctx.actions.declare_file(ctx.label.name + "_version")

    ctx.actions.run_shell(
        inputs = [],
        outputs = [version_file],
        command = """set -euo pipefail

{begins_with_fun}
V=$("{erlang_home}"/bin/{query_erlang_version})
if ! beginswith "{erlang_version}" "$V"; then
echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
exit 1
fi

echo "$V" >> {version_file}
""".format(
            begins_with_fun = BEGINS_WITH_FUN,
            query_erlang_version = QUERY_ERL_VERSION,
            erlang_version = erlang_version,
            erlang_home = erlang_home,
            version_file = version_file.path,
        ),
        mnemonic = "OTP",
        progress_message = "Validating otp at {}".format(erlang_home),
    )

    return [
        DefaultInfo(
            files = depset([version_file]),
        ),
        OtpInfo(
            release_dir = None,
            erlang_home = erlang_home,
            version_file = version_file,
        ),
    ]

erlang_external = rule(
    implementation = _erlang_external_impl,
    attrs = {
        "_erlang_home": attr.label(default = Label("//:erlang_home")),
        "_erlang_version": attr.label(default = Label("//:erlang_version")),
    },
)

def _erlang_headers_impl(ctx):
    commands = ["set -euo pipefail", ""]

    otpinfo = ctx.attr.otp[OtpInfo]

    if otpinfo.release_dir != None:
        otp_root = otpinfo.release_dir.path
        inputs = [otpinfo.release_dir]
    else:
        otp_root = otpinfo.erlang_home
        inputs = []
    inputs.append(otpinfo.version_file)

    outs = []
    for f in ctx.attr.filenames:
        dest = ctx.actions.declare_file(path_join(ctx.label.name, f))
        commands.append("cp {otp}/lib/erlang/usr/include/{f} {dest}".format(
            otp = otp_root,
            f = f,
            dest = dest.path,
        ))
        outs.append(dest)

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = outs,
        command = "\n".join(commands),
    )

    return [DefaultInfo(files = depset(outs))]

erlang_headers = rule(
    implementation = _erlang_headers_impl,
    attrs = {
        "otp": attr.label(
            mandatory = True,
            providers = [OtpInfo],
        ),
        "filenames": attr.string_list(
            default = [
                "driver_int.h",
                "ei.h",
                "ei_connect.h",
                "eicode.h",
                "erl_driver.h",
                "erl_drv_nif.h",
                "erl_fixed_size_int_types.h",
                "erl_int_sizes_config.h",
                "erl_memory_trace_parser.h",
                "erl_nif.h",
                "erl_nif_api_funcs.h",
            ],
        ),
    },
)
