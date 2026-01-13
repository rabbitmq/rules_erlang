load(
    "@bazel_tools//tools/build_defs/hash:hash.bzl",
    "sha256",
    "tools",
)
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
        "version": """The version that this build contains.
May be a prefix of the exact version found in the version_file.""",
        "release_dir_tar": """Tar file containing the erlang installation.
If this value is not None, it must be extracted to install_path
before use. May be None for external erlang.""",
        "install_path": """Fixed absolute path where release_dir_tar should be extracted.
This ensures path consistency across sandboxed actions.""",
        "erlang_home": """Absolute path to the erlang installation.
For internal/prebuilt erlang, this is install_path.
For external erlang, this is the absolute path to the system installation.""",
        "version_file": """A file containing the version of this
erlang, used to correctly invalidate the cache when an
external erlang is used""",
    },
)

DEFAULT_INSTALL_PREFIX = "/tmp/bazel/erlang"

def _install_path_for_label(ctx, install_prefix = DEFAULT_INSTALL_PREFIX):
    """Generate a consistent install path for an erlang installation."""
    target_cpu = ctx.var.get("TARGET_CPU", "k8")
    compilation_mode = ctx.var.get("COMPILATION_MODE", "fastbuild")
    label_hash = str(hash(str(ctx.label)))
    return "{}/{}_{}_{}_{}".format(
        install_prefix,
        ctx.label.name,
        compilation_mode,
        target_cpu,
        label_hash,
    )

def _erlang_build_impl(ctx):
    (_, _, filename) = ctx.attr.url.rpartition("/")
    downloaded_archive = ctx.actions.declare_file(filename)

    build_dir_tar = ctx.actions.declare_file(ctx.label.name + "_build.tar")
    build_log = ctx.actions.declare_file(ctx.label.name + "_build.log")
    release_dir_tar = ctx.actions.declare_file(ctx.label.name + "_release.tar")

    version_file = ctx.actions.declare_file(ctx.label.name + "_version")

    extra_configure_opts = " ".join(ctx.attr.extra_configure_opts)
    pre_configure_cmds = "\n".join(ctx.attr.pre_configure_cmds)
    post_configure_cmds = "\n".join(ctx.attr.post_configure_cmds)
    extra_make_opts = " ".join(ctx.attr.extra_make_opts)

    install_path = _install_path_for_label(ctx)

    ctx.actions.run_shell(
        inputs = [],
        outputs = [downloaded_archive],
        command = """set -euo pipefail
curl -L "{archive_url}" -o {archive_path}
""".format(
            archive_url = ctx.attr.url,
            archive_path = downloaded_archive.path,
        ),
        mnemonic = "OTP",
        progress_message = "Downloading {}".format(ctx.attr.url),
    )

    sha256file = sha256(ctx, downloaded_archive)

    strip_prefix = ctx.attr.strip_prefix
    if strip_prefix != "":
        strip_prefix += "\\/"

    ctx.actions.run_shell(
        inputs = [downloaded_archive, sha256file],
        outputs = [
            build_dir_tar,
            build_log,
            release_dir_tar,
        ],
        command = """set -euo pipefail

if [ -n "{sha256}" ]; then
    if [ "{sha256}" != "$(cat "{sha256file}")" ]; then
        echo "ERROR: Checksum mismatch. $(basename "{archive_path}") $(cat "{sha256file}") != {sha256}"
        exit 1
    fi
fi

ABS_BUILD_DIR_TAR=$PWD/{build_path}
ABS_RELEASE_DIR_TAR=$PWD/{release_path}
ABS_LOG=$PWD/{build_log}

ABS_BUILD_DIR="$(mktemp -d)"
ABS_DEST_DIR="$(mktemp -d)"

tar --extract \\
    --transform 's/{strip_prefix}//' \\
    --file "{archive_path}" \\
    --directory "$ABS_BUILD_DIR"

echo "Building OTP $(cat $ABS_BUILD_DIR/OTP_VERSION) in $ABS_BUILD_DIR"

trap 'catch $?' EXIT
catch() {{
    [[ $1 == 0 ]] || tail -n 50 "$ABS_LOG"
    echo "    archiving build dir to: {build_path}"
    cd "$ABS_BUILD_DIR"
    tar --create \\
        --file "$ABS_BUILD_DIR_TAR" \\
        *
    echo "    build log: {build_log}"
}}

cd "$ABS_BUILD_DIR"
{pre_configure_cmds}
./configure --prefix={install_path} {extra_configure_opts} >> "$ABS_LOG" 2>&1
{post_configure_cmds}
echo "    configure finished"
${{MAKE:=make}} {extra_make_opts} >> "$ABS_LOG" 2>&1
echo "    make finished"
${{MAKE}} install DESTDIR="$ABS_DEST_DIR" >> "$ABS_LOG" 2>&1
echo "    make install finished"

cd "$ABS_DEST_DIR"/{install_path}
tar --create \\
    --file "$ABS_RELEASE_DIR_TAR" \\
    *
""".format(
            sha256 = ctx.attr.sha256v,
            sha256file = sha256file.path,
            archive_path = downloaded_archive.path,
            strip_prefix = strip_prefix,
            build_path = build_dir_tar.path,
            release_path = release_dir_tar.path,
            install_path = install_path,
            build_log = build_log.path,
            extra_configure_opts = extra_configure_opts,
            pre_configure_cmds = pre_configure_cmds,
            post_configure_cmds = post_configure_cmds,
            extra_make_opts = extra_make_opts,
        ),
        use_default_shell_env = True,
        mnemonic = "OTP",
        progress_message = "Compiling otp from source",
    )

    erlang_home = path_join(install_path, "lib", "erlang")

    ctx.actions.run_shell(
        inputs = [release_dir_tar],
        outputs = [version_file],
        command = """set -euo pipefail

mkdir -p "{install_path}" || true
tar --extract \\
    --directory "{install_path}" \\
    --file {erlang_release_tar}

{begins_with_fun}
V=$("{erlang_home}"/bin/{query_erlang_version})

echo "$V" >> {version_file}
""".format(
            install_path = install_path,
            begins_with_fun = BEGINS_WITH_FUN,
            query_erlang_version = QUERY_ERL_VERSION,
            erlang_home = erlang_home,
            erlang_release_tar = release_dir_tar.path,
            version_file = version_file.path,
        ),
        mnemonic = "OTP",
        progress_message = "Validating otp at {}".format(erlang_home),
    )

    return [
        DefaultInfo(
            files = depset([
                release_dir_tar,
                version_file,
            ]),
        ),
        OtpInfo(
            version = ctx.attr.version,
            release_dir_tar = release_dir_tar,
            install_path = install_path,
            erlang_home = erlang_home,
            version_file = version_file,
        ),
    ]

erlang_build = rule(
    implementation = _erlang_build_impl,
    attrs = {
        "version": attr.string(mandatory = True),
        "url": attr.string(mandatory = True),
        "strip_prefix": attr.string(),
        "sha256v": attr.string(),
        "pre_configure_cmds": attr.string_list(),
        "extra_configure_opts": attr.string_list(),
        "post_configure_cmds": attr.string_list(),
        "extra_make_opts": attr.string_list(),
        "sha256": tools["sha256"],
    },
)

def _erlang_external_impl(ctx):
    erlang_home = ctx.attr.erlang_home
    if erlang_home == "":
        erlang_home = ctx.attr._erlang_home[BuildSettingInfo].value

    erlang_version = ctx.attr.erlang_version
    if erlang_version == "":
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
            version = erlang_version,
            release_dir_tar = None,
            install_path = None,
            erlang_home = erlang_home,
            version_file = version_file,
        ),
    ]

erlang_external = rule(
    implementation = _erlang_external_impl,
    attrs = {
        "_erlang_home": attr.label(default = Label("//:erlang_home")),
        "_erlang_version": attr.label(default = Label("//:erlang_version")),
        "erlang_home": attr.string(),
        "erlang_version": attr.string(),
    },
)

def _erlang_prebuilt_impl(ctx):
    (_, _, filename) = ctx.attr.url.rpartition("/")
    downloaded_archive = ctx.actions.declare_file(filename)
    release_dir_tar = ctx.actions.declare_file(ctx.label.name + "_release.tar")
    version_file = ctx.actions.declare_file(ctx.label.name + "_version")

    install_path = _install_path_for_label(ctx)

    ctx.actions.run_shell(
        inputs = [],
        outputs = [downloaded_archive],
        command = """set -euo pipefail
curl -L "{archive_url}" -o {archive_path}
""".format(
            archive_url = ctx.attr.url,
            archive_path = downloaded_archive.path,
        ),
        mnemonic = "OTP",
        progress_message = "Downloading {}".format(ctx.attr.url),
    )

    sha256file = sha256(ctx, downloaded_archive)

    # Extract to temp dir and create tar, then validate version
    ctx.actions.run_shell(
        inputs = [downloaded_archive, sha256file],
        outputs = [release_dir_tar, version_file],
        command = """set -euo pipefail

if [ -n "{sha256}" ]; then
    if [ "{sha256}" != "$(cat "{sha256file}")" ]; then
        echo "ERROR: Checksum mismatch. $(basename "{archive_path}") $(cat "{sha256file}") != {sha256}"
        exit 1
    fi
fi

ABS_RELEASE_TAR=$PWD/{release_tar}
ABS_VERSION_FILE=$PWD/{version_file}
TMP_DIR="$(mktemp -d)"

tar --extract \\
    --file "{archive_path}" \\
    --directory "$TMP_DIR"

# Repackage as tar (the prebuilt archive is already in the right format)
cd "$TMP_DIR"
tar --create \\
    --file "$ABS_RELEASE_TAR" \\
    *

# Extract to install_path for version check
mkdir -p "{install_path}"
tar --extract \\
    --directory "{install_path}" \\
    --file "$ABS_RELEASE_TAR"

{begins_with_fun}
V=$("{install_path}"/bin/{query_erlang_version})

echo "$V" >> "$ABS_VERSION_FILE"

rm -rf "$TMP_DIR"
""".format(
            sha256 = ctx.attr.sha256v,
            sha256file = sha256file.path,
            archive_path = downloaded_archive.path,
            release_tar = release_dir_tar.path,
            install_path = install_path,
            version_file = version_file.path,
            begins_with_fun = BEGINS_WITH_FUN,
            query_erlang_version = QUERY_ERL_VERSION,
        ),
        mnemonic = "OTP",
        progress_message = "Extracting prebuilt otp",
    )

    return [
        DefaultInfo(
            files = depset([release_dir_tar, version_file]),
        ),
        OtpInfo(
            version = ctx.attr.version,
            release_dir_tar = release_dir_tar,
            install_path = install_path,
            erlang_home = install_path,
            version_file = version_file,
        ),
    ]

erlang_prebuilt = rule(
    implementation = _erlang_prebuilt_impl,
    attrs = {
        "version": attr.string(mandatory = True),
        "url": attr.string(mandatory = True),
        "sha256v": attr.string(),
        "sha256": tools["sha256"],
    },
)
