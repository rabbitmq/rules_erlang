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
        "release_dir": """Directory containing the erlang installation.
This is a TreeArtifact (declared directory) that can be used
directly as erlang_home. May be None for external erlang.""",
        "erlang_home": """Path to the erlang installation.
For internal/prebuilt erlang, this is the path to release_dir.
For external erlang, this is the absolute path to the system installation.""",
        "version_file": """A file containing the version of this
erlang, used to correctly invalidate the cache when an
external erlang is used""",
    },
)

def _erlang_build_impl(ctx):
    (_, _, filename) = ctx.attr.url.rpartition("/")
    downloaded_archive = ctx.actions.declare_file(filename)

    build_dir_tar = ctx.actions.declare_file(ctx.label.name + "_build.tar")
    build_log = ctx.actions.declare_file(ctx.label.name + "_build.log")
    # Use a directory artifact instead of a tar - Erlang is relocatable
    release_dir = ctx.actions.declare_directory(ctx.label.name + "_release")

    version_file = ctx.actions.declare_file(ctx.label.name + "_version")

    extra_configure_opts = " ".join(ctx.attr.extra_configure_opts)
    pre_configure_cmds = "\n".join(ctx.attr.pre_configure_cmds)
    post_configure_cmds = "\n".join(ctx.attr.post_configure_cmds)
    extra_make_opts = " ".join(ctx.attr.extra_make_opts)

    # At one point this rule received the erlang sources as a
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
""".format(
            archive_url = ctx.attr.url,
            archive_path = downloaded_archive.path,
        ),
        mnemonic = "OTP",
        progress_message = "Downloading {}".format(ctx.attr.url),
    )

    sha256file = sha256(ctx, downloaded_archive)

    # zipper = ctx.executable._zipper

    strip_prefix = ctx.attr.strip_prefix
    if strip_prefix != "":
        strip_prefix += "\\/"

    # Build and install directly to the output directory.
    # Since Erlang is relocatable (OTP 23+), we use a temporary prefix during
    # configure and then copy the result to our output directory.
    ctx.actions.run_shell(
        inputs = [downloaded_archive, sha256file],
        outputs = [
            build_dir_tar,
            build_log,
            release_dir,
            version_file,
        ],
        command = """set -euo pipefail

if [ -n "{sha256}" ]; then
    if [ "{sha256}" != "$(cat "{sha256file}")" ]; then
        echo "ERROR: Checksum mismatch. $(basename "{archive_path}") $(cat "{sha256file}") != {sha256}"
        exit 1
    fi
fi

ABS_BUILD_DIR_TAR=$PWD/{build_path}
ABS_RELEASE_DIR=$PWD/{release_dir}
ABS_LOG=$PWD/{build_log}
ABS_VERSION_FILE=$PWD/{version_file}

ABS_BUILD_DIR="$(mktemp -d)"
ABS_DEST_DIR="$(mktemp -d)"
# Use a simple prefix - Erlang is relocatable so the actual path doesn't matter
INSTALL_PREFIX="/erlang"

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
./configure --prefix="$INSTALL_PREFIX" {extra_configure_opts} >> "$ABS_LOG" 2>&1
{post_configure_cmds}
echo "    configure finished"
${{MAKE:=make}} {extra_make_opts} >> "$ABS_LOG" 2>&1
echo "    make finished"
${{MAKE}} install DESTDIR="$ABS_DEST_DIR" >> "$ABS_LOG" 2>&1
echo "    make install finished"

# Copy the installed files to the output directory
# The structure is $ABS_DEST_DIR/$INSTALL_PREFIX/lib/erlang/...
# We want the erlang_home to be the release_dir itself
cp -r "$ABS_DEST_DIR$INSTALL_PREFIX/lib/erlang/"* "$ABS_RELEASE_DIR/"

{begins_with_fun}
V=$("$ABS_RELEASE_DIR"/bin/{query_erlang_version})
echo "$V" >> "$ABS_VERSION_FILE"
""".format(
            sha256 = ctx.attr.sha256v,
            sha256file = sha256file.path,
            archive_path = downloaded_archive.path,
            strip_prefix = strip_prefix,
            build_path = build_dir_tar.path,
            release_dir = release_dir.path,
            build_log = build_log.path,
            version_file = version_file.path,
            extra_configure_opts = extra_configure_opts,
            pre_configure_cmds = pre_configure_cmds,
            post_configure_cmds = post_configure_cmds,
            extra_make_opts = extra_make_opts,
            begins_with_fun = BEGINS_WITH_FUN,
            query_erlang_version = QUERY_ERL_VERSION,
        ),
        use_default_shell_env = True,
        mnemonic = "OTP",
        progress_message = "Compiling otp from source",
    )

    return [
        DefaultInfo(
            files = depset([
                release_dir,
                version_file,
            ]),
        ),
        OtpInfo(
            version = ctx.attr.version,
            release_dir = release_dir,
            erlang_home = release_dir.path,
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
        "erlang_home": attr.string(),
        "erlang_version": attr.string(),
    },
)

def _erlang_prebuilt_impl(ctx):
    (_, _, filename) = ctx.attr.url.rpartition("/")
    downloaded_archive = ctx.actions.declare_file(filename)
    # Use a directory artifact instead of a tar - Erlang is relocatable
    release_dir = ctx.actions.declare_directory(ctx.label.name + "_release")
    version_file = ctx.actions.declare_file(ctx.label.name + "_version")

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

    # Extract directly to the output directory
    ctx.actions.run_shell(
        inputs = [downloaded_archive, sha256file],
        outputs = [release_dir, version_file],
        command = """set -euo pipefail

if [ -n "{sha256}" ]; then
    if [ "{sha256}" != "$(cat "{sha256file}")" ]; then
        echo "ERROR: Checksum mismatch. $(basename "{archive_path}") $(cat "{sha256file}") != {sha256}"
        exit 1
    fi
fi

tar --extract \\
    --file "{archive_path}" \\
    --directory "{release_dir}"

{begins_with_fun}
V=$("{release_dir}"/bin/{query_erlang_version})

echo "$V" >> {version_file}
""".format(
            sha256 = ctx.attr.sha256v,
            sha256file = sha256file.path,
            archive_path = downloaded_archive.path,
            release_dir = release_dir.path,
            begins_with_fun = BEGINS_WITH_FUN,
            query_erlang_version = QUERY_ERL_VERSION,
            version_file = version_file.path,
        ),
        mnemonic = "OTP",
        progress_message = "Extracting prebuilt otp",
    )

    return [
        DefaultInfo(
            files = depset([release_dir, version_file]),
        ),
        OtpInfo(
            version = ctx.attr.version,
            release_dir = release_dir,
            erlang_home = release_dir.path,
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
