# Copyright (C) 2025 Cisco Systems, Inc.

"""
OpenAPI Erlang Server code generation and compilation rule.

This module generates Erlang server code from OpenAPI specifications using
openapi-generator-cli via Docker with the erlang-server generator.

Requirements:
- OTP 27+ (uses json:decode/1)
- Docker

Handler interface:
- accept_callback/4 and provide_callback/4 callbacks
- api_key_callback/2 for authorization
"""

load("//:erlang_app_info.bzl", "ErlangAppInfo", "flat_deps")
load("//:util.bzl", "path_join")
load(
    "//tools:erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_install_erlang",
)
load("//private:util.bzl", "erl_libs_contents")

def _impl(ctx):
    spec_file = ctx.file.spec
    spec_deps = ctx.files.spec_deps
    package_name = ctx.attr.package_name
    docker_image = ctx.attr.docker_image

    # Determine Java path - prefer explicit path, then toolchain
    if ctx.attr.java_path != "java":
        # User provided explicit path
        java_path = ctx.attr.java_path
        java_runtime_files = []
    elif ctx.attr._jdk:
        # Use Java toolchain
        java_runtime = ctx.attr._jdk[java_common.JavaRuntimeInfo]
        java_path = "%s/bin/java" % java_runtime.java_home
        java_runtime_files = ctx.attr._jdk.files.to_list()
    else:
        java_path = "java"
        java_runtime_files = []

    # Determine generator JAR path - prefer label over string path
    if ctx.file.generator_jar_file:
        generator_jar = ctx.file.generator_jar_file.path
    else:
        generator_jar = ctx.attr.generator_jar

    # Get erlang toolchain
    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    # Build ERL_LIBS from dependencies
    erl_libs_dir = ctx.actions.declare_directory(ctx.label.name + "_erl_libs")
    erl_libs_files = []
    for dep in flat_deps(ctx.attr.deps):
        erl_libs_files.extend(dep[ErlangAppInfo].include)
        erl_libs_files.extend(dep[ErlangAppInfo].beam)

    # Declare output directory structure
    out_dir = ctx.actions.declare_directory(package_name)
    ebin_dir = ctx.actions.declare_directory(package_name + "_ebin")
    priv_dir = ctx.actions.declare_directory(package_name + "_priv")

    # Get the directory containing the spec file
    spec_dir = spec_file.dirname
    spec_filename = spec_file.basename

    # Build the combined generation and compilation script
    script = """#!/bin/bash
set -euo pipefail

SPEC_DIR="$PWD/{spec_dir}"
OUT_DIR="$PWD/{out_dir}"
ERL_LIBS_DIR="$PWD/{erl_libs_dir}"

{maybe_install_erlang}

mkdir -p "$OUT_DIR"
mkdir -p "$ERL_LIBS_DIR"

# Set up ERL_LIBS from deps
{erl_libs_setup}

export ERL_LIBS="$ERL_LIBS_DIR"

# Step 1: Generate Erlang code
{generator_command}

# Step 2: Apply patches to generated code
# If a patch_script is provided, run it with OUT_DIR and PKG_NAME as environment variables
{patch_script_command}

# Step 3: Compile the generated code
mkdir -p "$OUT_DIR/ebin"

for erl_file in "$OUT_DIR"/src/*.erl; do
    if [ -f "$erl_file" ]; then
        "{erlang_home}"/bin/erlc \\
            +debug_info \\
            -o "$OUT_DIR/ebin" \\
            -I "$OUT_DIR/include" \\
            -pa "$OUT_DIR/ebin" \\
            "$erl_file"
    fi
done

# Generate .app file from .app.src
APP_SRC="$OUT_DIR/src/{package_name}.app.src"
if [ -f "$APP_SRC" ]; then
    MODULES=$(cd "$OUT_DIR/ebin" && ls -1 *.beam 2>/dev/null | sed 's/\\.beam$//' | tr '\\n' ',' | sed 's/,$//')
    sed "s/{{modules, \\[\\]}}/{{modules, [$MODULES]}}/" "$APP_SRC" > "$OUT_DIR/ebin/{package_name}.app"
fi

# Copy ebin contents
EBIN_DIR="$PWD/{ebin_dir}"
mkdir -p "$EBIN_DIR"
cp -R "$OUT_DIR/ebin"/* "$EBIN_DIR/"

# Copy priv contents
PRIV_DIR="$PWD/{priv_dir}"
mkdir -p "$PRIV_DIR"
if [ -d "$OUT_DIR/priv" ] && [ "$(ls -A "$OUT_DIR/priv")" ]; then
    cp -R "$OUT_DIR/priv"/* "$PRIV_DIR/"
fi
"""

    # Generate the appropriate command based on whether local JAR or Docker is used
    if generator_jar:
        generator_command = """{java_path} -jar {generator_jar} generate \\
    -i "$SPEC_DIR/{spec_filename}" \\
    -g erlang-server \\
    -o "$OUT_DIR" \\
    --additional-properties=packageName={package_name}""".format(
            java_path = java_path,
            generator_jar = generator_jar,
            spec_filename = spec_filename,
            package_name = package_name,
        )
    else:
        generator_command = """docker run --rm \\
    --user "$(id -u):$(id -g)" \\
    -v "$SPEC_DIR":/specs:ro \\
    -v "$OUT_DIR":/out \\
    {docker_image} generate \\
    -i /specs/{spec_filename} \\
    -g erlang-server \\
    -o /out \\
    --additional-properties=packageName={package_name}""".format(
            docker_image = docker_image,
            spec_filename = spec_filename,
            package_name = package_name,
        )

    # Generate patch script command if a patch_script is provided
    if ctx.file.patch_script:
        patch_script_command = """
PKG_NAME="{package_name}"
echo "=== Running patch script for $PKG_NAME ==="
export OUT_DIR
export PKG_NAME
"{patch_script}"
echo "=== Patch script completed ==="
""".format(
            package_name = package_name,
            patch_script = ctx.file.patch_script.path,
        )
    else:
        patch_script_command = "# No patch script provided"

    script = script.format(
        spec_dir = spec_dir,
        spec_filename = spec_filename,
        out_dir = out_dir.path,
        ebin_dir = ebin_dir.path,
        priv_dir = priv_dir.path,
        erl_libs_dir = erl_libs_dir.path,
        package_name = package_name,
        maybe_install_erlang = maybe_install_erlang(ctx),
        erlang_home = erlang_home,
        erl_libs_setup = _erl_libs_setup(ctx.attr.deps, erl_libs_dir.path),
        generator_command = generator_command,
        patch_script_command = patch_script_command,
    )

    # Build input files list - include JAR if provided as label
    direct_inputs = [spec_file] + spec_deps + erl_libs_files
    if ctx.file.generator_jar_file:
        direct_inputs.append(ctx.file.generator_jar_file)
    if ctx.file.patch_script:
        direct_inputs.append(ctx.file.patch_script)

    inputs = depset(
        direct = direct_inputs,
        transitive = [runfiles.files],
    )

    # Tools include the Java runtime (needed for java executable)
    tools = depset(java_runtime_files)

    ctx.actions.run_shell(
        inputs = inputs,
        tools = tools,
        outputs = [out_dir, ebin_dir, priv_dir, erl_libs_dir],
        command = script,
        mnemonic = "OpenAPIErlangServer",
        progress_message = "Generating and compiling Erlang server from %s" % spec_file.short_path,
        use_default_shell_env = True,
        execution_requirements = {
            "requires-network": "1",
            "no-sandbox": "1",
        },
    )

    return [
        ErlangAppInfo(
            app_name = package_name,
            include = [],
            beam = [ebin_dir],
            test_beam = [],
            priv = [priv_dir],
            srcs = [out_dir],
            test_srcs = [],
            test_data = [],
            license_files = [],
            deps = ctx.attr.deps,
            direct_deps = ctx.attr.deps,
            extra_apps = ctx.attr.extra_apps,
        ),
        DefaultInfo(files = depset([out_dir, ebin_dir, priv_dir])),
    ]

def _erl_libs_setup(deps, erl_libs_dir):
    """Generate shell commands to set up ERL_LIBS directory structure."""
    lines = []
    for dep in deps:
        if ErlangAppInfo in dep:
            info = dep[ErlangAppInfo]
            lines.append("mkdir -p \"{erl_libs_dir}/{app}/ebin\"".format(
                erl_libs_dir = erl_libs_dir,
                app = info.app_name,
            ))
            for beam in info.beam:
                if beam.is_directory:
                    lines.append("cp -r \"{beam}/\"* \"{erl_libs_dir}/{app}/\" 2>/dev/null || true".format(
                        beam = beam.path,
                        erl_libs_dir = erl_libs_dir,
                        app = info.app_name,
                    ))
                else:
                    lines.append("cp \"{beam}\" \"{erl_libs_dir}/{app}/ebin/\" 2>/dev/null || true".format(
                        beam = beam.path,
                        erl_libs_dir = erl_libs_dir,
                        app = info.app_name,
                    ))
    return "\n".join(lines)

openapi_erlang_server = rule(
    implementation = _impl,
    attrs = {
        "spec": attr.label(
            mandatory = True,
            allow_single_file = [".yaml", ".yml", ".json"],
            doc = "The OpenAPI specification file.",
        ),
        "spec_deps": attr.label_list(
            allow_files = [".yaml", ".yml", ".json"],
            doc = "Additional OpenAPI spec files that are referenced by the main spec.",
        ),
        "package_name": attr.string(
            mandatory = True,
            doc = "The name of the generated Erlang application/package.",
        ),
        "docker_image": attr.string(
            default = "openapitools/openapi-generator-cli:latest",
            doc = "Docker image to use for openapi-generator-cli (default: latest).",
        ),
        "generator_jar": attr.string(
            default = "",
            doc = "Path to local openapi-generator-cli.jar. If set, uses java -jar instead of Docker. Prefer generator_jar_file for downloaded JARs.",
        ),
        "generator_jar_file": attr.label(
            allow_single_file = [".jar"],
            doc = "Label for openapi-generator-cli.jar (e.g., from http_file). Takes precedence over generator_jar string.",
        ),
        "patch_script": attr.label(
            allow_single_file = [".sh"],
            doc = "Shell script to apply patches to generated code. Script receives OUT_DIR and PKG_NAME as environment variables.",
        ),
        "java_path": attr.string(
            default = "java",
            doc = "Path to java executable. If 'java' (default), uses the Bazel Java toolchain. Set explicit path to override.",
        ),
        "_jdk": attr.label(
            default = Label("@bazel_tools//tools/jdk:current_host_java_runtime"),
            providers = [java_common.JavaRuntimeInfo],
            doc = "Java runtime toolchain for running openapi-generator-cli.jar at build time.",
        ),
        "deps": attr.label_list(
            providers = [ErlangAppInfo],
            doc = "Erlang app dependencies (e.g., cowboy).",
        ),
        "extra_apps": attr.string_list(
            default = [],
            doc = "Extra OTP applications to include (e.g., inets, ssl).",
        ),
    },
    provides = [ErlangAppInfo, DefaultInfo],
    toolchains = ["//tools:toolchain_type", "@bazel_tools//tools/jdk:runtime_toolchain_type"],
)
