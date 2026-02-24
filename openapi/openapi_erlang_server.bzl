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
    java_path = ctx.attr.java_path

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

# Step 2: Apply patches to generated code for hjon compatibility
# These patches adapt the new generator output to work with the hjon codebase
# The new generator has different architecture - fewer patches needed

ROUTER_FILE="$OUT_DIR/src/{package_name}_router.erl"
PKG_NAME="{package_name}"

echo "=== Applying patches for $PKG_NAME ==="

# Patch 4: Add v1 route support for ts29503_Nudm_SDM
# Match ts29503_Nudm_SDM package
if echo "${{PKG_NAME}}" | grep -q "^ts29503_Nudm_SDM"; then
    echo "Adding v1 route support for ts29503_Nudm_SDM..."
    ROUTER_FILE="$OUT_DIR/src/${{PKG_NAME}}_router.erl"
    if [ -f "$ROUTER_FILE" ]; then
        # Replace base_path with version-aware path construction
        sed -i 's|base_path => "/nudm-sdm/v2"|base_path => "/nudm-sdm/" ++ Version|g' "$ROUTER_FILE"
        sed -i 's/^get_operations() ->$/get_operations(Version) ->/' "$ROUTER_FILE"

        awk '
/^group_paths[(][)] ->$/ {{
    print "group_paths() ->"
    print "    Versions = [\\"v1\\", \\"v2\\"],"
    print "    FoldFun = fun(OperationID, #{{base_path := BasePath, path := Path, method := Method, handler := Handler}}, Acc) ->"
    print "        FullPath = BasePath ++ Path,"
    print "        case maps:find(FullPath, Acc) of"
    print "            {{ok, PathInfo0 = #{{operations := Operations0}}}} ->"
    print "                Operations = Operations0#{{Method => OperationID}},"
    print "                PathInfo = PathInfo0#{{operations => Operations}},"
    print "                Acc#{{FullPath => PathInfo}};"
    print "            error ->"
    print "                Operations = #{{Method => OperationID}},"
    print "                PathInfo = #{{handler => Handler, operations => Operations}},"
    print "                Acc#{{FullPath => PathInfo}}"
    print "        end"
    print "    end,"
    print "    lists:foldl("
    print "        fun(Version, AccOuter) ->"
    print "            maps:fold(FoldFun, AccOuter, get_operations(Version))"
    print "        end,"
    print "        #{{}},"
    print "        Versions"
    print "    )."
    print ""
    while ((getline line) > 0) {{
        if (line ~ /^get_operations[(]Version[)]/) {{
            print line
            break
        }}
    }}
    next
}}
{{ print }}
' "$ROUTER_FILE" > "$ROUTER_FILE.tmp" && mv "$ROUTER_FILE.tmp" "$ROUTER_FILE"
    fi
fi

# Patch 5: Fix AccessToken API - use schema validation
if [ "${{PKG_NAME}}" = "ts29510_Nnrf_AccessToken" ]; then
    echo "Fixing AccessToken API validation..."
    API_FILE="$OUT_DIR/src/${{PKG_NAME}}_api.erl"

    awk '
/^request_params[(]'"'"'AccessTokenRequest'"'"'[)] ->$/ {{
    print "request_params('"'"'AccessTokenRequest'"'"') ->"
    print "    ['"'"'AccessTokenReq'"'"'];"
    print ""
    while ((getline line) > 0) {{
        if (line ~ /^request_params[(]/) {{
            print line
            break
        }}
    }}
    next
}}
/^request_param_info[(]OperationID, Name[)] ->$/ {{
    print "request_param_info('"'"'AccessTokenRequest'"'"', '"'"'AccessTokenReq'"'"') ->"
    print "    #{{"
    print "        source =>   body,"
    print "        rules => ["
    print "            {{schema, object, <<\\"#/components/schemas/AccessTokenReq\\">>}},"
    print "            required"
    print "        ]"
    print "    }};"
    print ""
    print $0
    next
}}
{{ print }}
' "$API_FILE" > "$API_FILE.tmp" && mv "$API_FILE.tmp" "$API_FILE"
fi

# Patch 6: Fix NFManagement API - change 'list' to 'PatchItem'
# Match ts29510_Nnrf_NFManagement package
if echo "${{PKG_NAME}}" | grep -q "^ts29510_Nnrf_NFManagement"; then
    echo "Fixing NFManagement API validation for UpdateNFInstance..."
    API_FILE="$OUT_DIR/src/${{PKG_NAME}}_api.erl"

    sed -i "s/'list'/'PatchItem'/g" "$API_FILE"

    # Add array validation support for PatchItem
    awk '
/^validate[(]Rule = [{{]schema,/ {{
    print "validate(Rule, Name, Value, ValidatorState) when is_list(Value) ->"
    print "    case lists:filter(fun(Item) -> validate(Rule, Name, Item, ValidatorState) =/= ok end, Value) of"
    print "        [] -> ok;"
    print "        _ -> validation_error(Rule, Name, Value)"
    print "    end;"
    print ""
    print $0
    next
}}
{{ print }}
' "$API_FILE" > "$API_FILE.tmp" && mv "$API_FILE.tmp" "$API_FILE"

    # Skip NFProfile schema validation - the anyOf schema is too strict
    # Replace schema validation with passing validation for NFProfile
    echo "Skipping strict NFProfile schema validation..."
    sed -i 's|{{schema, object, <<"#/components/schemas/NFProfile">>}}|not_required|g' "$API_FILE"
fi

echo "=== Patches applied successfully ==="

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
    )

    # Build input files list - include JAR if provided as label
    direct_inputs = [spec_file] + spec_deps + erl_libs_files
    if ctx.file.generator_jar_file:
        direct_inputs.append(ctx.file.generator_jar_file)

    inputs = depset(
        direct = direct_inputs,
        transitive = [runfiles.files],
    )

    ctx.actions.run_shell(
        inputs = inputs,
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
        "java_path": attr.string(
            default = "java",
            doc = "Path to java executable. Use full path if java is not in Bazel's sandbox PATH.",
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
    toolchains = ["//tools:toolchain_type"],
)
