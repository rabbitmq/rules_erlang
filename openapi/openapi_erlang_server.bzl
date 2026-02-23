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
    generator_jar = ctx.attr.generator_jar
    java_path = ctx.attr.java_path

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

# Patch 1: Merge handlers for shared paths (same as v1 patch 3)
# For APIs without default_handler, consolidate operations by path
DEFAULT_HANDLER="$OUT_DIR/src/${{PKG_NAME}}_default_handler.erl"

if [ ! -f "$DEFAULT_HANDLER" ]; then
    echo "No default_handler found - consolidating operations by path"

    # Extract path -> handler mappings from router
    grep 'path =>' "$ROUTER_FILE" | sed 's/.*path => "//; s/".*//' > "$OUT_DIR/paths.tmp"
    grep "handler => '" "$ROUTER_FILE" | sed "s/.*handler => '//; s/'$//" > "$OUT_DIR/handlers.tmp"
    paste -d'|' "$OUT_DIR/paths.tmp" "$OUT_DIR/handlers.tmp" > "$OUT_DIR/path_handler_map.txt"
    rm -f "$OUT_DIR/paths.tmp" "$OUT_DIR/handlers.tmp"

    # Build mapping of paths to canonical handlers
    rm -f "$OUT_DIR/canonical_map.txt" "$OUT_DIR/redirect_list.txt"
    touch "$OUT_DIR/canonical_map.txt" "$OUT_DIR/redirect_list.txt"

    # Find paths with multiple handlers
    cut -d'|' -f1 "$OUT_DIR/path_handler_map.txt" | sort | uniq -c | while read count path; do
        if [ "$count" -gt 1 ]; then
            echo "Shared path: $path (count: $count)"
            HANDLERS=$(grep "^$path|" "$OUT_DIR/path_handler_map.txt" | cut -d'|' -f2 | sort -u)
            CANONICAL=$(echo "$HANDLERS" | awk '{{ print length, $0 }}' | sort -n | head -1 | cut -d" " -f2-)
            echo "  Handlers: $HANDLERS"
            echo "  Canonical: $CANONICAL"
            echo "$path|$CANONICAL" >> "$OUT_DIR/canonical_map.txt"

            for h in $HANDLERS; do
                if [ "$h" != "$CANONICAL" ]; then
                    echo "$h|$CANONICAL" >> "$OUT_DIR/redirect_list.txt"
                fi
            done

            # Extract clauses from OTHER handlers into CANONICAL
            TARGET_HANDLER_NAME="$CANONICAL"
            TARGET_FILE="$OUT_DIR/src/$TARGET_HANDLER_NAME.erl"
            if [ -f "$TARGET_FILE" ]; then
                rm -f "$OUT_DIR/allowed_methods_extra.tmp" "$OUT_DIR/is_authorized_extra.tmp" "$OUT_DIR/valid_content_headers_extra.tmp" "$OUT_DIR/content_types_accepted_extra.tmp"
                touch "$OUT_DIR/allowed_methods_extra.tmp" "$OUT_DIR/is_authorized_extra.tmp" "$OUT_DIR/valid_content_headers_extra.tmp" "$OUT_DIR/content_types_accepted_extra.tmp"

                for SOURCE_HANDLER_NAME in $HANDLERS; do
                    if [ "$SOURCE_HANDLER_NAME" = "$TARGET_HANDLER_NAME" ]; then
                        continue
                    fi
                    SOURCE_FILE="$OUT_DIR/src/$SOURCE_HANDLER_NAME.erl"
                    if [ ! -f "$SOURCE_FILE" ]; then
                        continue
                    fi
                    echo "    Extracting from $SOURCE_HANDLER_NAME for $TARGET_HANDLER_NAME"

                    # Extract allowed_methods clauses
                    awk '
                        /^allowed_methods[(]/ {{ in_block=1; block=$0; next }}
                        in_block {{
                            block = block "\\n" $0
                            if (/State[}}];[[:space:]]*$/) {{
                                if (block ~ /operation_id = .[A-Z]/) print block
                                in_block=0; block=""
                            }}
                            if (/State[}}][.][[:space:]]*$/) {{
                                in_block=0; block=""
                            }}
                        }}
                    ' "$SOURCE_FILE" >> "$OUT_DIR/allowed_methods_extra.tmp"

                    # Extract is_authorized clauses
                    awk '
                        /^is_authorized[(]/ {{ in_block=1; block=$0; next }}
                        in_block {{
                            block = block "\\n" $0
                            if (/^[[:space:]]*end;[[:space:]]*$/) {{
                                if (block ~ /operation_id = .[A-Z]/) print block
                                in_block=0; block=""
                            }}
                            if (/^[[:space:]]*end[.][[:space:]]*$/ || /[}}][.][[:space:]]*$/) {{
                                in_block=0; block=""
                            }}
                        }}
                    ' "$SOURCE_FILE" >> "$OUT_DIR/is_authorized_extra.tmp"

                    # Extract valid_content_headers clauses
                    awk '
                        /^valid_content_headers[(]/ {{ in_block=1; block=$0; next }}
                        in_block {{
                            block = block "\\n" $0
                            if (/State[}}];[[:space:]]*$/) {{
                                if (block ~ /operation_id = .[A-Z]/) print block
                                in_block=0; block=""
                            }}
                            if (/State[}}][.][[:space:]]*$/) {{
                                in_block=0; block=""
                            }}
                        }}
                    ' "$SOURCE_FILE" >> "$OUT_DIR/valid_content_headers_extra.tmp"

                    # Extract content_types_accepted clauses
                    awk '
                        /^content_types_accepted[(]/ {{ in_block=1; block=$0; next }}
                        in_block {{
                            block = block "\\n" $0
                            if (/State[}}];[[:space:]]*$/) {{
                                if (block ~ /operation_id = .[A-Z]/) print block
                                in_block=0; block=""
                            }}
                            if (/State[}}][.][[:space:]]*$/) {{
                                in_block=0; block=""
                            }}
                        }}
                    ' "$SOURCE_FILE" >> "$OUT_DIR/content_types_accepted_extra.tmp"
                done

                # Insert extracted clauses into canonical handler
                for fn in allowed_methods is_authorized valid_content_headers content_types_accepted; do
                    TMPFILE="$OUT_DIR/$fn""_extra.tmp"
                    if [ -f "$TMPFILE" ] && [ -s "$TMPFILE" ]; then
                        CATCH_ALL_LINE=$(grep -n "^$fn(Req, State) ->$" "$TARGET_FILE" | head -1 | cut -d: -f1)
                        if [ -n "$CATCH_ALL_LINE" ]; then
                            head -n $((CATCH_ALL_LINE - 1)) "$TARGET_FILE" > "$TARGET_FILE.new"
                            echo "" >> "$TARGET_FILE.new"
                            cat "$TMPFILE" >> "$TARGET_FILE.new"
                            echo "" >> "$TARGET_FILE.new"
                            tail -n +$CATCH_ALL_LINE "$TARGET_FILE" >> "$TARGET_FILE.new"
                            mv "$TARGET_FILE.new" "$TARGET_FILE"
                            echo "      Inserted $fn clauses into canonical handler"
                        fi
                    fi
                done

                rm -f "$OUT_DIR/allowed_methods_extra.tmp" "$OUT_DIR/is_authorized_extra.tmp" "$OUT_DIR/valid_content_headers_extra.tmp" "$OUT_DIR/content_types_accepted_extra.tmp"
            fi
        fi
    done

    # Apply router redirects
    if [ -s "$OUT_DIR/redirect_list.txt" ]; then
        echo "Applying router redirects..."
        while IFS='|' read -r from_handler to_handler; do
            echo "  Redirecting $from_handler -> $to_handler"
            sed -i "s/handler => '$from_handler'/handler => '$to_handler'/g" "$ROUTER_FILE"
        done < "$OUT_DIR/redirect_list.txt"
    fi

    rm -f "$OUT_DIR/path_handler_map.txt" "$OUT_DIR/canonical_map.txt" "$OUT_DIR/redirect_list.txt"
else
    # Redirect all handlers to default_handler
    sed -i "s/handler => '.*_handler'/handler => '${{PKG_NAME}}_default_handler'/g" "$ROUTER_FILE"

    # Merge operation clauses from other handlers into default_handler
    for OTHER_HANDLER in "$OUT_DIR"/src/*_handler.erl; do
        case "$OTHER_HANDLER" in
            *default_handler*|*logic_handler*) continue ;;
        esac

        if [ -f "$OTHER_HANDLER" ]; then
            echo "Merging operations from $(basename "$OTHER_HANDLER")"

            awk '
                /^allowed_methods[(]/ {{ in_block=1; block=$0; next }}
                in_block {{
                    block = block "\\n" $0
                    if (/State[}}];[[:space:]]*$/) {{
                        if (block ~ /operation_id = .[A-Z]/) print block
                        in_block=0; block=""
                    }}
                    if (/State[}}][.][[:space:]]*$/) {{
                        in_block=0; block=""
                    }}
                }}
            ' "$OTHER_HANDLER" >> "$OUT_DIR/allowed_methods_clauses.tmp"

            awk '
                /^is_authorized[(]/ {{ in_block=1; block=$0; next }}
                in_block {{
                    block = block "\\n" $0
                    if (/^[[:space:]]*end;[[:space:]]*$/) {{
                        if (block ~ /operation_id = .[A-Z]/) print block
                        in_block=0; block=""
                    }}
                    if (/^[[:space:]]*end[.][[:space:]]*$/ || /[}}][.][[:space:]]*$/) {{
                        in_block=0; block=""
                    }}
                }}
            ' "$OTHER_HANDLER" >> "$OUT_DIR/is_authorized_clauses.tmp"

            awk '
                /^valid_content_headers[(]/ {{ in_block=1; block=$0; next }}
                in_block {{
                    block = block "\\n" $0
                    if (/State[}}];[[:space:]]*$/) {{
                        if (block ~ /operation_id = .[A-Z]/) print block
                        in_block=0; block=""
                    }}
                    if (/State[}}][.][[:space:]]*$/) {{
                        in_block=0; block=""
                    }}
                }}
            ' "$OTHER_HANDLER" >> "$OUT_DIR/valid_content_headers_clauses.tmp"

            awk '
                /^content_types_accepted[(]/ {{ in_block=1; block=$0; next }}
                in_block {{
                    block = block "\\n" $0
                    if (/State[}}];[[:space:]]*$/) {{
                        if (block ~ /operation_id = .[A-Z]/) print block
                        in_block=0; block=""
                    }}
                    if (/State[}}][.][[:space:]]*$/) {{
                        in_block=0; block=""
                    }}
                }}
            ' "$OTHER_HANDLER" >> "$OUT_DIR/content_types_accepted_clauses.tmp"
        fi
    done

    # Insert clauses into default_handler
    for fn in allowed_methods is_authorized valid_content_headers content_types_accepted; do
        TMPFILE="$OUT_DIR/$fn""_clauses.tmp"
        if [ -f "$TMPFILE" ] && [ -s "$TMPFILE" ]; then
            CATCH_ALL_LINE=$(grep -n "^$fn(Req, State) ->$" "$DEFAULT_HANDLER" | head -1 | cut -d: -f1)
            if [ -n "$CATCH_ALL_LINE" ]; then
                head -n $((CATCH_ALL_LINE - 1)) "$DEFAULT_HANDLER" > "$DEFAULT_HANDLER.new"
                echo "" >> "$DEFAULT_HANDLER.new"
                cat "$TMPFILE" >> "$DEFAULT_HANDLER.new"
                echo "" >> "$DEFAULT_HANDLER.new"
                tail -n +$CATCH_ALL_LINE "$DEFAULT_HANDLER" >> "$DEFAULT_HANDLER.new"
                mv "$DEFAULT_HANDLER.new" "$DEFAULT_HANDLER"
            fi
        fi
        rm -f "$TMPFILE"
    done
fi

echo "Handler merge complete"

# Patch 2: Add extra content types for 3GPP compatibility
# The new generator needs content types added for both content_types_provided and content_types_accepted
echo "Adding 3GPP content types to handlers..."
for HANDLER_FILE in "$OUT_DIR"/src/*_handler.erl; do
    if [ -f "$HANDLER_FILE" ]; then
        # Add 3gpp-hal+json and problem+json to content_types_provided
        # New generator has per-operation clauses, we need to add to the catch-all
        # Add 3gpp-hal+json to the catch-all clause for content_types_provided
        # The new generator's catch-all returns {{[], Req, State}} on one line
        awk '
/^content_types_provided[(]Req, State[)] ->$/ {{
    print "content_types_provided(Req, State) ->"
    print "    {{["
    print "        {{<<\\"application/json\\">>, handle_type_provided}},"
    print "        {{<<\\"application/3gpp-hal+json\\">>, handle_type_provided}},"
    print "        {{<<\\"application/problem+json\\">>, handle_type_provided}}"
    print "    ], Req, State}}."
    # Skip the original single-line return: {{[], Req, State}}.
    getline
    next
}}
{{ print }}
' "$HANDLER_FILE" > "$HANDLER_FILE.tmp" && mv "$HANDLER_FILE.tmp" "$HANDLER_FILE"
    fi
done

# Patch 3: Auth module - delegate to LogicHandler when no API key
# The new generator returns {{false, AuthHeader, Req}} when no API key found
# We need to call the user's logic handler authorize function instead
AUTH_FILE="$OUT_DIR/src/${{PKG_NAME}}_auth.erl"
if [ -f "$AUTH_FILE" ]; then
    echo "Patching auth module: $AUTH_FILE"
    EMPTYMAP='#{{}}'
    awk -v PKG="${{PKG_NAME}}" -v EMPTYMAP="$EMPTYMAP" '
/^authorize_api_key[(]Handler, OperationID, From, KeyParam, Req0[)] ->$/ {{
    print "authorize_api_key(Handler, OperationID, From, KeyParam, Req0) ->"
    print "    {{ApiKey, Req}} = get_api_key(From, KeyParam, Req0),"
    print "    case ApiKey of"
    print "        undefined ->"
    print "            %% No API key - call user'"'"'s authorize function"
    print "            Res = Handler(OperationID, <<>>),"
    print "            case Res of"
    print "                {{true, Context}} -> {{true, Context, Req}};"
    print "                {{false, AuthHeader}} -> {{false, AuthHeader, Req}}"
    print "            end;"
    print "        _ ->"
    print "            case Handler(OperationID, ApiKey) of"
    print "                {{true, Context}} ->"
    print "                    {{true, Context, Req}};"
    print "                {{false, AuthHeader}} ->"
    print "                    {{false, AuthHeader, Req}}"
    print "            end"
    print "    end."
    while ((getline line) > 0) {{
        if (line ~ /^[a-z_]+[(]/ || line ~ /^-spec/) {{
            print line
            break
        }}
    }}
    next
}}
{{ print }}
' "$AUTH_FILE" > "$AUTH_FILE.tmp" && mv "$AUTH_FILE.tmp" "$AUTH_FILE"
fi

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

# Patch 7: Fix empty handlers in router - replace with default handler
ROUTER_FILE="$OUT_DIR/src/${{PKG_NAME}}_router.erl"
if [ -f "$ROUTER_FILE" ]; then
    if grep -q "handler => ''" "$ROUTER_FILE"; then
        echo "Fixing empty handlers in router..."
        DEFAULT_HANDLER="${{PKG_NAME}}_default_handler"

        # Create a default handler module that delegates to the logic handler
        cat > "$OUT_DIR/src/$DEFAULT_HANDLER.erl" << 'HANDLER_EOF'
-module(DEFAULT_HANDLER_PLACEHOLDER).

-behaviour(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([handle_type_accepted/2]).
-export([handle_type_provided/2]).

-ignore_xref([init/2, allowed_methods/2, content_types_accepted/2,
              content_types_provided/2, handle_type_accepted/2, handle_type_provided/2]).

init(Req, {{Operations, LogicHandler}}) ->
    Method = cowboy_req:method(Req),
    OperationID = maps:get(Method, Operations, undefined),
    State = #{{
        operation_id => OperationID,
        logic_handler => LogicHandler,
        method => Method
    }},
    {{cowboy_rest, Req, State}}.

allowed_methods(Req, State = #{{operation_id := OperationID, method := Method}}) ->
    Methods = case OperationID of
        undefined -> [];
        _ -> [Method]
    end,
    {{Methods, Req, State}}.

content_types_accepted(Req, State) ->
    {{[
        {{<<"application/json">>, handle_type_accepted}},
        {{<<"application/3gpp-hal+json">>, handle_type_accepted}},
        {{<<"application/json-patch+json">>, handle_type_accepted}},
        {{<<"application/merge-patch+json">>, handle_type_accepted}}
    ], Req, State}}.

content_types_provided(Req, State) ->
    {{[
        {{<<"application/json">>, handle_type_provided}},
        {{<<"application/3gpp-hal+json">>, handle_type_provided}},
        {{<<"application/problem+json">>, handle_type_provided}}
    ], Req, State}}.

handle_type_accepted(Req, State = #{{logic_handler := LogicHandler, operation_id := OperationID}}) ->
    case LogicHandler:accept_callback(undefined, OperationID, Req, State) of
        {{stop, Req2, State2}} -> {{stop, Req2, State2}};
        {{Result, Req2, State2}} -> {{Result, Req2, State2}}
    end.

handle_type_provided(Req, State = #{{logic_handler := LogicHandler, operation_id := OperationID}}) ->
    case LogicHandler:provide_callback(undefined, OperationID, Req, State) of
        {{stop, Req2, State2}} -> {{stop, Req2, State2}};
        {{Result, Req2, State2}} -> {{Result, Req2, State2}}
    end.
HANDLER_EOF
        # Replace the placeholder with actual module name
        sed -i "s/DEFAULT_HANDLER_PLACEHOLDER/$DEFAULT_HANDLER/" "$OUT_DIR/src/$DEFAULT_HANDLER.erl"

        # Replace empty handlers in router with the default handler
        sed -i "s/handler => ''/handler => '$DEFAULT_HANDLER'/g" "$ROUTER_FILE"
    fi
fi

# Patch 8: Add delete_resource/2 callback for DELETE requests
# cowboy_rest requires delete_resource/2 for DELETE, not content_types_accepted
# This patch adds the callback to default_handler and any handler that handles DELETE
echo "Adding delete_resource/2 callback for DELETE requests..."
for HANDLER_FILE in "$OUT_DIR"/src/*_handler.erl; do
    if [ -f "$HANDLER_FILE" ]; then
        # Check if this handler exports handle_type_accepted (indicating it can handle modifications)
        if grep -q "handle_type_accepted" "$HANDLER_FILE" && ! grep -q "delete_resource" "$HANDLER_FILE"; then
            echo "Adding delete_resource to: $HANDLER_FILE"
            # Add export for delete_resource
            sed -i 's/-export(\\[handle_type_provided\\/2\\])./-export([handle_type_provided\\/2]).\\n-export([delete_resource\\/2])./' "$HANDLER_FILE"
            # Add delete_resource to ignore_xref
            sed -i 's/handle_type_provided\\/2\\])./handle_type_provided\\/2, delete_resource\\/2])./' "$HANDLER_FILE"
            # Add delete_resource function at the end of the file
            cat >> "$HANDLER_FILE" << 'DELETEFUNC'

delete_resource(Req, State = #{{logic_handler := LogicHandler, operation_id := OperationID}}) ->
    case LogicHandler:accept_callback(undefined, OperationID, Req, State) of
        {{stop, Req2, State2}} -> {{stop, Req2, State2}};
        {{true, Req2, State2}} -> {{true, Req2, State2}};
        {{{{true, _}}, Req2, State2}} -> {{true, Req2, State2}};
        {{false, Req2, State2}} -> {{false, Req2, State2}};
        {{Result, Req2, State2}} -> {{Result, Req2, State2}}
    end.
DELETEFUNC
        fi
    fi
done

# Patch 12: Add application/json to content_types_accepted for access_token_request handler
# The OAuth2 access token endpoint by spec uses application/x-www-form-urlencoded, but
# we need to also accept application/json so the request can reach the logic handler
# (which will return 501 Not Implemented)
for HANDLER_FILE in "$OUT_DIR"/src/*access_token_request_handler.erl; do
    if [ -f "$HANDLER_FILE" ]; then
        echo "Adding application/json to content_types_accepted in: $HANDLER_FILE"
        # Add application/json after x-www-form-urlencoded
        sed -i 's/{{<<"application\\/x-www-form-urlencoded">>, handle_type_accepted}}/{{<<"application\\/x-www-form-urlencoded">>, handle_type_accepted}},\\n      {{<<"application\\/json">>, handle_type_accepted}}/' "$HANDLER_FILE"
    fi
done

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

    inputs = depset(
        direct = [spec_file] + spec_deps + erl_libs_files,
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
            doc = "Path to local openapi-generator-cli.jar. If set, uses java -jar instead of Docker.",
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
