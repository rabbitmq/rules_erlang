def metadata_to_deps(metadata, other_deps):
    """
    Converts a metadata into a list of dependencies.
    """
    deps = []
    deps.extend(other_deps)
    reqs = metadata.get("requirements", [])
    reqs = reqs.values() if type(reqs) == type({}) else reqs
    for r in reqs:
        if not r["optional"] and r["app"] not in deps:
            deps.append(r["app"])
    return ["//{}".format(dep) for dep in deps]

def parse_term(content):
    """
    Parses a file restrictively compatible with Erlang's `file:consult/1` function into a struct.

    Allowed types:
    - Binary string: <<"foo">> - maps to string "foo".
    - List: [<<"foo">>, <<"bar">>] - maps to list ["foo", "bar"]. Values can be of any type.
    - Proplist: [{<<"foo">>, <<"bar">>}, {<<"baz">>, <<"qux">>}] - maps to dict {"foo": "bar", "baz": "qux"}. Keys must be binary strings, values can be of any type.
    - Map: #{<<"foo">> => <<"bar">>, <<"baz">> => <<"qux">>} - maps to dict {"foo": "bar", "baz": "qux"}. Keys must be binary strings, values can be of any type.
    - Boolean: true or false - maps to True or False.

    Example input:
    ```
    {<<"app">>,<<"cowboy">>}.
    {<<"build_tools">>,[<<"make">>,<<"rebar3">>]}.
    {<<"description">>,<<"Small, fast, modern HTTP server.">>}.
    {<<"files">>,
     [<<"ebin/cowboy.app">>,<<"erlang.mk">>,<<"LICENSE">>,<<"Makefile">>,
      <<"plugins.mk">>,<<"README.asciidoc">>,<<"rebar.config">>,
      <<"src/cowboy.erl">>,<<"src/cowboy_app.erl">>,<<"src/cowboy_bstr.erl">>,
      <<"src/cowboy_children.erl">>,<<"src/cowboy_clear.erl">>]}.
    {<<"licenses">>,[<<"ISC">>]}.
    {<<"links">>,
     [{<<"Function reference">>,
       <<"https://ninenines.eu/docs/en/cowboy/2.9/manual/">>},
      {<<"GitHub">>,<<"https://github.com/ninenines/cowboy">>},
      {<<"Sponsor">>,<<"https://github.com/sponsors/essen">>},
      {<<"User guide">>,<<"https://ninenines.eu/docs/en/cowboy/2.9/guide/">>}]}.
    {<<"name">>,<<"cowboy">>}.
    {<<"requirements">>,
     [{<<"cowlib">>,
       [{<<"app">>,<<"cowlib">>},
        {<<"optional">>,false},
        {<<"requirement">>,<<"2.11.0">>}]},
      {<<"ranch">>,
       [{<<"app">>,<<"ranch">>},
        {<<"optional">>,false},
        {<<"requirement">>,<<"1.8.0">>}]}]}.
    {<<"version">>,<<"2.9.0">>}.
    ```

    Example output:
    ```
    {
        "app": "cowboy",
        "build_tools": ["make", "rebar3"],
        "description": "Small, fast, modern HTTP server.",
        "files": [
            "ebin/cowboy.app",
            "erlang.mk",
            "LICENSE",
            "Makefile",
            "plugins.mk",
            "README.asciidoc",
            "rebar.config",
            "src/cowboy.erl",
            "src/cowboy_app.erl",
            "src/cowboy_bstr.erl",
            "src/cowboy_children.erl",
            "src/cowboy_clear.erl"
        ],
        "licenses": ["ISC"],
        "links": {
            "Function reference": "https://ninenines.eu/docs/en/cowboy/2.9/manual/",
            "GitHub": "https://github.com/ninenines/cowboy",
            "Sponsor": "https://github.com/sponsors/essen",
            "User guide": "https://ninenines.eu/docs/en/cowboy/2.9/guide/"
        },
        "name": "cowboy",
        "requirements": {
            "cowlib": {
                "app": "cowlib",
                "optional": false,
                "requirement": "2.11.0"
            },
            "ranch": {
                "app": "ranch",
                "optional": false,
                "requirement": "1.8.0"
            }
        },
        "version": "2.9.0"
    }
    ```
    """

    # Let's get rid of newlines, extra spaces and convert separate dot-terminated terms into a list
    content = "[" + "".join([l.strip().replace("}.", "},") for l in content.split("\n")]) + "]"

    stack = []
    for i in range(len(content)):
        if content == "":
            # We're done
            return stack[0][1]

        elif content.startswith(" ") or content.startswith(","):
            # Spaces, commas and arrows are considered whitespace
            content = content[1:]
        elif content.startswith("=>"):
            content = content[2:]

        elif content.startswith("<<\""):
            # Strings
            end = content.find("\">>")
            stack.append(("s", content[3:end]))
            content = content[end + 3:]

        elif content.startswith("true"):
            # Booleans
            stack.append(("b", True))
            content = content[4:]
        elif content.startswith("false"):
            stack.append(("b", False))
            content = content[5:]

        elif content.startswith("["):
            # Collection starts
            stack.append(("l", None))
            content = content[1:]
        elif content.startswith("{"):
            stack.append(("t", None))
            content = content[1:]
        elif content.startswith("#{"):
            stack.append(("m", None))
            content = content[2:]

        elif content.startswith("}"):
            # Collection ends - need to unwind
            # Closing brace can mean end of tuple or end of map
            # We need to check the stack to see what we're closing
            for i in range(len(stack) - 1, -1, -1):
                if stack[i] == ("t", None):
                    _unwind_tuple(stack)
                    break
                elif stack[i] == ("m", None):
                    _unwind_map(stack)
                    break
            content = content[1:]
        elif content.startswith("]"):
            # Closing bracket can mean end of list or end of proplist
            # We assume that if the last element was a tuple, we're dealing with a proplist
            # Yes, this might break horribly, we hope that hex.pm doesn't introduce
            # heterogeneous lists, why would they?
            if stack[-1][0] == "t":
                _unwind_proplist(stack)
            else:
                _unwind_list(stack)
            content = content[1:]
        else:
            fail("Unexpected content: {}".format(content))

    fail("Parsing has gotten leftovers: {}, stack: {}".format(content, stack))

def _unwind_tuple(stack):
    """
    Unwinds a tuple from the stack.
    We only support 2-tuples, because that's what hex.pm uses.
    """
    val = stack.pop()
    key = stack.pop()
    start_marker = stack.pop()
    if start_marker != ("t", None):
        fail("Unwinding tuple failed, expected tuple start marker but got: {}. key: {}, val: {}".format(start_marker, key, val))
    stack.append(("t", (key[1], val[1])))

def _unwind_map(stack):
    """
    Unwinds a map from the stack.
    """
    val = None
    map = {}

    # Iterate backwards through the stack, collecting elements.
    for i in range(len(stack) - 1, -1, -1):
        if len(stack) == 0:
            # We're out of elements before we've reached the start marker, crash
            fail("Unwinding map failed, collected elements: {}".format(map))
        tip = stack.pop()
        if tip == ("m", None):
            # We've reached the start marker of the map, so we're done
            stack.append(("m", map))
            return
        elif val == None:
            # The iteration order is reversed, so the value comes first, store it
            val = tip[1]
        else:
            # The key comes second, store the key-value pair in the map
            # and clear the value for the next iteration
            map[tip[1]] = val
            val = None

def _unwind_proplist(stack):
    """
    Unwinds a proplist from the stack.
    """
    val = {}

    # Iterate backwards through the stack, collecting elements.
    for i in range(len(stack) - 1, -1, -1):
        if len(stack) == 0:
            # We're out of elements before we've reached the start marker, crash
            fail("Unwinding proplist failed, collected elements: {}".format(val))
        tip = stack.pop()
        if tip == ("l", None):
            # We've reached the start marker of the proplist, so we're done
            # "m" because proplist is a sort of a map, not that anyone cares
            stack.append(("m", val))
            return
        elif tip[0] != "t":
            # We're expecting a tuple, crash if we don't get one
            fail("Unwinding proplist failed, expected tuple but got: {}".format(key))
        else:
            # We've got a tuple, store it in the proplist
            (key, value) = tip[1]
            val[key] = value

def _unwind_list(stack):
    """
    Unwinds a list from the stack.
    """
    val = []

    # Iterate backwards through the stack, collecting elements.
    for i in range(len(stack) - 1, -1, -1):
        if len(stack) == 0:
            # We're out of elements before we've reached the start marker, crash
            fail("Unwinding list failed, collected elements: {}".format(val))
        tip = stack.pop()
        if tip == ("l", None):
            # We've reached the start marker of the list, so we're done
            stack.append(("l", val))
            return
        else:
            # Store the value in the list
            val.append(tip[1])
