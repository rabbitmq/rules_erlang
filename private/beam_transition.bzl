def _impl(settings, attr):
    _ignore = (settings, attr)

    # print("_impl")
    # print("settings", settings)
    # print("attr", attr)
    return {
        "arm": {"//command_line_option:cpu": "arm"},
        "x86": {"//command_line_option:cpu": "x86"},
        "k8": {"//command_line_option:cpu": "k8"},
    }

beam_transition = transition(
    implementation = _impl,
    inputs = [],
    outputs = ["//command_line_option:cpu"],
)
