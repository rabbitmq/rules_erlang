def _impl(settings, attr):
    _ignore = (attr)
    if not settings["@rules_erlang//transitions:enable"]:
        return {}
    return {"//command_line_option:cpu": "beam"}

beam_transition = transition(
    implementation = _impl,
    inputs = ["@rules_erlang//transitions:enable"],
    outputs = ["//command_line_option:cpu"],
)
