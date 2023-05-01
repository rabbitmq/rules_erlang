def _impl(settings, attr):
    _ignore = (settings, attr)
    return {"//command_line_option:cpu": "beam"}

beam_transition = transition(
    implementation = _impl,
    inputs = [],
    outputs = ["//command_line_option:cpu"],
)
