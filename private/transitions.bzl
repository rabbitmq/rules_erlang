def _beam_transition(settings, attr):
    _ignore = (settings, attr)
    return {
        "//command_line_option:platforms": ["@erlang_config//:beam"],
    }

beam_transition = transition(
    implementation = _beam_transition,
    inputs = [
        # "//command_line_option:host_platform",
        "//command_line_option:platforms",
        # "//command_line_option:extra_execution_platforms",
        # "@rules_erlang//:erlang_version",
    ],
    outputs = ["//command_line_option:platforms"],
)
