_PLATFORMS = "//command_line_option:platforms"

def _beam_transition(settings, attr):
    _ignore = (settings, attr)
    return {
        _PLATFORMS: settings[_PLATFORMS],
        # _PLATFORMS: ["@erlang_config//:beam"],
    }

beam_transition = transition(
    implementation = _beam_transition,
    inputs = [
        # "//command_line_option:host_platform",
        _PLATFORMS,
        # "//command_line_option:extra_execution_platforms",
        # "@rules_erlang//:erlang_version",
    ],
    outputs = [_PLATFORMS],
)
