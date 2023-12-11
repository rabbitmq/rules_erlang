-type request_args() :: #{targets_file := string()}.
-type request() :: #{arguments := request_args(),
                     inputs := #{file:name() := binary()},
                     request_id => integer()}.

-type response() :: #{exit_code := integer(), output := string()}.

-type target() :: #{src_path := string(),
                    erlc_opts_file := string(),
                    app_src := string() | null,
                    srcs := [string()],
                    outs := [string()]}.

-type module_index() :: #{string() := string()}.

-type config() :: #{module_index := module_index(),
                    code_paths := [string()],
                    targets := #{string() := target()}}.

-type warnings_list() :: [{file:name(), [term()]}].
-type errors_list() :: warnings_list().

-type compilation_result() :: {ok, module(), binary(), warnings_list()} |
                              {error, errors_list(), warnings_list()}.
