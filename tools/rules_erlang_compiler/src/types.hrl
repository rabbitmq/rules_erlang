-type input() :: #{path := string(), digest := string()}.
-type request_args() :: #{targets_file := string()}.
-type request() :: #{arguments := request_args(),
                     inputs := [input()],
                     request_id => integer()}.

-type response() :: #{exit_code := integer(), output := string()}.

-type target() :: #{path := string(),
                    erlc_opts_file := string(),
                    app_src := string() | null,
                    srcs := [string()],
                    analysis := [string()],
                    analysis_id := string(),
                    outs := [string()]}.

-type module_index() :: #{string() := string()}.

-type config() :: #{module_index := module_index(),
                    code_paths := [string()],
                    targets := #{string() := target()}}.
