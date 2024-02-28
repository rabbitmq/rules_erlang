-type request_args() :: #{targets_file := string()}.
-type inputs() :: #{file:name() := binary()}.
-type request() :: #{arguments := request_args(),
                     inputs := inputs(),
                     request_id => integer()}.

-type response() :: #{exit_code := integer(), output := string()}.

-type target() :: #{src_path := string(),
                    erlc_opts_file := string(),
                    app_src := string() | null,
                    srcs := [string()],
                    outs := [string()]}.

-type module_index() :: #{module() := atom()}.

-type config() :: #{label := string(),
                    module_index := module_index(),
                    code_paths := [string()],
                    targets := #{atom() := target()}}.

-type warnings_list() :: [{file:name(), [term()]}].
-type errors_list() :: warnings_list().

-type file_compilation_result() :: {ok, module(), binary(), warnings_list()} |
                                   {error, errors_list(), warnings_list()}.

-type app_compilation_result() :: {ok, Modules :: [module()], Warnings :: warnings_list()} |
                                  {error, Modules :: [module()], Errors :: errors_list(), Warnings :: warnings_list()}.
