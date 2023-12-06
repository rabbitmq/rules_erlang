-type target() :: #{path := string(),
                    erlc_opts_file := string(),
                    app_src := string() | null,
                    srcs := [string()],
                    analysis := [string()],
                    analysis_id := string(),
                    outs := [string()]}.
