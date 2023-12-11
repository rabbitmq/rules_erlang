-type macro() :: [atom() | {atom(), term()}].
-type include() :: string().

-type src_analysis() :: #{include_lib := [string()],
                          include := [string()],
                          behaviour => [module()],
                          parse_transform => [module()],
                          call => #{module() := [atom()]}}.

-type analysis_result() :: {ok, src_analysis()} | {error, term()}.
