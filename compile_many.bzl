load(
    "//private:compile_many.bzl",
    _compile_many = "compile_many",
)

def compile_many(**kwargs):
    _compile_many(
        rules_erlang_compiler = Label("@rules_erlang//tools/rules_erlang_compiler:wrapper"),
        **kwargs
    )
