load(
    "//private:extract_many.bzl",
    _extract_many_transitive = "extract_many_transitive",
)

def extract_many_transitive(**kwargs):
    _extract_many_transitive(
        extract_many_tool = Label("@rules_erlang//tools/extract_many_tool:wrapper"),
        **kwargs
    )
