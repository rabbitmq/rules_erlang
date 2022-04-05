load("//private:erlc.bzl", "erlc_private")

def erlc(**kwargs):
    erlc_private(
        compile_first = Label("//compile_first:escript"),
        **kwargs
    )
