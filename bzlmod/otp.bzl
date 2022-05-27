OTP_BUILD_FILE_CONTENT = """load(
    "@rules_erlang//tools:erlang.bzl",
    "standard_erlang_tools",
)

standard_erlang_tools(
    extra_configure_opts = {extra_configure_opts},
    post_configure_cmds = {post_configure_cmds},
)
"""

def merge_archive(an_archive, archives):
    for archive in archives:
        if archive["url"] == an_archive["url"]:
            if archive == an_archive:
                return archives
            else:
                fail("Conflicting definitions for otp src: {}, {}".format(an_archive, archive))
    archives.append(an_archive)
    return archives
