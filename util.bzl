BEGINS_WITH_FUN = """beginswith() { case $2 in "$1"*) true;; *) false;; esac; }"""
QUERY_ERL_VERSION = """erl -eval '{ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:fwrite(Version), halt().' -noshell"""

def path_join(*components):
    nec = []
    for c in components:
        if c != "":
            nec.append(c)
    return "/".join(nec)

def windows_path(path):
    return path.replace("/c/", "C:\\").replace("/", "\\")
