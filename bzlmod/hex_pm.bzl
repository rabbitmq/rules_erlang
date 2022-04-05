def hex_package_info(ctx, name):
    curl = ctx.which("curl")
    endpoint = "https://hex.pm/api/packages/{}".format(name)
    response = ctx.execute(
        [curl, "--silent", endpoint],
        timeout = 30,
    )
    if response.return_code != 0:
        fail("hex.pm api returned {} for {}".format(response.return_code, endpoint))
    return json.decode(response.stdout)

def hex_release_info(ctx, name, version):
    curl = ctx.which("curl")
    endpoint = "https://hex.pm/api/packages/{}/releases/{}".format(name, version)
    response = ctx.execute(
        [curl, "--silent", endpoint],
        timeout = 30,
    )
    if response.return_code != 0:
        fail("hex.pm api returned {} for {}".format(response.return_code, endpoint))
    return json.decode(response.stdout)

Version = provider(fields = ["major", "minor", "patch"])

def _version(version):
    [major, minor, patch] = version.split(".", 2)
    if not patch.isdigit():
        return None
    return Version(
        major = int(major),
        minor = int(minor),
        patch = int(patch),
    )

def _gte(a, b):
    if a.major > b.major:
        return True
    elif a.major == b.major:
        if a.minor > b.minor:
            return True
        elif a.minor == b.minor:
            return a.patch >= b.patch
        else:
            return False
    else:
        return False

def _lt(a, b):
    if a.major < b.major:
        return True
    elif a.major == b.major:
        if a.minor < b.minor:
            return True
        elif a.minor == b.minor:
            return a.patch < b.patch
        else:
            return False
    else:
        return False

def _eq(a, b):
    return a.major == b.major and a.minor == b.minor and a.patch == b.patch

def newest(a, b):
    if a.version == b.version:
        return a
    if _lt(_version(a.version), _version(b.version)):
        return b
    else:
        return a

def satisfies(version, requirement):
    v = _version(version)
    if v == None:
        # print("Ignoring version", version)
        return False
    if requirement.startswith("~>"):
        min = _version(requirement.removeprefix("~>").lstrip())
        if min == None:
            print("Ignoring requirement", requirement)
            return False
        max = Version(major = min.major, minor = min.minor + 1, patch = 0)
        return _gte(v, min) and _lt(v, max)
    elif _version(requirement) != None:
        return _eq(v, _version(requirement))
    else:
        fail(requirement)
