
def path_join(*components):
    return "/".join(components)

def windows_path(path):
    return path.replace("/c/", "C:\\").replace("/", "\\")
