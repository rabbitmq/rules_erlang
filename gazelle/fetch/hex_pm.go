package fetch

import (
	"fmt"
	"regexp"
)

func ParseHexImportArg(imp string) (name, pkg, version string, err error) {
	r := regexp.MustCompile(`(?P<Name>[^=]*)=?hex\.pm/(?P<Pkg>[^@]+)@?(?P<Version>.*)`)
	match := r.FindStringSubmatch(imp)
	if len(match) == 4 {
		name = match[1]
		pkg = match[2]
		version = match[3]
		if name == "" {
			name = pkg
		}
		if version == "" {
			version = "latest"
		}
	} else {
		err = fmt.Errorf("not a valid import string: %s", imp)
	}
	return
}
