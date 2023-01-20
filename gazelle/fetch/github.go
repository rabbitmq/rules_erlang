package fetch

import (
	"fmt"
	"regexp"
)

func ParseGithubImportArg(imp string) (name, owner, repo, ref string, err error) {
	r := regexp.MustCompile(`(?P<Name>[^=]*)=?github\.com/(?P<Owner>[^/]+)/(?P<Repo>[^@]+)@?(?P<Ref>.*)`)
	match := r.FindStringSubmatch(imp)
	if len(match) == 5 {
		name = match[1]
		owner = match[2]
		repo = match[3]
		ref = match[4]
		if name == "" {
			name = repo
		}
		if ref == "" {
			ref = "main"
		}
	} else {
		err = fmt.Errorf("not a valid import string: %s", imp)
	}
	return
}
