package erlang

import (
	"net/url"
	"path"
)

func DownloadRef(owner, repo, ref, filepath string) error {
	url := url.URL{
		Scheme: "https",
		Host:   "github.com",
		Path:   path.Join(owner, repo, "archive", ref+".tar.gz"),
	}

	return Download(&url, filepath)
}
