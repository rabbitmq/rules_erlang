package erlang

import (
	"fmt"

	"github.com/bazelbuild/bazel-gazelle/config"
)

func Contains[T comparable](s []T, e T) bool {
	for _, v := range s {
		if v == e {
			return true
		}
	}
	return false
}

func ContainsAll[T comparable](s []T, elements []T) bool {
	for _, element := range elements {
		if !Contains(s, element) {
			return false
		}
	}
	return true
}

func Log(c *config.Config, a ...interface{}) (n int, err error) {
	if c.Exts[languageName].(ErlangConfig).Verbose {
		return fmt.Println(a...)
	}
	return 0, nil
}
