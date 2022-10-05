package erlang

import (
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"

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

func CopyMap[K, V comparable](m map[K]V) map[K]V {
	result := make(map[K]V)
	for k, v := range m {
		result[k] = v
	}
	return result
}

func Map[T, R any](f func(T) R, s []T) []R {
	result := make([]R, len(s))
	for i, elem := range s {
		result[i] = f(elem)
	}
	return result
}

func MapCat[T, R any](f func(T) []R, s []T) []R {
	var result []R
	for _, elem := range s {
		result = append(result, f(elem)...)
	}
	return result
}

func Log(c *config.Config, a ...interface{}) (n int, err error) {
	rootConfig := c.Exts[languageName].(ErlangConfigs)[""]
	if rootConfig.Verbose {
		return fmt.Println(a...)
	}
	return 0, nil
}

func Download(url *url.URL, filepath string) error {
	resp, err := http.Get(url.String())
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("bad status: %s", resp.Status)
	}

	out, err := os.Create(filepath)
	if err != nil {
		return err
	}
	defer out.Close()

	_, err = io.Copy(out, resp.Body)
	if err != nil {
		return err
	}

	return nil
}
