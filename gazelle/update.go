package erlang

import (
	"fmt"
	"log"
	"path/filepath"
	"sort"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

func (*erlangLang) UpdateRepos(args language.UpdateReposArgs) language.UpdateReposResult {
	fmt.Println("UpdateRepos:", args)
	return language.UpdateReposResult{}
}

func (*erlangLang) CanImport(path string) bool {
	fmt.Println("CanImport:", path)
	return filepath.Base(path) == "metadata.config"
}

func (*erlangLang) ImportRepos(args language.ImportReposArgs) language.ImportReposResult {
	fmt.Println("ImportRepos:", args)

	dir, f := filepath.Split(args.Path)
	parser := newHexMetadataParser(dir, "")
	hexMetadata, err := parser.parseHexMetadata(f)
	if err != nil {
		log.Fatalf("ERROR: %v\n", err)
	}

	gen := []*rule.Rule{}

	for _, req := range hexMetadata.Requirements {
		if req.Repository == "hexpm" {
			r := rule.NewRule(hexPmErlangAppKind, req.Name)
			r.SetAttr("version", minVersion(req.Requirement))
			r.SetAttr("erlc_opts", []string{
				"+deterministic",
				"+debug_info",
			})
			gen = append(gen, r)
		}
	}

	sort.SliceStable(gen, func(i, j int) bool {
		return gen[i].Name() < gen[j].Name()
	})

	return language.ImportReposResult{Gen: gen}
}

func minVersion(requirement string) string {
	return strings.TrimPrefix(requirement, "~> ")
}
