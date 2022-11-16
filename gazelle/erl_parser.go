package erlang

import (
	"bufio"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"sync"
	"time"

	"github.com/bazelbuild/rules_go/go/tools/bazel"
)

var (
	erlParserStdin  io.Writer
	erlParserStdout io.Reader
	erlParserMutex  sync.Mutex
)

// based on bazelbuild/rules_python/gazelle/parser.go
// https://github.com/bazelbuild/rules_python/blob/main/gazelle/parser.go

func init() {
	scriptRunfile, err := bazel.Runfile("gazelle/erl_attrs_to_json")
	if err != nil {
		log.Printf("failed to initialize erl_attrs_to_json: %v\n", err)
		os.Exit(1)
	}

	ctx := context.Background()
	ctx, parserCancel := context.WithTimeout(ctx, time.Minute*5)
	cmd := exec.CommandContext(ctx, scriptRunfile)

	cmd.Stderr = os.Stderr

	stdin, err := cmd.StdinPipe()
	if err != nil {
		log.Printf("failed to initialize erl_attrs_to_json: %v\n", err)
		os.Exit(1)
	}
	erlParserStdin = stdin

	stdout, err := cmd.StdoutPipe()
	if err != nil {
		log.Printf("failed to initialize erl_attrs_to_json: %v\n", err)
		os.Exit(1)
	}
	erlParserStdout = stdout

	if err := cmd.Start(); err != nil {
		log.Printf("failed to initialize erl_attrs_to_json: %v\n", err)
		os.Exit(1)
	}

	go func() {
		defer parserCancel()
		if err := cmd.Wait(); err != nil {
			log.Printf("failed to wait for erl_attrs_to_json: %v\n", err)
			os.Exit(1)
		}
	}()
}

type erlParser struct {
	// The value of language.GenerateArgs.Config.RepoRoot.
	repoRoot string
	// The value of language.GenerateArgs.Rel.
	relPackagePath string
}

func newErlParser(
	repoRoot string,
	relPackagePath string,
) *erlParser {
	return &erlParser{
		repoRoot:       repoRoot,
		relPackagePath: relPackagePath,
	}
}

func (p *erlParser) parseErl(erlFilePath string) (*erlAttrs, error) {
	parserMutex.Lock()
	defer parserMutex.Unlock()

	encoder := json.NewEncoder(erlParserStdin)
	if err := encoder.Encode(&erlFilePath); err != nil {
		return nil, fmt.Errorf("failed to parse: %w", err)
	}

	reader := bufio.NewReader(erlParserStdout)
	data, err := reader.ReadBytes(0)
	if err != nil {
		return nil, fmt.Errorf("failed to parse: %w", err)
	}

	data = data[:len(data)-1]
	var metadata erlAttrs
	if err := json.Unmarshal(data, &metadata); err != nil {
		return nil, fmt.Errorf("failed to parse: %w", err)
	}

	return &metadata, nil
}

type erlAttrs struct {
	IncludeLib []string `json:"include_lib"`
	Include    []string `json:"include"`
	Behaviour  []string `json:"behaviour"`
}

func pathFor(erlangApp *erlangApp, include string) string {
	privatePath := filepath.Join("src", include)
	if erlangApp.PrivateHdrs.Contains(privatePath) {
		return privatePath
	}
	publicPath := filepath.Join("include", include)
	if erlangApp.PublicHdrs.Contains(publicPath) {
		return publicPath
	}
	return ""
}

func (p *erlParser) parseHrl(hrlFilePath string, erlangApp *erlangApp, erlAttrs *erlAttrs) error {
	hrlAttrs, err := p.parseErl(filepath.Join(p.repoRoot, p.relPackagePath, hrlFilePath))
	if err != nil {
		return err
	}
	for _, include := range hrlAttrs.Include {
		erlAttrs.Include = append(erlAttrs.Include, include)
		path := pathFor(erlangApp, include)
		if path != "" {
			err := p.parseHrl(path, erlangApp, erlAttrs)
			if err != nil {
				return err
			}
		}
	}
	erlAttrs.IncludeLib = append(erlAttrs.IncludeLib, hrlAttrs.IncludeLib...)
	return nil
}

func (p *erlParser) deepParseErl(erlFilePath string, erlangApp *erlangApp) (*erlAttrs, error) {
	rootAttrs, err := p.parseErl(erlFilePath)
	if err != nil {
		return nil, err
	}

	for _, include := range rootAttrs.Include {
		path := pathFor(erlangApp, include)
		if path != "" {
			err := p.parseHrl(path, erlangApp, rootAttrs)
			if err != nil {
				return nil, err
			}
		}
	}

	return rootAttrs, nil
}
