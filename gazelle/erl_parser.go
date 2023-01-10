package erlang

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
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

type erlParser struct{}

func newErlParser() *erlParser {
	return &erlParser{}
}

type parseCmd struct {
	Path string `json:"path"`
	Test bool   `json:"test"`
}

func (p *erlParser) parseErl(erlFilePath string, test bool) (*erlAttrs, error) {
	erlParserMutex.Lock()
	defer erlParserMutex.Unlock()

	command := parseCmd{
		Path: erlFilePath,
		Test: test,
	}

	encoder := json.NewEncoder(erlParserStdin)
	if err := encoder.Encode(&command); err != nil {
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
	IncludeLib []string            `json:"include_lib"`
	Include    []string            `json:"include"`
	Behaviour  []string            `json:"behaviour"`
	Call       map[string][]string `json:"call"`
}

func (p *erlParser) parseHrl(hrlFile string, erlangApp *erlangApp, test bool, erlAttrs *erlAttrs) error {
	hrlFilePath := filepath.Join(erlangApp.RepoRoot, erlangApp.Rel, hrlFile)
	if _, err := os.Stat(hrlFilePath); errors.Is(err, os.ErrNotExist) {
		return nil
	}

	hrlAttrs, err := p.parseErl(hrlFilePath, test)
	if err != nil {
		return err
	}
	for _, include := range hrlAttrs.Include {
		erlAttrs.Include = append(erlAttrs.Include, include)
		path := erlangApp.pathFor(hrlFile, include)
		if path != "" {
			err := p.parseHrl(path, erlangApp, test, erlAttrs)
			if err != nil {
				return err
			}
		}
	}
	erlAttrs.IncludeLib = append(erlAttrs.IncludeLib, hrlAttrs.IncludeLib...)
	return nil
}

func (p *erlParser) deepParseErl(erlFile string, erlangApp *erlangApp, test bool) (*erlAttrs, error) {
	erlFilePath := filepath.Join(erlangApp.RepoRoot, erlangApp.Rel, erlFile)
	if _, err := os.Stat(erlFilePath); errors.Is(err, os.ErrNotExist) {
		return &erlAttrs{}, nil
	}

	rootAttrs, err := p.parseErl(erlFilePath, test)
	if err != nil {
		return nil, err
	}

	for _, include := range rootAttrs.Include {
		path := erlangApp.pathFor(erlFile, include)
		if path != "" {
			err := p.parseHrl(path, erlangApp, test, rootAttrs)
			if err != nil {
				return nil, err
			}
		}
	}

	return rootAttrs, nil
}
