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
}
