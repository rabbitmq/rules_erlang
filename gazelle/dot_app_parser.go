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
	dotAppParserStdin  io.Writer
	dotAppParserStdout io.Reader
	dotAppParserMutex  sync.Mutex
)

// based on bazelbuild/rules_python/gazelle/parser.go
// https://github.com/bazelbuild/rules_python/blob/main/gazelle/parser.go

func init() {
	scriptRunfile, err := bazel.Runfile("gazelle/dot_app_to_json")
	if err != nil {
		log.Printf("failed to initialize dot_app_to_json: %v\n", err)
		os.Exit(1)
	}

	ctx := context.Background()
	ctx, parserCancel := context.WithTimeout(ctx, time.Minute*5)
	cmd := exec.CommandContext(ctx, scriptRunfile)

	cmd.Stderr = os.Stderr

	stdin, err := cmd.StdinPipe()
	if err != nil {
		log.Printf("failed to initialize dot_app_to_json: %v\n", err)
		os.Exit(1)
	}
	dotAppParserStdin = stdin

	stdout, err := cmd.StdoutPipe()
	if err != nil {
		log.Printf("failed to initialize dot_app_to_json: %v\n", err)
		os.Exit(1)
	}
	dotAppParserStdout = stdout

	if err := cmd.Start(); err != nil {
		log.Printf("failed to initialize dot_app_to_json: %v\n", err)
		os.Exit(1)
	}

	go func() {
		defer parserCancel()
		if err := cmd.Wait(); err != nil {
			log.Printf("failed to wait for dot_app_to_json: %v\n", err)
			os.Exit(1)
		}
	}()
}

type dotAppParser struct {
	// The value of language.GenerateArgs.Config.RepoRoot.
	repoRoot string
	// The value of language.GenerateArgs.Rel.
	relPackagePath string
}

func newDotAppParser(
	repoRoot string,
	relPackagePath string,
) *dotAppParser {
	return &dotAppParser{
		repoRoot:       repoRoot,
		relPackagePath: relPackagePath,
	}
}

func (p *dotAppParser) parseAppSrc(appFile string) (*dotApp, error) {
	dotAppParserMutex.Lock()
	defer dotAppParserMutex.Unlock()

	appFilePath := filepath.Join(p.repoRoot, p.relPackagePath, appFile)

	encoder := json.NewEncoder(dotAppParserStdin)
	if err := encoder.Encode(&appFilePath); err != nil {
		return nil, fmt.Errorf("failed to parse: %w", err)
	}

	reader := bufio.NewReader(dotAppParserStdout)
	data, err := reader.ReadBytes(0)
	if err != nil {
		return nil, fmt.Errorf("failed to parse: %w", err)
	}

	data = data[:len(data)-1]
	var metadata dotApp
	if err := json.Unmarshal(data, &metadata); err != nil {
		return nil, fmt.Errorf("failed to parse: %w", err)
	}

	return &metadata, nil
}

type dotApp map[string]dotAppProps

type dotAppProps struct {
	Description  string   `json:"description"`
	Vsn          string   `json:"vsn"`
	Licenses     []string `json:"licenses"`
	Applications []string `json:"applications"`
}
