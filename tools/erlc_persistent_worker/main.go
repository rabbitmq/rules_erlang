package main

import (
	"bufio"
	"bytes"
	"context"
	"flag"
	"fmt"
	"hash/fnv"
	"log"
	"math/rand"
	"os"
	"os/exec"
	"strings"

	wp "github.com/rabbitmq/rules_erlang/tools/erlc_persistent_worker/worker_protocol"
	"google.golang.org/protobuf/encoding/protodelim"
)

type WorkerState struct {
	Id int
}

func NewWorkerState() *WorkerState {
	return &WorkerState{
		Id: rand.Intn(1000),
	}
}

func main() {
	persistent := flag.Bool(
		"persistent_worker", false, "use as a bazel persistent worker",
	)
	logfile := flag.String(
		"logfile", "", "log commands and respsonses to a file",
	)
	flag.Parse()

	worker_state := NewWorkerState()

	erlc, err := exec.LookPath("erlc")
	if err != nil {
		log.Fatalf("ERROR: %v\n", err)
		os.Exit(1)
	}

	id_hits := make(map[string]int)

	var logger *log.Logger
	if *logfile != "" {
		f, err := os.OpenFile(*logfile, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
			os.Exit(1)
		}
		defer f.Close()
		logger = log.New(f, "ERLC_PW ", log.LstdFlags)
	}

	if *persistent {
		reader := bufio.NewReader(os.Stdin)
		writer := bufio.NewWriter(os.Stdout)
		for true {
			request := &wp.WorkRequest{}
			err := protodelim.UnmarshalFrom(reader, request)
			if err != nil {
				log.Fatalf("ERROR: %v\n", err)
			}

			if !strings.HasPrefix(request.GetArguments()[0], "--PACKAGE_DIR=") {
				log.Fatalf("ERROR: First argument did not begin with --PACKAGE_DIR=\n")
				os.Exit(1)
			}

			if !strings.HasPrefix(request.GetArguments()[1], "--ERL_LIBS=") {
				log.Fatalf("ERROR: First argument did not begin with --ERL_LIBS=\n")
				os.Exit(1)
			}

			erl_libs_pair := strings.TrimPrefix(request.GetArguments()[1], "--")

			erlc_server_id := worker_state.server_id(request)

			id_hits[erlc_server_id]++

			ctx := context.Background()
			cmd := exec.CommandContext(ctx, erlc)
			cmd.Env = append(cmd.Env,
				"ERLC_USE_SERVER=true",
				"ERLC_SERVER_ID="+erlc_server_id,
				erl_libs_pair,
			)
			cmd.Args = append(cmd.Args, request.GetArguments()[2:]...)
			output, err := cmd.CombinedOutput()

			response := wp.WorkResponse{
				Output:   bytes.NewBuffer(output).String(),
				ExitCode: int32(cmd.ProcessState.ExitCode()),
			}

			if logger != nil {
				cwd, _ := os.Getwd()
				logger.Println("-------------------------------------------------------------")
				logger.Println("WORKER ID:", worker_state.Id)
				logger.Println("ID:       ", erlc_server_id)
				logger.Println("ID HITS:  ", id_hits[erlc_server_id])
				logger.Println("CWD:      ", cwd)
				logger.Println("ENV:      ", cmd.Env)
				logger.Println("ARGS:     ", cmd.Args)
				logger.Println("RESPONSE: ", response)
			}

			protodelim.MarshalTo(writer, &response)
			err = writer.Flush()
			if err != nil {
				log.Fatalf("ERROR: %v\n", err)
			}
		}
	} else {
		argsfile := strings.TrimPrefix(os.Args[1], "@")
		argsfile_bytes, err := os.ReadFile(argsfile)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
			os.Exit(1)
		}
		erlcArgs := strings.Split(string(argsfile_bytes), "\n")

		if !strings.HasPrefix(erlcArgs[0], "--PACKAGE_DIR=") {
			log.Fatalf("ERROR: First argument did not begin with --PACKAGE_DIR=\n")
			os.Exit(1)
		}

		if !strings.HasPrefix(erlcArgs[1], "--ERL_LIBS=") {
			log.Fatalf("ERROR: First argument did not begin with --ERL_LIBS=\n")
			os.Exit(1)
		}

		erl_libs_pair := strings.TrimPrefix(erlcArgs[0], "--")

		ctx := context.Background()
		cmd := exec.CommandContext(ctx, erlc)
		cmd.Env = append(cmd.Env, erl_libs_pair)
		cmd.Args = append(cmd.Args, erlcArgs[2:]...)
		output, err := cmd.CombinedOutput()

		fmt.Println(erl_libs_pair, cmd)
		fmt.Print(bytes.NewBuffer(output).String())
		if err != nil {
			os.Exit(cmd.ProcessState.ExitCode())
		}
	}
	os.Exit(0)
}

func (s *WorkerState) server_id(request *wp.WorkRequest) string {
	package_dir := strings.TrimPrefix(request.GetArguments()[0], "--PACKAGE_DIR=")
	erl_libs := strings.TrimPrefix(request.GetArguments()[1], "--ERL_LIBS=")

	erlc_args := parse_erlc_args(request.GetArguments()[2:], &ErlcArgs{})

	return fmt.Sprintf("%s-%d-%d",
		strings.ReplaceAll(package_dir, "/", "_"),
		string_hash(erl_libs),
		erlc_args.hash(),
	)
}

func parse_erlc_args(arguments []string, acc *ErlcArgs) *ErlcArgs {
	if len(arguments) == 0 {
		return acc
	}

	switch arguments[0] {
	case "-v":
		acc.Verbose = true
		return parse_erlc_args(arguments[1:], acc)
	case "-I":
		acc.Includes = append(acc.Includes, arguments[1])
		return parse_erlc_args(arguments[2:], acc)
	case "-o":
		acc.Output = arguments[1]
		return parse_erlc_args(arguments[2:], acc)
	default:
		if strings.HasPrefix(arguments[0], "-D") {
			acc.ErlTerms = append(acc.ErlTerms, arguments[0])
		} else if strings.HasPrefix(arguments[0], "+") {
			acc.ErlTerms = append(acc.ErlTerms, arguments[0])
		}
		return parse_erlc_args(arguments[1:], acc)
	}
}

type ErlcArgs struct {
	Verbose  bool
	Includes []string
	ErlTerms []string
	Output   string
}

func (args *ErlcArgs) hash() uint32 {
	h := fnv.New32a()
	for i := range args.Includes {
		h.Write([]byte(args.Includes[i]))
	}
	for i := range args.ErlTerms {
		h.Write([]byte(args.ErlTerms[i]))
	}
	h.Write([]byte(args.Output))
	return h.Sum32()
}

func string_hash(s string) uint32 {
	h := fnv.New32a()
	h.Write([]byte(s))
	return h.Sum32()
}
