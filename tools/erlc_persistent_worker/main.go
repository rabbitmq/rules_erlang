package main

import (
	"bufio"
	"bytes"
	"context"
	"flag"
	"fmt"
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
	flag.Parse()

	worker_state := NewWorkerState()
	// fmt.Fprintln(os.Stderr, "PATH:", os.Getenv("PATH"))

	erlc, err := exec.LookPath("erlc")
	if err != nil {
		log.Fatalf("ERROR: %v\n", err)
		os.Exit(1)
	}

	id_hits := make(map[string]int)

	f, err := os.OpenFile("/tmp/erlc_persistent_worker.log",
		os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		log.Println(err)
	}
	defer f.Close()

	logger := log.New(f, "ERLC_PW ", log.LstdFlags)

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

			cwd, _ := os.Getwd()
			logger.Println("-------------------------------------------------------------")
			logger.Println("ID:      ", erlc_server_id)
			logger.Println("ID HITS: ", id_hits[erlc_server_id])
			logger.Println("CWD:     ", cwd)
			logger.Println("ENV:     ", cmd.Env)
			logger.Println("ARGS:    ", cmd.Args)
			logger.Println("RESPONSE:", response)

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
	// the question is what is a reasonable function of the args
	// that will actually let us get some re-use?
	// Maybe the working directory? In rabbitmq-server that might
	// work pretty well...

	package_dir := strings.TrimPrefix(request.GetArguments()[0], "--PACKAGE_DIR=")

	return strings.ReplaceAll(package_dir, "/", "_")
}
