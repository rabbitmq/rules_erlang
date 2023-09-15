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

			ctx := context.Background()
			cmd := exec.CommandContext(ctx, erlc)
			cmd.Env = append(cmd.Env,
				"ERLC_USE_SERVER=true",
				"ERLC_SERVER_ID="+worker_state.server_id(request),
				request.GetArguments()[0],
			)
			cmd.Args = append(cmd.Args, request.GetArguments()[1:]...)
			output, err := cmd.CombinedOutput()

			response := wp.WorkResponse{
				Output:   bytes.NewBuffer(output).String(),
				ExitCode: int32(cmd.ProcessState.ExitCode()),
			}

			cwd, _ := os.Getwd()
			logger.Println("-------------------------------------------------------------")
			logger.Println("ID:      ", worker_state.server_id(request))
			logger.Println("CWD:     ", cwd)
			logger.Println("REQUEST: ", cmd.Args)
			logger.Println("RESPONSE:", response)

			protodelim.MarshalTo(writer, &response)
			err = writer.Flush()
			if err != nil {
				log.Fatalf("ERROR: %v\n", err)
			}
		}
	} else {
		fmt.Println("One Shot mode")
	}

	os.Exit(1)
}

func (s *WorkerState) server_id(request *wp.WorkRequest) string {
	return fmt.Sprintf("%03d", s.Id)
}
