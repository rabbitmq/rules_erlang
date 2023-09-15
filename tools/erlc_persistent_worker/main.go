package main

import (
	"bufio"
	"bytes"
	"context"
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"

	wp "github.com/rabbitmq/rules_erlang/tools/erlc_persistent_worker/worker_protocol"
	"google.golang.org/protobuf/encoding/protodelim"
)

func main() {
	p := flag.Bool("persistent_worker", false, "use as a bazel persistent worker")
	flag.Parse()

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

	if *p {
		reader := bufio.NewReader(os.Stdin)
		writer := bufio.NewWriter(os.Stdout)
		for true {
			request := &wp.WorkRequest{}
			err := protodelim.UnmarshalFrom(reader, request)
			if err != nil {
				log.Fatalf("ERROR: %v\n", err)
			}

			// need to shell out to erlc, using it as a server with the right env
			ctx := context.Background()
			cmd := exec.CommandContext(ctx, erlc)
			cmd.Env = append(cmd.Env,
				// "ERLC_USE_SERVER=true",
				request.GetArguments()[0],
			)
			cmd.Args = append(cmd.Args, request.GetArguments()[1:]...)
			output, err := cmd.CombinedOutput()

			response := &wp.WorkResponse{}
			response.Output = bytes.NewBuffer(output).String()
			response.ExitCode = int32(cmd.ProcessState.ExitCode())

			logger.Println("REQUEST: ", cmd.Args)
			// cwd, _ := os.Getwd()
			// logger.Println(cwd)
			logger.Println("RESPONSE:", response)

			protodelim.MarshalTo(writer, response)
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
