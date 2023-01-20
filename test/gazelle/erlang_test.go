package erlang_test

import (
	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
	erlang "github.com/rabbitmq/rules_erlang/gazelle"
	"testing"
)

func TestUpdate(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Erlang Suite")
}

var _ = Describe("beamFilesRules", func() {
	It("should allow include_lib to work right", func() {
		app := erlang.NewErlangApp("fake_root", "fake_rel")
		Expect(app).NotTo(BeNil())
	})
})
