package fetch_test

import (
	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
	"github.com/rabbitmq/rules_erlang/gazelle/fetch"
	"testing"
)

func TestUpdate(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Fetch Suite")
}

var _ = Describe("ParseGithubImportArg", func() {
	It("should parse the full specification", func() {
		imp := "inet_tcp_proxy_dist=github.com/rabbitmq/inet_tcp_proxy@master"
		name, owner, repo, ref, err := fetch.ParseGithubImportArg(imp)

		Expect(name).To(Equal("inet_tcp_proxy_dist"))
		Expect(owner).To(Equal("rabbitmq"))
		Expect(repo).To(Equal("inet_tcp_proxy"))
		Expect(ref).To(Equal("master"))
		Expect(err).NotTo(HaveOccurred())
	})
})
