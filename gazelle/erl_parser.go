package erlang

type ErlAttrs struct {
	IncludeLib []string            `json:"include_lib"`
	Include    []string            `json:"include"`
	Behaviour  []string            `json:"behaviour"`
	Call       map[string][]string `json:"call"`
}

type ErlParser interface {
	DeepParseErl(string, *ErlangApp, bool) (*ErlAttrs, error)
}
