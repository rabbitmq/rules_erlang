package erlang_test

import (
	erlang "github.com/rabbitmq/rules_erlang/gazelle"
)

type erlParserFake struct {
	Responses map[string]*erlang.ErlAttrs
}

func fakeErlParser(responses map[string]*erlang.ErlAttrs) *erlParserFake {
	return &erlParserFake{Responses: responses}
}

func (p *erlParserFake) DeepParseErl(erlFile string, erlangApp *erlang.ErlangApp, macros erlang.ErlParserMacros) (*erlang.ErlAttrs, error) {
	if r, ok := p.Responses[erlFile]; ok {
		return r, nil
	}
	return &erlang.ErlAttrs{}, nil
}
