# rules_erlang

Bazel rules for Erlang sources.

## Maintenance Status

Team RabbitMQ were using Bazel heavily alongside `erlang.mk` for a few years for its parallel
builds and caching features.

However, `erlang.mk` has caught up and Bazel fell out of use.

Therefore, **this project is not actively maintained**. You are welcome to fork it,
or become a new maintainer.

## Examples

See [basic](examples/basic/)

## Typical rules

### Compile and run all tests

```shell
bazel test //...
```

### Run the unit suite only

```shell
bazel test //:unit_SUITE
```

### Run a single test case

```shell
bazel test //:unit_SUITE \
    --test_env FOCUS="-group my_group -case my_case"
```

## Assumptions

`erlang_app` and `ct_suite` macros require the standard otp layout, relative to the bazel package (to some degree abitrary layout can be handled with with the `erlang_bytecode`, `app_file`, `erlang_app_info` & `ct_test` rules which those macros utilize). For an erlang application named `my_erlang_app` this means:

```
my_erlang_app
├── BUILD.bazel
├── include
│   ├── ...
│   └── my_header.hrl
├── priv
│   └── schema
├── src
│   ├── ...
│   └── my_erlang_app.erl
└── test
    ├── ...
    └── unit_SUITE.erl
```

And that the convention is followed where, using the `dest` attribute of the `erlang_bytecode` rule:
1. Compiled production bytecode is placed in `ebin`
2. Compiled test bytecode is placed in `src`
3. Compiled test suite and test helper bytecode is placed in `test`

The example above follows these conventions.

## Additional examples

- https://github.com/rabbitmq/lz4-erlang uses rules_erlang with bzlmod and compiles native extensions
- https://github.com/rabbitmq/rabbitmq-server provides a complex example of usage

## Copyright and License

(c) 2020-2023 Broadcom. All Rights Reserved. The term “Broadcom” refers to Broadcom Inc. and/or its subsidiaries.  All rights reserved.

Dual licensed under the Apache License Version 2.0 and
Mozilla Public License Version 2.0.

This means that the user can consider the library to be licensed under
**any of the licenses from the list** above. For example, you may
choose the Apache Public License 2.0 and include this library into a
commercial product.

See [LICENSE](./LICENSE) for details.
