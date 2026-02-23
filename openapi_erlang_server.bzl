# Copyright (C) 2025 Cisco Systems, Inc.

"""
Re-export of openapi_erlang_server rule for convenience.

Uses the current erlang-server generator (OTP 27+, json:decode/1).
"""

load(
    "//openapi:openapi_erlang_server.bzl",
    _openapi_erlang_server = "openapi_erlang_server",
)

openapi_erlang_server = _openapi_erlang_server

