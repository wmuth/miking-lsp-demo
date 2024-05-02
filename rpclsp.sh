#!/bin/bash

# Reason for this intermittent file:
# The vscode lsp client cannot itself handle attaching a pipe.
# This script is a workaround to attach the pipe to the lsp server.
# Miking cannot currently handle parsing JSON-RPC due to a lack of a readStdin function which can read x amount of bytes.
# We have an intermittent server, handling extracting RPC packets to the Miking LSP server.

DIR=$(dirname "$0")
echo $DIR 1>&2;

.$DIR/rust-server/target/debug/rust-server --stdin \
	| .$DIR/miking-lsp/dsl/dsl \
	| .$DIR/rust-server/target/debug/rust-server --stdout