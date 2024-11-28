#!/bin/bash

# Reason for this intermittent file:
# The vscode lsp client cannot itself handle attaching a pipe.
# This script is a workaround to attach the pipe to the lsp server.
# Miking cannot currently handle parsing JSON-RPC due to a lack of a readStdin function which can read x amount of bytes.
# We have an intermittent server, handling extracting RPC packets to the Miking LSP server.

DIR=$(dirname "$0")

# # If we are in dev mode, we continually run the new lsp-server binary,
# # so that we don't constantly need to restart the extension host.
# # In non-dev mode, we let the lsp-server binary run continuously for performance.
# # Change this flag in serverOptions in `lsp-client/client/src/extension.ts`
# if [ "$1" == "--dev" ]; then
# 	while true
# 	do
# 		# .$DIR/rpc-wrapper/target/debug/rpc-wrapper --stdin --quit-after-one-request \
# 		.$DIR/miking-lsp/dsl/lsp-server \
# 		# | .$DIR/rpc-wrapper/target/debug/rpc-wrapper --stdout --quit-after-one-request

		# $DIR/miking-lsp/dsl/lsp-server
# 	done	
# else
# 	# .$DIR/rpc-wrapper/target/debug/rpc-wrapper --stdin \
# 	.$DIR/miking-lsp/dsl/lsp-server \
# 	# | .$DIR/rpc-wrapper/target/debug/rpc-wrapper --stdout
# fi

# docker run --rm -v /Users/didrik/projects/miking/:/mnt -i miking /app/ProbTime/lsp-server

# docker run --rm -v /Users/didrik/projects/miking/:/mnt -i miking /app/lsp-demo/mcore-lsp/lsp-server

$DIR/mcore-lsp/lsp-server