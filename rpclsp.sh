#!/bin/bash

DIR=$(dirname "$0")

# docker run --rm -v /Users/didrik/projects/miking/:/mnt -i miking /app/ProbTime/lsp-server
# docker run --rm -v /Users/didrik/projects/miking/:/mnt -i miking /app/lsp-demo/mcore-lsp/lsp-server

$DIR/lsps/mcore/lsp-server
# $DIR/lsps/didrik/lsp-server