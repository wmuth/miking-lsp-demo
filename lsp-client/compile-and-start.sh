#!/bin/bash

DIR=$(dirname "$0")

mi compile $DIR/mcore/lsps/mcore/lsp-server.mc --output $DIR/out/lsp-server && \
$DIR/out/lsp-server