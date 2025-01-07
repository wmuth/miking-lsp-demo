#!/bin/bash

DIR=$(dirname "$0")
VERSION=$1

# If no version is provided error out
if [ -z "$VERSION" ]; then
  echo "Please provide a version number"
  exit 1
fi

BINARY="lsp-server-$VERSION"

# If the binary doesn't exists, compile it
if [ ! -f "$DIR/out/$BINARY" ]; then
  >&2 echo "MCore LSP binary not found, compiling..."
  rm -rf $DIR/out && \
  mkdir -p $DIR/out && \
  MCORE_LIBS="stdlib=$DIR/stdlib"  \
  mi compile $DIR/mcore/lsps/mcore/lsp-server.mc --output $DIR/out/$BINARY
else
  >&2 echo "MCore LSP binary found (v$VERSION), skipping compilation"
fi

$DIR/out/lsp-server-$VERSION