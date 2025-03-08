#!/bin/bash

# >&2 echo $PATH

DIR=$(dirname "$0")
VERSION=$1

# If no version is provided error out
if [ -z "$VERSION" ]; then
  echo "Please provide a version number"
  exit 1
fi

BINARY="lsp-server-$VERSION"

if ! command -v mi 2>&1 >/dev/null
then
    >&2 echo "mi could not be found"
    exit 1
fi

# If the binary doesn't exists, compile it
if [ ! -f "$DIR/out/$BINARY" ]; then
  >&2 echo "ProbTime LSP binary not found, compiling..."
  rm -rf $DIR/out && \
  mkdir -p $DIR/out && \
  MCORE_LIBS="stdlib=$DIR/stdlib" \
  mi compile $DIR/probtime/lsps/probtime/lsp-server.mc --output $DIR/out/$BINARY && \
  $DIR/out/lsp-server-$VERSION || \
  (>&2 echo "Compilation failed" && exit 1)
else
  >&2 echo "ProbTime LSP binary found (v$VERSION), skipping compilation" && \
  $DIR/out/lsp-server-$VERSION
fi