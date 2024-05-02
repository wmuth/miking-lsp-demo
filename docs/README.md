# Miking LSP PoC

https://github.com/didrikmunther/miking-lsp-demo/assets/5240046/3fb31104-b3fc-44c9-8262-ad91f5245114

## Requirements

1. VS Code
2. Make
3. Node
4. NPM
5. Rust/Cargo

## Usage

1. `make all`
2. Press `F5` in VS Code
3. Open `test.txt`
4. Try changing float values to integer values to see diagnostics

## How it works

<img src="mikinglspdemo.png" alt="Miking LSP" height="200"/>

1. The user opens a plaintext file in VS Code.
2. The LSP client eventually sends a `textDocument/didChange` notification to the LSP client.
3. The LSP client forwards the message to the RPC Wrapper.
4. The RPC Wrapper strips the RPC header*, and forwards the message to the Miking LSP server.
5. The Miking LSP server parses the message, compiles the code, and sends a `textDocument/publishDiagnostics` notification with the results to the RPC Wrapper.
6. The RPC Wrapper wraps the JSON object with an RPC header, and forwards the message to the LSP client.
7. The LSP client forwards the message to VS Code, which displays the diagnostics.

*The RPC header consists of the line `Content-Length: <n>`, describing the length of the JSON body. We can't handle this in the Miking LSP server, so we strip it in the RPC Wrapper.
