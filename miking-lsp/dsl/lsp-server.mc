include "json.mc"

include "./dsl.mc"
include "./json-rpc.mc"
include "./lsp/lsp.mc"

let handleRequest = lam request.
  let request = getRPCRequest request in
  let method = request.method in
  use LSP in
  let params = getParams request request.method in
  match params with UnknownMethod {} then
    eprintln (join ["[Unknown method] ", method])
  else
    let executionContext = {
      parseFunc = parseCalc
    } in
    let response = execute executionContext params in
    match response with Some response then
      rpcprint (json2string response)
    else
      eprintln ""

recursive let readJsonRPC = lam.
  switch readLine stdin
    case None _ then {}
    case Some s then
      let json = jsonParseExn s in
      handleRequest json;
      readJsonRPC()
  end
end

mexpr
use Complete in

let emptyEnv = mapEmpty cmpString in

eprintln "Miking LSP started";
readJsonRPC ();
eprintln "Miking LSP ended"