include "json.mc"
include "./utils.mc"

type RPCRequest = {
	method: String,
	params: Map String JsonValue,
	id: Option Int
}

type RPCError = {
	code: Int,
	message: String,
	data: JsonValue
}

type RPCResult
con RPCSuccess : JsonValue -> RPCResult
con RPCFailure : RPCError  -> RPCResult

type RPCResponse = {
	result: RPCResult,
	id: Int
}

let getRPCRequest: JsonValue -> RPCRequest = lam request.
	match request with JsonObject request in
	let id = match mapLookup "id" request with Some JsonInt id then Some id else None () in
	match mapLookup "method" request with Some JsonString method in
	match mapLookup "params" request with Some JsonObject params in
	{
		method = method,
		params = params,
		id = id
	}