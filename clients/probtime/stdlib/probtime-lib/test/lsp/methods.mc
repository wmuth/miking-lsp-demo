include "./methods/unknown.mc"
include "./methods/initialize.mc"

-- To be extended with more methods
lang LSP =
LSPInitialize
+ LSPUnknownMethod
end

mexpr

use LSP in

let test = getMessage {
  jsonrpc = "2.0",
  id = 1,
  method = "initialize",
  params = {
	processId = 123,
	locale = "en",
	rootPath = "file:///home/user",
	rootUri = "file:///home/user",
	initializationOptions = null,
	trace = "off"
  }
} in

()