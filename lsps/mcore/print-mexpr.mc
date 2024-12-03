include "../miking-lsp/dsl/utils.mc"
include "../../miking/src/main/eval.mc"

mexpr

let uri = "/Users/didrik/projects/miking/lsp-demo/miking-lsp/dsl/dsl.mc" in

let ast = parseParseMCoreFile {
	keepUtests = false,
	keywords = [],
	pruneExternalUtests = true,
	pruneExternalUtestsWarning = true,
	findExternalsExclude = false, -- the interpreter does not support externals
	eliminateDeadCode = false
} uri in

use MExprPrettyPrint in eprintln (expr2str ast);

()