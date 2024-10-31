type LSPHoverImplementation = {
  info: Info,
  content: String
}

type LSPImplementations = {
  -- varEnv: Map Name (use Ast in Type),

  hover: [LSPHoverImplementation]
}

let lsp: LSPImplementations = {
  hover=[]
}