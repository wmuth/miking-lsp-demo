include "./symbol-kind.mc"
include "./completion-kind.mc"

lang LSPSymbolToCompletion = LSPCompletionKind + LSPSymbolKind
  sem symbolToCompletion: SymbolKind -> CompletionItemKind
  sem symbolToCompletion =
  | SymbolFile () -> CompletionFile ()
  | SymbolModule () -> CompletionModule ()
  | SymbolNamespace () -> CompletionFolder ()
  | SymbolPackage () -> CompletionFolder ()
  | SymbolClass () -> CompletionClass ()
  | SymbolMethod () -> CompletionMethod ()
  | SymbolProperty () -> CompletionProperty ()
  | SymbolField () -> CompletionField ()
  | SymbolConstructor () -> CompletionConstructor ()
  | SymbolEnum () -> CompletionEnum ()
  | SymbolInterface () -> CompletionInterface ()
  | SymbolFunction () -> CompletionFunction ()
  | SymbolVariable () -> CompletionVariable ()
  | SymbolConstant () -> CompletionConstant ()
  | SymbolString () -> CompletionValue ()
  | SymbolNumber () -> CompletionValue ()
  | SymbolBoolean () -> CompletionValue ()
  | SymbolArray () -> CompletionValue ()
  | SymbolObject () -> CompletionValue ()
  | SymbolKey () -> CompletionValue ()
  | SymbolNull () -> CompletionValue ()
  | SymbolEnumMember () -> CompletionEnumMember ()
  | SymbolStruct () -> CompletionStruct ()
  | SymbolEvent () -> CompletionEvent ()
  | SymbolOperator () -> CompletionOperator ()
  | SymbolTypeParameter () -> CompletionTypeParameter ()
end 