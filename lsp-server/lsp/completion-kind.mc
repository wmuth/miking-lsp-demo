lang LSPCompletionKind

  -- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind
  syn CompletionItemKind =
    | CompletionText
    | CompletionMethod
    | CompletionFunction
    | CompletionConstructor
    | CompletionField
    | CompletionVariable
    | CompletionClass
    | CompletionInterface
    | CompletionModule
    | CompletionProperty
    | CompletionUnit
    | CompletionValue
    | CompletionEnum
    | CompletionKeyword
    | CompletionSnippet
    | CompletionColor
    | CompletionFile
    | CompletionReference
    | CompletionFolder
    | CompletionEnumMember
    | CompletionConstant
    | CompletionStruct
    | CompletionEvent
    | CompletionOperator
    | CompletionTypeParameter

  sem getCompletionItemKind: CompletionItemKind -> Int
  sem getCompletionItemKind =
  | CompletionText () -> 1
  | CompletionMethod () -> 2
  | CompletionFunction () -> 3
  | CompletionConstructor () -> 4
  | CompletionField () -> 5
  | CompletionVariable () -> 6
  | CompletionClass () -> 7
  | CompletionInterface () -> 8
  | CompletionModule () -> 9
  | CompletionProperty () -> 10
  | CompletionUnit () -> 11
  | CompletionValue () -> 12
  | CompletionEnum () -> 13
  | CompletionKeyword () -> 14
  | CompletionSnippet () -> 15
  | CompletionColor () -> 16
  | CompletionFile () -> 17
  | CompletionReference () -> 18
  | CompletionFolder () -> 19
  | CompletionEnumMember () -> 20
  | CompletionConstant () -> 21
  | CompletionStruct () -> 22
  | CompletionEvent () -> 23
  | CompletionOperator () -> 24
  | CompletionTypeParameter () -> 25
end