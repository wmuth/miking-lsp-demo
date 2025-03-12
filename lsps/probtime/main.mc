include "mexpr/mexpr.mc"
include "probtime-lib/src/ast.mc"
include "../../lsp-server/lsp/root.mc"
include "util.mc"

lang RtpplLanguageServerCompiler =
  RtpplAst + LSPRoot + MExprAst
  + MExprPrettyPrint + ProbTimeLanguageServerPrettyPrint

  type TypeMap = Map Name Type

  sem topParamsLookup: TypeMap -> RtpplTopParams -> [LanguageServerPayload]
  sem topParamsLookup types =
  | _ -> []
  | ParamsRtpplTopParams { params = params } ->
    let paramLookup = lam param.
      match param with { id = { v=name, i=info } } in
      let documentation = join [
        probtimeDefinition types "(parameter) var" name,
        getSym name
      ] in
      [
        LsDefinition {
          documentation=lam. Some documentation,
          kind = SymbolVariable (),
          location = info,
          name = name,
          exported = false
        },
        LsHover {
          location = info,
          toString = lam. Some documentation
        }
      ]
    in

    flatMap paramLookup params

  sem stmtLookup: TypeMap -> RtpplStmt -> [LanguageServerPayload]
  sem stmtLookup types =
  | _ -> []
  | BindingRtpplStmt { id = { v = name, i = info } } ->
    let documentation = join [
      probtimeCode (join ["var ", nameGetStr name]),
      getSym name
    ] in
    [
      LsDefinition {
        documentation=lam. Some documentation,
        kind = SymbolVariable (),
        location = info,
        name = name,
        exported = false
      },
      LsHover {
        location = info,
        toString = lam. Some documentation
      }
    ]

  sem topDocumentation: TypeMap -> RtpplTop -> String
  sem topDocumentation types =
  | FunctionDefRtpplTop { id = { v = name, i = info } } ->
    join [
      probtimeDefinition types "def" name,
      getSym name
    ]
  | ModelDefRtpplTop { id = { v = name, i = info } } ->
    join [
      probtimeDefinition types "model" name,
      getSym name
    ]
  | TemplateDefRtpplTop { id = { v = name, i = info } } ->
    join [
      probtimeDefinition types "template" name,
      getSym name
    ]

  sem topDefinitionSymbol: RtpplTop -> SymbolKind
  sem topDefinitionSymbol =
  | FunctionDefRtpplTop _ -> SymbolFunction ()
  | ModelDefRtpplTop _ -> SymbolEvent ()
  | TemplateDefRtpplTop _ -> SymbolModule ()

  sem topLookup: TypeMap -> RtpplTop -> [LanguageServerPayload]
  sem topLookup types =
  | _ -> []
  | top & FunctionDefRtpplTop { id = { v = name, i = info } }
  | top & ModelDefRtpplTop { id = { v = name, i = info } }
  | top & TemplateDefRtpplTop { id = { v = name, i = info } } ->
    let documentation = topDocumentation types top in
    [
      LsDefinition {
        documentation=lam. Some documentation,
        kind = topDefinitionSymbol top,
        location = info,
        name = name,
        exported = true
      },
      LsHover {
        location = info,
        toString = lam. Some documentation
      }
    ]

  sem recursiveTopLookup: TypeMap -> RtpplTop -> [LanguageServerPayload]
  sem recursiveTopLookup types =| top ->
    createAccumulators [
      topLookup types,
      createAccumulator sfold_RtpplTop_RtpplTop       (recursiveTopLookup types),
      createAccumulator sfold_RtpplTop_RtpplStmt      (stmtLookup types),
      createAccumulator sfold_RtpplTop_RtpplTopParams (topParamsLookup types)
    ] top

  sem probtimeProgramToLanguageSupport: TypeMap -> RtpplProgram -> [LanguageServerPayload]
  sem probtimeProgramToLanguageSupport types =
  | ProgramRtpplProgram { tops = tops } ->
    flatMap (recursiveTopLookup types) tops
end

lang MExprLanguageServerCompiler =
  MExpr + MExprAst + LSPRoot
  + MExprPrettyPrint + ProbTimeLanguageServerPrettyPrint

  sem getTypeNames : Type -> [Name]
  sem getTypeNames =
  | _ -> []
  | TyCon { ident=ident, info=info } ->
    [ident]

  sem getTypeNamesRecursively : Type -> [Name]
  sem getTypeNamesRecursively =| typ ->
    createAccumulators [
      getTypeNames,
      createAccumulator sfold_Type_Type getTypeNamesRecursively
    ] typ

  sem typeLookup: Type -> [LanguageServerPayload]
  sem typeLookup =
  | _ -> []

  sem exprLookup: Expr -> [LanguageServerPayload]
  sem exprLookup =
  | _ -> []
  | TmVar { ident=ident, ty=ty, info=info } ->
    [
      LsHover {
        location = info,
        toString = lam. Some (join [
          probtimeCode (join ["(mexpr) var ", nameGetStr ident, ": ", type2str ty]),
          getSym ident
        ])
      }
      -- LsUsage {
      --   location = info,
      --   name = ident
      -- }
    ]

  sem patLookup: Pat -> [LanguageServerPayload]
  sem patLookup =
  | _ -> []

  sem recursiveTypeLookup: Type -> [LanguageServerPayload]
  sem recursiveTypeLookup =| ty ->
    createAccumulators [
      typeLookup,
      createAccumulator sfold_Type_Type recursiveTypeLookup
    ] ty

  sem recursivePatLookup: Pat -> [LanguageServerPayload]
  sem recursivePatLookup =| pat ->
    createAccumulators [
      patLookup,
      createAccumulator sfold_Pat_Pat   recursivePatLookup,
      createAccumulator sfold_Pat_Type  recursiveTypeLookup
    ] pat

  sem recursiveExprLookup: Expr -> [LanguageServerPayload]
  sem recursiveExprLookup =| expr ->
    createAccumulators [
      exprLookup,
      createAccumulator sfold_Expr_Expr recursiveExprLookup,
      createAccumulator sfold_Expr_Type recursiveTypeLookup,
      createAccumulator sfold_Expr_Pat  recursivePatLookup
    ] expr

  sem exprToLanguageSupport: Expr -> [LanguageServerPayload]
  sem exprToLanguageSupport =| expr ->
    recursiveExprLookup expr
end

lang MExprLanguageServerLinkerCompiler =
  MExpr + MExprAst + LSPRoot
  + MExprPrettyPrint + ProbTimeLanguageServerPrettyPrint

  sem getTypeNames : Type -> [Name]
  sem getTypeNames =
  | _ -> []
  | TyCon { ident=ident, info=info } ->
    [ident]

  sem getTypeNamesRecursively : Type -> [Name]
  sem getTypeNamesRecursively =| typ ->
    createAccumulators [
      getTypeNames,
      createAccumulator sfold_Type_Type getTypeNamesRecursively
    ] typ

  sem typeLookup: Type -> [LanguageServerPayload]
  sem typeLookup =
  | _ -> []

  sem exprLookup: Expr -> [LanguageServerPayload]
  sem exprLookup =
  | _ -> []
  | TmVar { ident=ident & !("", _), ty=ty, info=info } ->
    [
      LsHover {
        location = info,
        toString = lam. Some (join [
          probtimeCode (join ["var ", nameGetStr ident, ": ", type2str ty]),
          getSym ident
        ])
      },
      LsUsage {
        location = info,
        name = ident
      }
    ]

  sem patLookup: Pat -> [LanguageServerPayload]
  sem patLookup =
  | _ -> []

  sem recursiveTypeLookup: Type -> [LanguageServerPayload]
  sem recursiveTypeLookup =| ty ->
    createAccumulators [
      typeLookup,
      createAccumulator sfold_Type_Type recursiveTypeLookup
    ] ty

  sem recursivePatLookup: Pat -> [LanguageServerPayload]
  sem recursivePatLookup =| pat ->
    createAccumulators [
      patLookup,
      createAccumulator sfold_Pat_Pat   recursivePatLookup,
      createAccumulator sfold_Pat_Type  recursiveTypeLookup
    ] pat

  sem recursiveExprLookup: Expr -> [LanguageServerPayload]
  sem recursiveExprLookup =| expr ->
    createAccumulators [
      exprLookup,
      createAccumulator sfold_Expr_Expr recursiveExprLookup,
      createAccumulator sfold_Expr_Type recursiveTypeLookup,
      createAccumulator sfold_Expr_Pat  recursivePatLookup
    ] expr

  sem exprToLanguageSupport: Expr -> [LanguageServerPayload]
  sem exprToLanguageSupport =| expr ->
    recursiveExprLookup expr
end

let exprToLanguageSupport = use MExprLanguageServerCompiler in exprToLanguageSupport
let exprToLanguageSupportLinker = use MExprLanguageServerLinkerCompiler in exprToLanguageSupport
let probtimeProgramToLanguageSupport = use RtpplLanguageServerCompiler in probtimeProgramToLanguageSupport