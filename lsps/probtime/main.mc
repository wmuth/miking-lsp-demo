include "mexpr/mexpr.mc"
include "probtime-lib/src/ast.mc"
include "../../lsp-server/lsp/root.mc"
include "util.mc"

lang RtpplLanguageServerCompiler = RtpplAst + LSPRoot
  sem topParamsLookup: RtpplTopParams -> [LanguageServerPayload]
  sem topParamsLookup =
  | _ -> []
  | ParamsRtpplTopParams { params = params } ->
    let paramLookup = lam param.
      match param with { id = { v=name, i=info } } in
      let documentation = join [
        probtimeCode (join ["(parameter) var ", nameGetStr name]),
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

  sem stmtLookup: RtpplStmt -> [LanguageServerPayload]
  sem stmtLookup =
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

  sem topDocumentation: RtpplTop -> String
  sem topDocumentation =
  | FunctionDefRtpplTop { id = { v = name, i = info } } ->
    join [
      probtimeCode (join ["def ", nameGetStr name]),
      getSym name
    ]
  | ModelDefRtpplTop { id = { v = name, i = info } } ->
    join [
      probtimeCode (join ["model ", nameGetStr name]),
      getSym name
    ]
  | TemplateDefRtpplTop { id = { v = name, i = info } } ->
    join [
      probtimeCode (join ["template ", nameGetStr name]),
      getSym name
    ]

  sem topDefinitionSymbol: RtpplTop -> SymbolKind
  sem topDefinitionSymbol =
  | FunctionDefRtpplTop _ -> SymbolFunction ()
  | ModelDefRtpplTop _ -> SymbolEvent ()
  | TemplateDefRtpplTop _ -> SymbolModule ()

  sem topLookup: RtpplTop -> [LanguageServerPayload]
  sem topLookup =
  | _ -> []
  | top & FunctionDefRtpplTop { id = { v = name, i = info } }
  | top & ModelDefRtpplTop { id = { v = name, i = info } }
  | top & TemplateDefRtpplTop { id = { v = name, i = info } } ->
    let documentation = topDocumentation top in
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

  sem recursiveTopLookup: RtpplTop -> [LanguageServerPayload]
  sem recursiveTopLookup =| top ->    
    createAccumulators [
      topLookup,
      createAccumulator sfold_RtpplTop_RtpplTop recursiveTopLookup,
      createAccumulator sfold_RtpplTop_RtpplStmt stmtLookup,
      createAccumulator sfold_RtpplTop_RtpplTopParams topParamsLookup
    ] top

  sem recursiveProgramLookup: RtpplProgram -> [LanguageServerPayload]
  sem recursiveProgramLookup =| program & ProgramRtpplProgram { tops = tops } ->
    flatMap recursiveTopLookup tops

  sem probtimeProgramToLanguageSupport: RtpplProgram -> [LanguageServerPayload]
  sem probtimeProgramToLanguageSupport =| program ->
    recursiveProgramLookup program
end

lang MExprLanguageServerCompiler =
  MExpr + MExprAst + MExprPrettyPrint + LSPRoot

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
      createAccumulator sfold_Pat_Pat recursivePatLookup,
      createAccumulator sfold_Pat_Type recursiveTypeLookup
    ] pat

  sem recursiveExprLookup: Expr -> [LanguageServerPayload]
  sem recursiveExprLookup =| expr ->
    createAccumulators [
      exprLookup,
      createAccumulator sfold_Expr_Expr recursiveExprLookup,
      createAccumulator sfold_Expr_Type recursiveTypeLookup,
      createAccumulator sfold_Expr_Pat recursivePatLookup
    ] expr

  sem exprToLanguageSupport: Expr -> [LanguageServerPayload]
  sem exprToLanguageSupport =| expr ->
    recursiveExprLookup expr
end

lang MExprLanguageServerLinkerCompiler =
  MExpr + MExprAst + MExprPrettyPrint + LSPRoot

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
      createAccumulator sfold_Pat_Pat recursivePatLookup,
      createAccumulator sfold_Pat_Type recursiveTypeLookup
    ] pat

  sem recursiveExprLookup: Expr -> [LanguageServerPayload]
  sem recursiveExprLookup =| expr ->
    createAccumulators [
      exprLookup,
      createAccumulator sfold_Expr_Expr recursiveExprLookup,
      createAccumulator sfold_Expr_Type recursiveTypeLookup,
      createAccumulator sfold_Expr_Pat recursivePatLookup
    ] expr

  sem exprToLanguageSupport: Expr -> [LanguageServerPayload]
  sem exprToLanguageSupport =| expr ->
    recursiveExprLookup expr
end

let exprToLanguageSupport = use MExprLanguageServerCompiler in exprToLanguageSupport
let exprToLanguageSupportLinker = use MExprLanguageServerLinkerCompiler in exprToLanguageSupport
let probtimeProgramToLanguageSupport = use RtpplLanguageServerCompiler in probtimeProgramToLanguageSupport