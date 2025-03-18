include "mexpr/mexpr.mc"
include "probtime-lib/src/ast.mc"
include "../../lsp-server/lsp/root.mc"
include "util.mc"

lang RtpplLanguageServerBase =
  RtpplAst + LSPRoot + MExprAst
  + MExprPrettyPrint + ProbTimeLanguageServerPrettyPrint
  type TypeMap = Map Name Type
end

lang RtpplLanguageServerCompiler = RtpplLanguageServerBase
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
          location = Some info,
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

  sem recursiveTopParamsLookup: TypeMap -> RtpplTopParams -> [LanguageServerPayload]
  sem recursiveTopParamsLookup types =| params ->
    createAccumulators [
      topParamsLookup types,
      createAccumulator sfold_RtpplTopParams_RtpplTopParams (topParamsLookup types)
    ] params

  sem stmtDocumentation: TypeMap -> RtpplStmt -> String
  sem stmtDocumentation types =
  | _ -> ""
  | ForLoopRtpplStmt { id = { v = name, i = info } } ->
    join [
      probtimeDefinition types "for" name,
      getSym name
    ]
  | BindingRtpplStmt { id = { v = name, i = info } } ->
    join [
      probtimeDefinition types "var" name,
      getSym name
    ]
  | ReadRtpplStmt { port = { v = portStr }, dst = { v = name, i = info } } ->
    eprintln (join ["Read ", portStr, " to ", nameGetStr name]);
    join [
      probtimeDefinition types (join ["read ", portStr, " to"]) name,
      getSym name
    ]

  sem stmtSymbol: RtpplStmt -> SymbolKind
  sem stmtSymbol =
  | _ -> SymbolFile ()
  | ForLoopRtpplStmt _ -> SymbolVariable ()
  | BindingRtpplStmt _ -> SymbolVariable ()
  | ReadRtpplStmt _ -> SymbolEvent ()

  sem stmtLookup: TypeMap -> RtpplStmt -> [LanguageServerPayload]
  sem stmtLookup types =
  | _ -> []
  | stmt & ForLoopRtpplStmt { id = { v = name, i = info } }
  | stmt & BindingRtpplStmt { id = { v = name, i = info } }
  | stmt & ReadRtpplStmt { dst = { v = name, i = info } } ->
    let documentation = lam. Some (stmtDocumentation types stmt) in
    [
      LsDefinition {
        documentation=documentation,
        kind = stmtSymbol stmt,
        location = Some info,
        name = name,
        exported = false
      },
      LsHover {
        location = info,
        toString = documentation
      }
    ]

  sem recursiveStmtLookup: TypeMap -> RtpplStmt -> [LanguageServerPayload]
  sem recursiveStmtLookup types =| stmt ->
    createAccumulators [
      stmtLookup types,
      createAccumulator sfold_RtpplStmt_RtpplStmt (recursiveStmtLookup types)
    ] stmt

  sem splitFunctionType: Type -> [Type]
  sem splitFunctionType =
  | ty -> [ty]
  | TyArrow { from=from, to=to } -> flatMap splitFunctionType [from, to]

  sem topDocumentation: TypeMap -> RtpplTop -> String
  sem topDocumentation types =
  | _ -> "Documentation unavailable"
  | FunctionDefRtpplTop {
    id = { v = name, i = info },
    params = ParamsRtpplTopParams { params = params }
  } ->
    match mapLookup name types with Some ty then
      let types = splitFunctionType ty in
      let params = zipWith (lam ty. lam param.
        match param with { id = { v = paramName } } in
        join [
          nameGetStr paramName,
          ": ",
          type2str ty
        ]
      ) types params in
      let retType = type2str (last types) in
      join [
        probtimeCode (join [
          "def ", nameGetStr name, "(", strJoin ", " params, "): ", retType
        ]),
        getSym name
      ]
    else
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
  | _ -> SymbolFile ()
  | FunctionDefRtpplTop _ -> SymbolFunction ()
  | ModelDefRtpplTop _ -> SymbolModule ()
  | TemplateDefRtpplTop _ -> SymbolStruct ()

  sem topLookup: TypeMap -> RtpplTop -> [LanguageServerPayload]
  sem topLookup types =
  | _ -> []
  | top & FunctionDefRtpplTop { id = { v = name, i = info } }
  | top & ModelDefRtpplTop { id = { v = name, i = info } }
  | top & TemplateDefRtpplTop { id = { v = name, i = info } } ->
    let documentation = lam. Some (topDocumentation types top) in
    [
      LsDefinition {
        documentation=documentation,
        kind = topDefinitionSymbol top,
        location = Some info,
        name = name,
        exported = true
      },
      LsHover {
        location = info,
        toString = documentation
      }
    ]

  sem recursiveTopLookup: TypeMap -> RtpplTop -> [LanguageServerPayload]
  sem recursiveTopLookup types =| top ->
    createAccumulators [
      topLookup types,
      createAccumulator sfold_RtpplTop_RtpplTop       (recursiveTopLookup types),
      createAccumulator sfold_RtpplTop_RtpplStmt      (recursiveStmtLookup types),
      createAccumulator sfold_RtpplTop_RtpplTopParams (recursiveTopParamsLookup types)
    ] top

  sem probtimeProgramToLanguageSupport: TypeMap -> RtpplProgram -> [LanguageServerPayload]
  sem probtimeProgramToLanguageSupport types =
  | ProgramRtpplProgram { tops = tops } ->
    flatMap (recursiveTopLookup types) tops
end

lang MExprLanguageServerLinkerCompiler = RtpplLanguageServerBase
  sem getTypeNames : TypeMap -> Type -> [Name]
  sem getTypeNames types =
  | _ -> []
  | TyCon { ident=ident, info=info } ->
    [ident]

  sem getTypeNamesRecursively : TypeMap -> Type -> [Name]
  sem getTypeNamesRecursively types =| typ ->
    createAccumulators [
      getTypeNames types,
      createAccumulator sfold_Type_Type (getTypeNamesRecursively types)
    ] typ

  sem typeLookup: TypeMap -> Type -> [LanguageServerPayload]
  sem typeLookup types =
  | _ -> []

  sem exprLookup: TypeMap -> Expr -> [LanguageServerPayload]
  sem exprLookup types =
  | _ -> []
  | TmVar { ident=ident, ty=ty, info=info } ->
    if not (nameHasSym ident) then [] else [
      LsHover {
        location = info,
        toString = lam. Some (join [
          probtimeDefinition types "var" ident,
          getSym ident
        ])
      },
      LsUsage {
        location = info,
        name = ident
      }
    ]

  sem patLookup: TypeMap -> Pat -> [LanguageServerPayload]
  sem patLookup types =
  | _ -> []

  sem recursiveTypeLookup: TypeMap -> Type -> [LanguageServerPayload]
  sem recursiveTypeLookup types =| ty ->
    createAccumulators [
      typeLookup types,
      createAccumulator sfold_Type_Type (recursiveTypeLookup types)
    ] ty

  sem recursivePatLookup: TypeMap -> Pat -> [LanguageServerPayload]
  sem recursivePatLookup types =| pat ->
    createAccumulators [
      patLookup types,
      createAccumulator sfold_Pat_Pat   (recursivePatLookup types),
      createAccumulator sfold_Pat_Type  (recursiveTypeLookup types)
    ] pat

  sem recursiveExprLookup: TypeMap -> Expr -> [LanguageServerPayload]
  sem recursiveExprLookup types =| expr ->
    createAccumulators [
      exprLookup types,
      createAccumulator sfold_Expr_Expr (recursiveExprLookup types),
      createAccumulator sfold_Expr_Type (recursiveTypeLookup types),
      createAccumulator sfold_Expr_Pat  (recursivePatLookup types)
    ] expr

  sem exprToLanguageSupport: TypeMap -> Expr -> [LanguageServerPayload]
  sem exprToLanguageSupport types =| expr ->
    recursiveExprLookup types expr
end

let exprToLanguageSupportLinker = use MExprLanguageServerLinkerCompiler in exprToLanguageSupport
let probtimeProgramToLanguageSupport = use RtpplLanguageServerCompiler in probtimeProgramToLanguageSupport