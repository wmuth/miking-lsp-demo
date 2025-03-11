include "mexpr/mexpr.mc"

include "probtime-lib/src/ast.mc"

include "../../lsp-server/lsp/root.mc"

let debugSym = true

let probtimeCode = lam code. join ["```probtime\n", code, "\n```\n"]

recursive let getAnyType = lam types.
  use MExprAst in
  switch types
    case [] then
      TyUnknown { info = NoInfo () }
    case [t] then
      t
    case [TyUnknown _] ++ rest then
      getAnyType rest
    case [t] ++ rest then
      t
  end
end

let getSym = 
  if debugSym then
    lam name.
      join [
        "\n",
        optionGetOr "No symbol" (optionMap (compose int2string sym2hash) (nameGetSym name))
      ]
    else
      lam. ""

-- Basically do this:
-- let childTypes = sfold_Decl_Type (lam acc. lam ty.
--   join [acc, recursiveTypeLookup file env ty]
-- ) [] decl
-- But with less boilerplate.
let createAccumulator: all a. all b. all acc. (([acc] -> a -> [acc]) -> [acc] -> b -> [acc]) -> (a -> [acc]) -> b -> [acc] =
  lam sfold. lam generator. lam item.
    sfold (lam acc. lam pat.
      join [acc, generator pat]
    ) [] item

lang RtpplLanguageServerCompiler = RtpplAst + LSPRoot
  sem topParamsLookup: String -> RtpplTopParams -> [LanguageServerPayload]
  sem topParamsLookup filename =
  | _ -> []
  | ParamsRtpplTopParams { params = params } ->
    let paramLookup = lam param.
      match param with { id = { v=name, i=info } } in
      let documentation = join [
        probtimeCode (join [nameGetStr name]),
        "(param. ", getSym name, ")"
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

  sem stmtLookup: String -> RtpplStmt -> [LanguageServerPayload]
  sem stmtLookup filename =
  | _ -> []
  | BindingRtpplStmt { id = { v = name, i = info } } ->
    let documentation = join [
      probtimeCode (join ["var ", nameGetStr name]),
      "(var. ", getSym name, ")"
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

  sem topLookup: String -> RtpplTop -> [LanguageServerPayload]
  sem topLookup filename =| top ->
    let stmts = sfold_RtpplTop_RtpplStmt (lam acc. lam stmt.
      join [acc, stmtLookup filename stmt]
    ) [] top in

    let topParams = sfold_RtpplTop_RtpplTopParams (lam acc. lam stmt.
      join [acc, topParamsLookup filename stmt]
    ) [] top in

    join [stmts, topParams]

  sem recursiveProgramLookup: String -> RtpplProgram -> [LanguageServerPayload]
  sem recursiveProgramLookup filename =| program & ProgramRtpplProgram { tops = tops } ->
    flatMap (topLookup filename) tops

  sem probtimeProgramToLanguageSupport: String -> RtpplProgram -> [LanguageServerPayload]
  sem probtimeProgramToLanguageSupport filename =| program ->
    recursiveProgramLookup filename program
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
    let myTypes = getTypeNames typ in
    sfold_Type_Type (lam acc. lam ty.
      join [acc, getTypeNamesRecursively ty]
    ) myTypes typ

  sem typeLookup: String -> Type -> [LanguageServerPayload]
  sem typeLookup filename =
  | _ -> []

  -- sem exprDocumentation: Expr -> String
  -- sem exprDocumentation =
  -- | TmLet { ident=ident, tyAnnot=tyAnnot, tyBody=tyBody, info=info } ->
  --   join [
  --     probtimeCode (join ["var ", nameGetStr ident, ": ", type2str (getAnyType [tyAnnot, tyBody])]),
  --     "(let. ", getSym ident, ")"
  --   ]
  -- | TmVar { ident=ident, ty=ty, info=info } ->
  --   join [
  --     probtimeCode (join ["var ", nameGetStr ident, ": ", type2str ty]),
  --     "(var. ", getSym ident, ")"
  --   ]

  sem exprLookup: String -> Expr -> [LanguageServerPayload]
  sem exprLookup filename =
  | _ -> []
  -- | expr & TmLet { ident=ident, tyAnnot=tyAnnot, tyBody=tyBody, info=info } ->
  --   let documentation = exprDocumentation expr in
  --   [
  --     LsHover {
  --       location = info,
  --       toString = lam. Some documentation
  --     }
  --   ]
  | expr & TmVar { ident=ident, ty=ty, info=info } ->
    -- let documentation = exprDocumentation expr in
    [
      LsUsage {
        location = info,
        name = ident
      }
      -- LsHover {
      --   location = info,
      --   toString = lam. Some documentation
      -- }
    ]

  sem patLookup: String -> Pat -> [LanguageServerPayload]
  sem patLookup filename =
  | _ -> []

  sem recursiveTypeLookup: String -> Type -> [LanguageServerPayload]
  sem recursiveTypeLookup filename =| ty ->
    let self = typeLookup filename ty in

    let childTypes = sfold_Type_Type (lam acc. lam ty.
      let types = recursiveTypeLookup filename ty in
      join [acc, types]
    ) [] ty in

    join [self, childTypes]

  sem recursivePatLookup: String -> Pat -> [LanguageServerPayload]
  sem recursivePatLookup filename =| pat ->
    let self = patLookup filename pat in

    let childTypes = sfold_Pat_Type (lam acc. lam ty.
      join [acc, recursiveTypeLookup filename ty]
    ) [] pat in

    let childPatterns = sfold_Pat_Pat (
      lam acc. lam pat.
        join [acc, recursivePatLookup filename pat]
    ) [] pat in

    join [self, childTypes, childPatterns]

  sem recursiveExprLookup: String -> Expr -> [LanguageServerPayload]
  sem recursiveExprLookup filename =| expr ->
    let self = exprLookup filename expr in

    let childTypes = sfold_Expr_Type (lam acc. lam ty.
      join [acc, recursiveTypeLookup filename ty]
    ) [] expr in

    let childExprs = sfold_Expr_Expr (lam acc. lam expr.
      join [acc, recursiveExprLookup filename expr]
    ) [] expr in

    let childPatterns = sfold_Expr_Pat (lam acc. lam pat.
      join [acc, recursivePatLookup filename pat]
    ) [] expr in

    join [self, childExprs, childPatterns, childTypes]

  sem exprToLanguageSupport: String -> Expr -> [LanguageServerPayload]
  sem exprToLanguageSupport filename =| expr ->
    recursiveExprLookup filename expr
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
    let myTypes = getTypeNames typ in
    sfold_Type_Type (lam acc. lam ty.
      join [acc, getTypeNamesRecursively ty]
    ) myTypes typ

  sem typeLookup: String -> Type -> [LanguageServerPayload]
  sem typeLookup filename =
  | _ -> []

  sem exprLookup: String -> Expr -> [LanguageServerPayload]
  sem exprLookup filename =
  | _ -> []
  -- | TmLet { ident=ident, parentIdent=Some (_, info), tyAnnot=tyAnnot, tyBody=tyBody } ->
  --   let documentation = join [
  --     probtimeCode (join ["var ", nameGetStr ident, ": ", type2str (getAnyType [tyAnnot, tyBody])]),
  --     "(let. ", getSym ident, ")"
  --   ] in
  --   [
  --     LsDefinition {
  --       documentation=lam. Some documentation,
  --       exported = false,
  --       kind = SymbolVariable (),
  --       location = info,
  --       name = ident
  --     }
  --   ]
  | TmVar { ident=ident & !("", _), ty=ty, info=info } ->
    [
      LsUsage {
        location = info,
        name = ident
      }
    ]

  sem patLookup: String -> Pat -> [LanguageServerPayload]
  sem patLookup filename =
  | _ -> []

  sem recursiveTypeLookup: String -> Type -> [LanguageServerPayload]
  sem recursiveTypeLookup filename =| ty ->
    let self = typeLookup filename ty in

    let childTypes = sfold_Type_Type (lam acc. lam ty.
      let types = recursiveTypeLookup filename ty in
      join [acc, types]
    ) [] ty in

    join [self, childTypes]

  sem recursivePatLookup: String -> Pat -> [LanguageServerPayload]
  sem recursivePatLookup filename =| pat ->
    let self = patLookup filename pat in

    let childTypes = sfold_Pat_Type (lam acc. lam ty.
      join [acc, recursiveTypeLookup filename ty]
    ) [] pat in

    let childPatterns = sfold_Pat_Pat (
      lam acc. lam pat.
        join [acc, recursivePatLookup filename pat]
    ) [] pat in

    join [self, childTypes, childPatterns]

  sem recursiveExprLookup: String -> Expr -> [LanguageServerPayload]
  sem recursiveExprLookup filename =| expr ->
    let self = exprLookup filename expr in

    let childTypes = sfold_Expr_Type (lam acc. lam ty.
      join [acc, recursiveTypeLookup filename ty]
    ) [] expr in

    let childExprs = sfold_Expr_Expr (lam acc. lam expr.
      join [acc, recursiveExprLookup filename expr]
    ) [] expr in

    let childPatterns = sfold_Expr_Pat (lam acc. lam pat.
      join [acc, recursivePatLookup filename pat]
    ) [] expr in

    join [self, childExprs, childPatterns, childTypes]

  sem exprToLanguageSupport: String -> Expr -> [LanguageServerPayload]
  sem exprToLanguageSupport filename =| expr ->
    recursiveExprLookup filename expr
end

let exprToLanguageSupport = use MExprLanguageServerCompiler in exprToLanguageSupport
let exprToLanguageSupportLinker = use MExprLanguageServerLinkerCompiler in exprToLanguageSupport
let probtimeProgramToLanguageSupport = use RtpplLanguageServerCompiler in probtimeProgramToLanguageSupport