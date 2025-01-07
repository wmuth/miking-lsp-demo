include "file.mc"

lang MLangDefinition = MLangFileHandler + MLangPipeline
  type DefinitionSeq = [(Name, Info)]

  sem generateDefinitions : MLangProgram -> Definitions
  sem generateDefinitions =| program ->
    mapFromSeq nameCmp (join [
      flatMap (recursiveDeclDefinitions) program.decls,
      recursiveExprDefinitions program.expr
    ])

  sem exprDefinitions: Expr -> DefinitionSeq
  sem exprDefinitions =
  | _ -> []
  -- TODO: Extend
  | TmLet { ident=ident, ty=ty, info=info }
  | TmLam { ident=ident, ty=ty, info=info } ->
    [(ident, info)]

  sem declDefinitions: Decl -> DefinitionSeq
  sem declDefinitions =
  | _ -> []
  | DeclLet { ident=ident, info=info }
  | DeclSyn { ident=ident, info=info }
  | DeclType { ident=ident, info=info }
  | DeclConDef { ident=ident, info=info }
  | DeclExt { ident=ident, info=info }
  | DeclLang { ident=ident, info=info } ->
    [(ident, info)]
  | DeclSem { ident=ident, info=info, args = args } ->
    -- Simply refer to the position of the sem, since
    -- we have no way to knowing the position of the args
    join [
      [(ident, info)],
      map (lam arg. (arg.ident, info)) (optionGetOr [] args)
    ]

  sem recursiveExprDefinitions: Expr -> DefinitionSeq
  sem recursiveExprDefinitions =| expr ->
    let lookup = exprDefinitions expr in
    let children = sfold_Expr_Expr (lam acc. lam expr.
      let children = recursiveExprDefinitions expr in
      join [acc, children]
    ) [] expr in
    join [lookup, children]

  sem recursiveDeclDefinitions: Decl -> DefinitionSeq
  sem recursiveDeclDefinitions =| decl ->
    let lookup = declDefinitions decl in

    let childExprs = sfold_Decl_Expr (lam acc. lam expr.
      let children = recursiveExprDefinitions expr in
      join [acc, children]
    ) [] decl in

    let childDecls = sfold_Decl_Decl (lam acc. lam decl.
      let children = recursiveDeclDefinitions decl in
      join [acc, children]
    ) [] decl in

    join [lookup, childExprs, childDecls]

  -- sem variablesLookup: MLangFile -> [(Info, LookupResult)]
  -- sem variablesLookup =| file ->
  --   match (getProgram file, getSymEnv file)
  --     with (Some program, Some env) then
  --       join [
  --         flatMap (recursiveDeclLookup file env) program.decls,
  --         exprLookup file env program.expr
  --       ]
  --     else
  --       []
end