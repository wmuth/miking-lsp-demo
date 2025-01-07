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
  | TmRecLets { bindings=bindings } ->
    map (lam binding. (binding.ident, binding.info)) bindings

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
  | DeclRecLets { bindings=bindings, info=Info info } ->
    -- Ugly hack to keep the filename of the bindings
    -- the same as the filename of the decl.
    filterMap (lam binding.
      match binding.info with Info r then
        Some (binding.ident, Info {
          r with
          filename = info.filename
        })
      else 
        None ()
    ) bindings
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
end