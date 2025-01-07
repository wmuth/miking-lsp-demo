lang MLangLookupInclude = MLangFileHandler
  sem includesLookup: MLangFile -> [(Info, LookupResult)]
  sem includesLookup =| file ->
    let f: (Info, Include) -> LookupResult = lam infoInclude.
      match infoInclude with (info, inc) in
      let lookupDefinition = match inc
        with ExistingFile path then Some (lam. makeInfo {filename=path, row=1, col=1} {filename=path, row=1, col=1})
        else None ()
      in
      {
        info = info,
        pprint = lam. inc2str inc,
        lookupDefinition = lookupDefinition
      }
    in

    let includes = map (lam inc. (inc.0, inc.1)) (getIncludes file) in
    map (lam v. (v.0, f v)) includes
end

lang MLangLookupVariable = MLangFileHandler + MLangPipeline
  sem exprLookup: SymEnv -> Expr -> [(Info, LookupResult)]
  sem exprLookup env =
  | _ -> []
  | TmLet { ident=ident, ty=ty, info=info }
  | TmVar { ident=ident, ty=ty, info=info }
  | TmConApp { ident=ident, ty=ty, info=info }
  | TmType { ident=ident, ty=ty, info=info } ->
    [(
      info,
      {
        info = info,
        pprint = lam. Some (nameGetStr ident),
        lookupDefinition = None ()
      }
    )]
  -- | TmMatch {
  --     thn=TmVar { ident=ident },
  --     ty=ty,
  --     info=info,
  --     pat=p & PatRecord { bindings=bindings }
  --   } ->
  --   -- eprintln (join ["Match: ", expr2str p]);
  --   mapInsertVariable info (ident, ty) m
  -- | TmRecLets { bindings=bindings } ->
  --     -- The info field appears to point to just the "let" keyword??
  --     let f = lam acc. lam x.
  --       mapInsertVariable x.info (x.ident, x.tyAnnot) acc
  --     in
  --     foldl f m bindings

  sem recursiveExprLookup: SymEnv -> Expr -> [(Info, LookupResult)]
  sem recursiveExprLookup env =| expr ->
    sfold_Expr_Expr (lam acc. lam expr.
      let lookup = exprLookup env expr in
      let children = recursiveExprLookup env expr in
      join [acc, children, lookup]
    ) [] expr

  sem declLookup: SymEnv -> Decl -> [(Info, LookupResult)]
  sem declLookup env =
  | decl -> []
  | DeclLet { body = body } -> recursiveExprLookup env body

  sem variablesLookup: MLangFile -> [(Info, LookupResult)]
  sem variablesLookup =| file ->
    match (getProgram file, getSymEnv file)
      with (Some program, Some env) then
        join [
          flatMap (declLookup env) program.decls,
          exprLookup env program.expr
        ]
      else
        []
end