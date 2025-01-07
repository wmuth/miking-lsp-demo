include "./file.mc"

-- Lookup elements depending on location,
-- and return the element with the highest precedence (innermost contained element).
let createLookup: [(Info, LookupResult)] -> Path -> Int -> Int -> Option LookupResult =
  lam lookups. lam uri. lam row. lam col.
    let foundVariables: [LookupResult] = filterMap (
      lam lookup.
        match lookup with (info, lookup) in
        if infoCollision info uri row col
          then Some lookup
          else None ()
    ) lookups in

    match foundVariables with [x] ++ seq then
      let f = lam var1. lam var2.
        if infoContainsInfo var1.info var2.info then var1 else var2
      in
      Some (foldl f x seq)
    else
      None ()

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
  | TmLam { ident=ident, ty=ty, info=info }
  | TmType { ident=ident, ty=ty, info=info } ->
    [(
      info,
      {
        info = info,
        pprint = lam. Some (
          join [
            "`", nameGetStr ident, "`",
            " `<",
            type2str ty,
            ">`"
          ]
        ),
        lookupDefinition = None ()
      }
    )]
  | TmMatch {
    thn=TmVar { ident=ident },
    ty=ty,
    info=info,
    pat=p & PatRecord { bindings=bindings }
  } ->
    [(
      info,
      {
        info = info,
        pprint = lam. Some (nameGetStr ident),
        lookupDefinition = None ()
      }
    )]
  -- | TmRecLets { bindings=bindings } ->
  --     -- The info field appears to point to just the "let" keyword??
  --     let f = lam acc. lam x.
  --       mapInsertVariable x.info (x.ident, x.tyAnnot) acc
  --     in
  --     foldl f m bindings

  sem declLookup: SymEnv -> Decl -> [(Info, LookupResult)]
  sem declLookup env =
  | _ -> []

  sem recursiveExprLookup: SymEnv -> Expr -> [(Info, LookupResult)]
  sem recursiveExprLookup env =| expr ->
    let lookup = exprLookup env expr in
    let children = sfold_Expr_Expr (lam acc. lam expr.
      let children = recursiveExprLookup env expr in
      join [acc, children]
    ) [] expr in
    join [lookup, children]

  sem recursiveDeclLookup: SymEnv -> Decl -> [(Info, LookupResult)]
  sem recursiveDeclLookup env =| decl ->
    let lookup = declLookup env decl in

    let childExprs = sfold_Decl_Expr (lam acc. lam expr.
      let children = recursiveExprLookup env expr in
      join [acc, children]
    ) [] decl in

    let childDecls = sfold_Decl_Decl (lam acc. lam decl.
      let children = recursiveDeclLookup env decl in
      join [acc, children]
    ) [] decl in

    join [lookup, childExprs, childDecls]

  sem variablesLookup: MLangFile -> [(Info, LookupResult)]
  sem variablesLookup =| file ->
    match (getProgram file, getSymEnv file)
      with (Some program, Some env) then
        join [
          flatMap (recursiveDeclLookup env) program.decls,
          exprLookup env program.expr
        ]
      else
        []
end