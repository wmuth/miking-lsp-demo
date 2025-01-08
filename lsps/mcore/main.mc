include "./file.mc"

-- This is very crude and wrong,
-- it doesn't even handle columns.
let getContentInSection: String -> Info -> String =
  lam content. lam info.
    match info with Info info then
      let start = info.row1 in
      let endd = subi info.row2 info.row1 in
      let lines = strSplit "\n" content in
      let lines = subsequence lines start endd in
      strJoin "\n" lines
    else content

-- lang LSPLang
--   syn LSPInfo =
--   | Hover { info: Info, value: () -> String }
--   | Definition { info: Info, location: Info }
-- end

lang MLangLSP = MLangFileHandler + MLangPipeline
  sem getDefinitionLookup: MLangFile -> Name -> (() -> Option Info)
  sem getDefinitionLookup file =| name -> lam. mapLookup name (getDefinitions file)

  sem exprLookup: MLangFile -> SymEnv -> Expr -> [(Info, LookupResult)]
  sem exprLookup file env =
  | _ -> []
  | TmLet { ident=ident, ty=ty, info=info }
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
            ">` (definition)"
          ]
        ),
        lookupDefinition = None ()
      }
    )]
  | TmVar { ident=ident, ty=ty, info=info }
  | TmConApp { ident=ident, ty=ty, info=info } ->
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
        lookupDefinition = Some (getDefinitionLookup file ident)
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

  sem declLookup: MLangFile -> SymEnv -> Decl -> [(Info, LookupResult)]
  sem declLookup file env =
  | _ -> []
  | DeclLang { ident=ident, includes=includes, info=info } ->
    -- Here we extract the languages from the DeclLang includes.
    -- In a very crude way by looking at the original source code.
    -- CURRENTLY NOT USED
    let content = getContent file in
    let content = getContentInSection content info in
    let languages = map strTrim (strSplit "+" content) in
    -- eprintln (join ["Languages: ", strJoin "," languages]);

    [(
      info,
      {
        info = info,
        pprint = lam. Some (nameGetStr ident),
        lookupDefinition = None ()
      }
    )]

  sem recursiveExprLookup: MLangFile -> SymEnv -> Expr -> [(Info, LookupResult)]
  sem recursiveExprLookup file env =| expr ->
    let lookup = exprLookup file env expr in
    let children = sfold_Expr_Expr (lam acc. lam expr.
      let children = recursiveExprLookup file env expr in
      join [acc, children]
    ) [] expr in
    join [lookup, children]

  sem recursiveDeclLookup: MLangFile -> SymEnv -> Decl -> [(Info, LookupResult)]
  sem recursiveDeclLookup file env =| decl ->
    let lookup = declLookup file env decl in

    let childExprs = sfold_Decl_Expr (lam acc. lam expr.
      let children = recursiveExprLookup file env expr in
      join [acc, children]
    ) [] decl in

    let childDecls = sfold_Decl_Decl (lam acc. lam decl.
      let children = recursiveDeclLookup file env decl in
      join [acc, children]
    ) [] decl in

    join [lookup, childExprs, childDecls]

  sem variablesLookup: MLangFile -> [(Info, LookupResult)]
  sem variablesLookup =| file ->
    eprintln "Looking up variables";
    eprintln (printFileKind file);
    match (getProgram file, getSymEnv file)
      with (Some program, Some env) then
        join [
          flatMap (recursiveDeclLookup file env) program.decls,
          recursiveExprLookup file env program.expr
        ]
      else
        []
end