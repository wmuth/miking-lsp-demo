include "./file.mc"

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

let setLookupResultFilename: Path -> [(Info, LookupResult)] -> [(Info, LookupResult)] =
  lam filename. lam lookups.
    map (lam lookup.
      match lookup with (info, lookup) in
      (infoWithFilename filename info, lookup)
    ) lookups

lang MLangLookupInclude = MLangFileHandler
  sem includesLookup: MLangFile -> [(Info, LookupResult)]
  sem includesLookup =| file ->
    let f: (Info, Include) -> LookupResult = lam infoInclude.
      match infoInclude with (info, inc) in

      let lookupDefinition = lam. match inc
        with ExistingFile path then Some (makeInfo {filename=path, row=1, col=1} {filename=path, row=1, col=1})
        else None ()
      in

      {
        info = info,
        pprint = lam. inc2str inc,
        lookupDefinition = Some lookupDefinition
      }
    in

    let includes = map (lam inc. (inc.0, inc.1)) (getIncludes file) in
    map (lam v. (v.0, f v)) includes
end

lang MLangLookupVariable = MLangFileHandler + MLangPipeline
  sem getDefinitionLookup: MLangFile -> Name -> (() -> Option Info)
  sem getDefinitionLookup file =| name -> lam. mapLookup name (getDefinitions file)

  sem typeLookup: MLangFile -> SymEnv -> Type -> [(Info, LookupResult)]
  sem typeLookup file env =
  | _ -> []
  | TyCon { ident=ident, info=info }
  | TyVar { ident=ident, info=info }
  | TyAll { ident=ident, info=info } ->
    [(
      info,
      {
        info = info,
        pprint = lam. Some (
          join [
            "`", nameGetStr ident, "`",
            " (type)"
          ]
        ),
        lookupDefinition = Some (getDefinitionLookup file ident)
      }
    )]

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

  sem patLookup: MLangFile -> SymEnv -> Path -> Pat -> [(Info, LookupResult)]
  sem patLookup file env filename =
  | _ -> []
  | pat & PatSeqEdge { middle=PName ident, info=info }
  | pat & PatNamed { ident=PName ident, info=info }
  | pat & PatCon { ident=ident, info=info } ->
    let info = infoWithFilename filename info in
    [(
      info,
      {
        info = info,
        pprint = lam. Some (join [
          nameGetStr ident,
          "\n",
          -- TODO: We need to store the defined types.
          match getPatStringCode 0 pprintEnvEmpty pat with (_env,pat) in pat
        ]),
        lookupDefinition = Some (getDefinitionLookup file ident)
      }
    )]

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
  | DeclSem { ident=ident, info=info, args=args, cases=cases, info=info } ->
    let patterns = map (lam cas. cas.pat) cases in

    join [
      [(
        info,
        {
          info = info,
          pprint = lam. Some (nameGetStr ident),
          lookupDefinition = None ()
        }
      )]
    ]

  sem recursiveTypeLookup: MLangFile -> SymEnv -> Path -> Type -> [(Info, LookupResult)]
  sem recursiveTypeLookup file env filename =| ty ->
    let self = typeLookup file env ty in

    let childTypes = sfold_Type_Type (lam acc. lam ty.
      let types = recursiveTypeLookup file env filename ty in
      let types = setLookupResultFilename filename types in
      join [acc, types]
    ) [] ty in

    join [self, childTypes]

  sem recursivePatLookup: MLangFile -> SymEnv -> Path -> Pat -> [(Info, LookupResult)]
  sem recursivePatLookup file env filename =| pat ->
    let self = patLookup file env filename pat in

    let childTypes = sfold_Pat_Type (lam acc. lam ty.
      join [acc, recursiveTypeLookup file env filename ty]
    ) [] pat in

    let childPatterns = sfold_Pat_Pat (
      lam acc. lam pat.
        join [acc, recursivePatLookup file env filename pat]
    ) [] pat in

    join [self, childTypes, childPatterns]

  sem recursiveExprLookup: MLangFile -> SymEnv -> Expr -> [(Info, LookupResult)]
  sem recursiveExprLookup file env =| expr ->
    let filename = getFilename (infoTm expr) in
    let self = exprLookup file env expr in

    let childTypes = sfold_Expr_Type (lam acc. lam ty.
      join [acc, recursiveTypeLookup file env filename ty]
    ) [] expr in

    let childExprs = sfold_Expr_Expr (lam acc. lam expr.
      join [acc, recursiveExprLookup file env expr]
    ) [] expr in

    let childPatterns = sfold_Expr_Pat (lam acc. lam pat.
      join [acc, recursivePatLookup file env filename pat]
    ) [] expr in

    join [self, childExprs, childPatterns, childTypes]

  sem recursiveDeclLookup: MLangFile -> SymEnv -> Decl -> [(Info, LookupResult)]
  sem recursiveDeclLookup file env =| decl ->
    let filename = (getFilename (infoDecl decl)) in
    let self = declLookup file env decl in

    let childPatterns = sfold_Decl_Pat (lam acc. lam pat.
      join [acc, recursivePatLookup file env filename pat]
    ) [] decl in

    let childTypes = sfold_Decl_Type (lam acc. lam ty.
      join [acc, recursiveTypeLookup file env filename ty]
    ) [] decl in

    let childExprs = sfold_Decl_Expr (lam acc. lam expr.
      join [acc, recursiveExprLookup file env expr]
    ) [] decl in

    let childDecls = sfold_Decl_Decl (lam acc. lam decl.
      join [acc, recursiveDeclLookup file env decl]
    ) [] decl in

    join [self, childExprs, childDecls, childTypes, childPatterns]

  sem variablesLookup: MLangFile -> [(Info, LookupResult)]
  sem variablesLookup =| file ->
    match (getProgram file, getSymEnv file)
      with (Some program, Some env) then
        join [
          flatMap (recursiveDeclLookup file env) program.decls,
          recursiveExprLookup file env program.expr
        ]
      else
        []
end