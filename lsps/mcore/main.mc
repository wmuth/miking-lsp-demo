include "./root.mc"

let debugSym = true

let getSym = 
  if debugSym then
    lam name.
      optionGetOr
      "No symbol"
      (optionMap (compose int2string sym2hash) (nameGetSym name))
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

lang MLangLanguageServerCompiler = MLangRoot
  sem typeLookup: MLangFile -> SymEnv -> Type -> [LanguageServerPayload]
  sem typeLookup file env =
  | _ -> []
  | TyCon { ident=ident, info=info }
  | TyVar { ident=ident, info=info }
  | TyAll { ident=ident, info=info }
  | TyUse { ident=ident, info=info } ->
    let filename = file.filename in
    let info = infoWithFilename filename info in
    [
      LsHover {
        location = info,
        toString = lam. Some (join ["`", nameGetStr ident, "` (type)", getSym ident])
      },
      LsUsage {
        location = info,
        name = ident
      }
    ]

  sem exprLookup: MLangFile -> SymEnv -> Expr -> [LanguageServerPayload]
  sem exprLookup file env =
  | _ -> []
  | TmUse { ident=ident, ty=ty, info=info } ->
    [
      LsHover {
        location = info,
        toString = lam. Some (join ["`", nameGetStr ident, "` `<", type2str ty, ">`", getSym ident])
      },
      LsUsage {
        location = info,
        name = ident
      }
    ]
  | TmRecord { bindings=bindings, info=info } ->
    [
      LsHover {
        location = info,
        toString = lam. Some (join ["`record`"])
      }
    ]
  | TmRecLets { bindings=bindings } ->
    map (lam binding.
      LsDefinition {
        location = binding.info,
        name = binding.ident
      }
    ) bindings
  | TmLet { ident=ident, ty=ty, info=info }
  | TmLam { ident=ident, ty=ty, info=info }
  | TmType { ident=ident, ty=ty, info=info } ->
    [
      LsHover {
        location = info,
        toString = lam. Some (join ["`", nameGetStr ident, "` `<", type2str ty, ">` (definition)", getSym ident])
      },
      LsDefinition {
        location = info,
        name = ident
      }
    ]
  | TmVar { ident=ident, ty=ty, info=info }
  | TmConDef { ident=ident, ty=ty, info=info }
  | TmConApp { ident=ident, ty=ty, info=info }
  | TmExt { ident=ident, ty=ty, info=info } ->
    [
      LsHover {
        location = info,
        toString = lam. Some (join ["`", nameGetStr ident, "` `<", type2str ty, ">` (TmConApp)", getSym ident])
      },
      LsUsage {
        location = info,
        name = ident
      }
    ]
  | TmUtest { info = info & Info r } ->
    [
      LsCodeLens {
        title = "Run Test (expr)",
        ideCommand = "mcore.debugSingle",
        arguments = [
          JsonString r.filename,
          JsonString (info2str info)
        ],
        data = None (),
        location = info
      }
    ]

  sem patLookup: MLangFile -> SymEnv -> Path -> Pat -> [LanguageServerPayload]
  sem patLookup file env filename =
  | _ -> []
  | PatSeqEdge { middle=PName ident, info=info }
  | PatNamed { ident=PName ident, info=info } ->
    let info = infoWithFilename filename info in
    [
      LsDefinition {
        location = info,
        name = ident
      },
      LsHover {
        location = info,
        toString = lam. Some (join ["`", nameGetStr ident, "` (PatSeqEdge / PatNamed)", getSym ident])
        -- match getPatStringCode 0 pprintEnvEmpty pat with (_env,pat) in pat
      }
    ]
  | PatCon { ident=ident, info=info } ->
    let info = infoWithFilename filename info in
    [
      LsUsage {
        location = info,
        name = ident
      },
      LsHover {
        location = info,
        toString = lam. Some (join ["`", nameGetStr ident, "` (PatCon)", getSym ident])
        -- match getPatStringCode 0 pprintEnvEmpty pat with (_env,pat) in pat
      }
    ]

  sem declLookup: MLangFile -> SymEnv -> Decl -> [LanguageServerPayload]
  sem declLookup file env =
  | _ -> []
  | DeclInclude { path=path, info=info } ->
    [
      LsHover {
        location = info,
        toString = lam. Some (join ["`", path, "` (include)"])
      }
    ]
  | DeclLet { info = info, ident = ident } ->
    [
      LsHover {
        location = info,
        toString = lam. Some (join ["`", nameGetStr ident, "` (let)", getSym ident])
      },
      LsDefinition {
        location = info,
        name = ident
      }
    ]
  | DeclType { ident=ident, info=info }
  | DeclConDef { ident=ident, info=info }
  | DeclExt { ident=ident, info=info } ->
    [
      LsDefinition {
        location = info,
        name = ident
      }
    ]
  | DeclSyn { ident=ident, info=info, defs=defs } ->
    let filename = file.filename in
    join [
      [
        LsHover {
          location = info,
          toString = lam. Some (join ["`", nameGetStr ident, "` (syn)", getSym ident])
        },
        LsDefinition {
          location = info,
          name = ident
        }
      ],
      flatMap (lam def.
        [
          LsDefinition {
            location = info,
            name = def.ident
          }
        ]
      ) defs
    ]
  | DeclRecLets { bindings=bindings, info=info } ->
    let filename = file.filename in
    map (lam binding.
      LsDefinition {
        location = infoWithFilename filename info,
        name = binding.ident
      }
    ) bindings
  | DeclLang { ident=ident, includes=includes, info=info } ->
    -- Here we extract the languages from the DeclLang includes.
    -- In a very crude way by looking at the original source code.
    -- CURRENTLY NOT USED
    let content = file.content in
    let content = getContentInSection content info in
    let languages = map strTrim (strSplit "+" content) in
    -- eprintln (join ["Languages: ", strJoin "," languages]);

    [
      LsHover {
        location = info,
        toString = lam. Some (nameGetStr ident)
      }
    ]
  | DeclSem { ident=ident, info=info, args=args, cases=cases, info=info } ->
    let patterns = map (lam cas. cas.pat) cases in
    join [
      [
        LsHover {
          location = info,
          toString = lam. Some (join ["`", nameGetStr ident, "` (sem)", getSym ident])
        },
        LsDefinition {
          location = info,
          name = ident
        }
      ],
      map (lam arg. 
        LsDefinition {
          location = info,
          name = arg.ident
        }  
      ) (optionGetOr [] args)
    ]
  | DeclUtest { info = info & Info r } ->
    [
      LsCodeLens {
        title = "Run Test (decl)",
        ideCommand = "mcore.debugSingle",
        arguments = [
          JsonString r.filename,
          JsonString (info2str info)
        ],
        data = None (),
        location = info
      }
    ]

  sem recursiveTypeLookup: MLangFile -> SymEnv -> Type -> [LanguageServerPayload]
  sem recursiveTypeLookup file env =| ty ->
    let filename = file.filename in
    let self = typeLookup file env ty in

    let childTypes = sfold_Type_Type (lam acc. lam ty.
      let types = recursiveTypeLookup file env ty in
      join [acc, types]
    ) [] ty in

    join [self, childTypes]

  sem recursivePatLookup: MLangFile -> SymEnv -> Pat -> [LanguageServerPayload]
  sem recursivePatLookup file env =| pat ->
    let filename = file.filename in
    let self = patLookup file env filename pat in

    let childTypes = sfold_Pat_Type (lam acc. lam ty.
      join [acc, recursiveTypeLookup file env ty]
    ) [] pat in

    let childPatterns = sfold_Pat_Pat (
      lam acc. lam pat.
        join [acc, recursivePatLookup file env pat]
    ) [] pat in

    join [self, childTypes, childPatterns]

  sem recursiveExprLookup: MLangFile -> SymEnv -> Expr -> [LanguageServerPayload]
  sem recursiveExprLookup file env =| expr ->
    let filename = file.filename in
    let self = exprLookup file env expr in

    let childTypes = sfold_Expr_Type (lam acc. lam ty.
      join [acc, recursiveTypeLookup file env ty]
    ) [] expr in

    let childExprs = sfold_Expr_Expr (lam acc. lam expr.
      join [acc, recursiveExprLookup file env expr]
    ) [] expr in

    let childPatterns = sfold_Expr_Pat (lam acc. lam pat.
      join [acc, recursivePatLookup file env pat]
    ) [] expr in

    join [self, childExprs, childPatterns, childTypes]

  sem recursiveDeclLookup: MLangFile -> SymEnv -> Decl -> [LanguageServerPayload]
  sem recursiveDeclLookup file env =| decl ->
    let filename = file.filename in
    let self = declLookup file env decl in

    let childPatterns = sfold_Decl_Pat (lam acc. lam pat.
      join [acc, recursivePatLookup file env pat]
    ) [] decl in

    let childTypes = sfold_Decl_Type (lam acc. lam ty.
      join [acc, recursiveTypeLookup file env ty]
    ) [] decl in

    let childExprs = sfold_Decl_Expr (lam acc. lam expr.
      join [acc, recursiveExprLookup file env expr]
    ) [] decl in

    let childDecls = sfold_Decl_Decl (lam acc. lam decl.
      join [acc, recursiveDeclLookup file env decl]
    ) [] decl in

    join [self, childExprs, childDecls, childTypes, childPatterns]

  sem programToLanguageSupport: MLangFile -> [LanguageServerPayload]
  sem programToLanguageSupport =| file ->
    let res = switch (file.status, file.symbolized)
      case (Changed (), _) then
        None ()
      case (Symbolized (), Some symbolized) then
        Some (symbolized.symEnv, symbolized.program)
      case (_, Some symbolized) then
        Some (symEnvEmpty, symbolized.program)
      case _ then
        error "Unhandeled case in programToLanguageSupport"
    end in

    let languageSupport = optionMap (lam res. match res with (symEnv, program) in join [
      flatMap (recursiveDeclLookup file symEnv) (optionGetOr [] (optionMap (lam program. program.decls) program)),
      optionGetOr [] (optionMap (lam program. recursiveExprLookup file symEnv program.expr) program)
    ]) res in

    optionGetOr [] languageSupport

  sem linksToLanguageSupport: MLangFile -> [LanguageServerPayload]
  sem linksToLanguageSupport =
  | file & { status = Symbolized () | Linked (), linked=Some linked } ->
    flatMap (
      lam link.
        match link with (info, path) in
        let fileName = nameSym path in
        [
          LsHover {
            location = info,
            toString = lam. Some (join ["`", path, "` (link)"])
          },
          LsDefinition {
            location = makeInfo (posVal path 1 0) (posVal path 1 0),
            name = fileName
          },
          LsUsage {
            location = info,
            name = fileName
          }
        ]
    ) linked.links

  sem fileToLanguageSupport: MLangFile -> [LanguageServerPayload]
  sem fileToLanguageSupport =| file ->
    let programLanguageSupport = programToLanguageSupport file in
    let linksLanguageSupport = linksToLanguageSupport file in
    let diagnostics = map (lam diagnostic. LsDiagnostic diagnostic) (getFileDiagnostics file) in

    join [
      programLanguageSupport,
      linksLanguageSupport,
      diagnostics
    ]
end