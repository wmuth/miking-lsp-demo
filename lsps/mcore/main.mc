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

lang MLangScope = MLangRoot + LSPRoot
  type AvailableName = {
    location: Info,
    name: Name,
    kind: CompletionItemKind,
    documentation: Option String
  }

  type LSEnv = {
    availableVariables: [AvailableName],
    availableTypes: [AvailableName]
  }
end

let emptyLSEnv: use MLangScope in LSEnv = {
  availableVariables = [],
  availableTypes = []
}

lang MLangLanguageServerCompiler = MLangRoot + MLangScope
  type LSCompileResult = (LSEnv, [LanguageServerPayload])

  sem getTypeNames : Type -> [Name]
  sem getTypeNames =
  | _ -> []
  | typ & TyCon { ident=ident, info=info } ->
    [ident]

  sem getTypeNamesRecursively : Type -> [Name]
  sem getTypeNamesRecursively =| typ ->
    let childTypes = sfold_Type_Type (lam acc. lam ty.
      join [acc, getTypeNamesRecursively ty]
    ) [] typ in
    join [getTypeNames typ, childTypes]

  sem typeLookup: MLangFile -> SymEnv -> Type -> [LanguageServerPayload]
  sem typeLookup file env =
  | _ -> []
  | TyAlias { display=TyCon { ident=ident, info=info }, content=typ }
  | typ & TyCon { ident=ident, info=info }
  | typ & TyVar { ident=ident, info=info }
  | typ & TyAll { ident=ident, info=info }
  | typ & TyUse { ident=ident, info=info } ->
    let filename = file.filename in
    let info = infoWithFilename filename info in
    join [
      [
        LsHover {
          location = info,
          toString = lam. Some (join ["`", nameGetStr ident, "` (type Ty)", getSym ident])
        },
        LsUsage {
          location = info,
          name = ident
        }
      ],
      sfold_Type_Type (lam acc. lam ty.
        join [
          acc,
          [
            LsType {
              location = info,
              ident = ident,
              superIdents = getTypeNamesRecursively ty
            }
          ]
        ]
      ) [] typ
    ]

  sem exprLookup: MLangFile -> SymEnv -> LSEnv -> Expr -> LSCompileResult
  sem exprLookup file symEnv env =
  | _ -> (env, [])
  | TmUse { ident=ident, ty=ty, info=info } ->
    (
      env,
      [
        LsType {
          location = info,
          ident = ident,
          superIdents = getTypeNamesRecursively ty
        },
        LsHover {
          location = info,
          toString = lam. Some (join ["`", nameGetStr ident, "` `<", type2str ty, ">`", getSym ident])
        },
        LsUsage {
          location = info,
          name = ident
        }
      ]
    )
  | TmRecord { bindings=bindings, info=info } ->
    (
      env,
      [
        LsHover {
          location = info,
          toString = lam. Some (join ["`record`"])
        }
      ]
    )
  | TmRecLets { bindings=bindings } ->
    (
      env,
      map (lam binding.
        LsDefinition {
          kind = SymbolFunction (),
          location = binding.info,
          name = binding.ident
        }
      ) bindings
    )
  | TmLet { ident=ident, ty=ty, info=info, inexpr=inexpr } ->
    let documentation = join ["`", nameGetStr ident, "` `<", type2str ty, ">` (TmLet)", getSym ident] in
    (
      env,
      [
        -- LsCompletion {
        --   location = Some (infoTm inexpr),
        --   getCompletion = lam. {
        --     label = nameGetStr ident,
        --     kind = CompletionVariable (),
        --     insertText = None (),
        --     deprecated = false,
        --     documentation = Some documentation
        --   }
        -- },
        LsHover {
          location = info,
          toString = lam. Some documentation
        },
        LsDefinition {
          kind = SymbolTypeParameter (),
          location = info,
          name = ident
        }
      ]
    )
  | TmLam { ident=ident, ty=ty, info=info, body=body } ->
    (
      env,
      -- {
      --   env with
      --   availableVariables = concat env.availableVariables [{
      --     location = info,
      --     name = ident,
      --     kind = CompletionVariable (),
      --     documentation = None ()
      --   }]
      -- },
      [
        -- LsCompletion {
        --   location = Some info,
        --   getCompletion = lam. {
        --     label = nameGetStr ident,
        --     kind = CompletionVariable (),
        --     insertText = None (),
        --     deprecated = false,
        --     documentation = Some (join ["`", nameGetStr ident, "` `<", type2str ty, ">` (definition)", getSym ident])
        --   }
        -- },
        LsHover {
          location = info,
          toString = lam. Some (join ["`", nameGetStr ident, "` `<", type2str ty, ">` (definition)", getSym ident])
        },
        LsDefinition {
          kind = SymbolTypeParameter (),
          location = info,
          name = ident
        }
      ]
    )
  | TmType { ident=ident, ty=ty, info=info, inexpr=inexpr } ->
    (
      {
        env with
        availableTypes = concat env.availableTypes [{
          location = info,
          name = ident,
          kind = CompletionTypeParameter (),
          documentation = None ()
        }]
      },
      [
        LsType {
          location = info,
          ident = ident,
          superIdents = getTypeNamesRecursively ty
        },
        LsHover {
          location = info,
          toString = lam. Some (join ["`", nameGetStr ident, "` `<", type2str ty, ">` (definition)", getSym ident])
        },
        LsDefinition {
          kind = SymbolTypeParameter (),
          location = info,
          name = ident
        }
      ]
    )
  | TmVar { ident=ident, ty=ty, info=info } ->
    (
      env,
      [
        LsHover {
          location = info,
          toString = lam. Some (join ["`", nameGetStr ident, "` `<", type2str ty, ">` (TmVar)", getSym ident])
        },
        LsUsage {
          location = info,
          name = ident
        }
      ]
    )
  | TmConDef { ident=ident, ty=ty, info=info }
  | TmConApp { ident=ident, ty=ty, info=info }
  | TmExt { ident=ident, ty=ty, info=info } ->
    (
      env,
      [
        -- LsCompletion {
        --   location = Some info,
        --   getCompletions = lam. map (lam v. {
        --     label = nameGetStr v.name,
        --     kind = v.kind,
        --     insertText = None (),
        --     deprecated = false,
        --     documentation = v.documentation
        --   }) env.availableTypes 
        -- },
        LsType {
          location = info,
          ident = ident,
          superIdents = getTypeNamesRecursively ty
        },
        LsHover {
          location = info,
          toString = lam. Some (join ["`", nameGetStr ident, "` `<", type2str ty, ">` (TmConApp)", getSym ident])
        },
        LsUsage {
          location = info,
          name = ident
        }
      ]
    )
  | TmUtest { info = info & Info r } ->
    (
      env,
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
    )

  sem patLookup: MLangFile -> SymEnv -> Path -> Pat -> [LanguageServerPayload]
  sem patLookup file env filename =
  | _ -> []
  | PatSeqEdge { middle=PName ident, info=info }
  | PatNamed { ident=PName ident, info=info } ->
    let info = infoWithFilename filename info in
    [
      LsDefinition {
        kind = SymbolConstructor (),
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
        kind = SymbolVariable (),
        location = info,
        name = ident
      }
    ]
  | DeclType { ident=ident, info=info, tyIdent=tyIdent }
  | DeclConDef { ident=ident, info=info, tyIdent=tyIdent }
  | DeclExt { ident=ident, info=info, tyIdent=tyIdent } ->
    join [
      [
        LsHover {
          location = info,
          toString = lam. Some (join ["`", nameGetStr ident, "` (type decl)", getSym ident, ", ", type2str tyIdent])
        },
        LsDefinition {
          kind = SymbolTypeParameter (),
          location = info,
          name = ident
        },
        LsType {
          location = info,
          ident = ident,
          superIdents = getTypeNamesRecursively tyIdent
        }
      ]
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
          kind = SymbolTypeParameter (),
          location = info,
          name = ident
        }
      ],
      flatMap (lam def.
        [
          LsDefinition {
            kind = SymbolEnumMember (),
            location = info,
            name = def.ident
          }
        ]
      ) defs
    ]
  | DeclRecLets { bindings=bindings, info=info } ->
    let filename = file.filename in
    flatMap (lam binding.
      [
        LsHover {
          location = info,
          toString = lam. Some (join ["`", nameGetStr binding.ident, "` (let)", getSym binding.ident])
        },
        LsDefinition {
          kind = SymbolVariable (),
          location = infoWithFilename filename info,
          name = binding.ident
        }
      ]
    ) bindings
  | DeclLang { ident=ident, includes=includes, info=info } ->
    [
      LsHover {
        location = info,
        toString = lam. Some (nameGetStr ident)
      },
      LsDefinition {
        kind = SymbolModule (),
        location = info,
        name = ident
      }
    ]
  | DeclSem { ident=ident, info=info, args=args, cases=cases, info=info, tyAnnot=tyAnnot } ->
    let patterns = map (lam cas. cas.pat) cases in
    join [
      [
        LsType {
          location = info,
          ident = ident,
          superIdents = getTypeNamesRecursively tyAnnot
        },
        LsHover {
          location = info,
          toString = lam. Some (join ["`", nameGetStr ident, "` (sem)", getSym ident])
        },
        LsDefinition {
          kind = SymbolFunction (),
          location = info,
          name = ident
        }
      ],
      map (lam arg. 
        LsDefinition {
          kind = SymbolVariable (),
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

  sem recursiveExprLookup: MLangFile -> SymEnv -> LSEnv -> Expr -> [LanguageServerPayload]
  sem recursiveExprLookup file symEnv env =| expr ->
    let filename = file.filename in
    match exprLookup file symEnv env expr with (env, self) in

    let childTypes = sfold_Expr_Type (lam acc. lam ty.
      join [acc, recursiveTypeLookup file symEnv ty]
    ) [] expr in

    let childExprs = sfold_Expr_Expr (lam acc. lam expr.
      join [acc, recursiveExprLookup file symEnv env expr]
    ) [] expr in

    let childPatterns = sfold_Expr_Pat (lam acc. lam pat.
      join [acc, recursivePatLookup file symEnv pat]
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
      join [acc, recursiveExprLookup file env emptyLSEnv expr]
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
      optionGetOr [] (optionMap (lam program. recursiveExprLookup file symEnv emptyLSEnv program.expr) program)
    ]) res in

    optionGetOr [] languageSupport

  -- sem intrinsicInLanguageSupport: MLangFile -> LSEnv -> LSCompileResult
  -- sem intrinsicInLanguageSupport =| file ->
  --   let filename = file.filename in
  --   let info = infoWithFilename filename (Info filename 1 0) in
    
  --   [
  --     -- LsCompletion {
  --     --   location = Some info,
  --     --   getCompletions = lam. map (lam v. {
  --     --     label = nameGetStr v.name,
  --     --     kind = v.kind,
  --     --     insertText = None (),
  --     --     deprecated = false,
  --     --     documentation = v.documentation
  --     --   }) env.availableVariables 
  --     -- }
  --   ]

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
            kind = SymbolFile (),
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