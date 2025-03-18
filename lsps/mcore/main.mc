include "./root.mc"

let debugSym = false

let mcoreCode = lam code. join ["```mcore\n", code, "\n```"]

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

lang MLangLanguageServerCompiler = MLangRoot + MLangScope + MLangPrettyPrint
  type LSCompileResult = (LSEnv, [LanguageServerPayload])

  sem getTypeNames : Type -> [Name]
  sem getTypeNames =
  | _ -> []
  | TyCon { ident=ident, info=info } ->
    -- eprintln (join ["getTypeNames: ", nameGetStr ident]);
    [ident]

  sem getTypeNamesRecursively : Type -> [Name]
  sem getTypeNamesRecursively =| typ ->
    -- eprintln (join ["getTypeNamesRecursively: ", type2str typ]);
    let myTypes = getTypeNames typ in
    sfold_Type_Type (lam acc. lam ty.
      join [acc, getTypeNamesRecursively ty]
    ) myTypes typ

  sem typeLookup: MLangFile -> SymEnv -> Type -> [LanguageServerPayload]
  sem typeLookup file env =
  | _ -> []
  | TyAlias { display=TyCon { ident=ident, info=info }, content=typ }
  | TyCon { ident=ident, info=info, data=typ }
  | typ & TyVar { ident=ident, info=info }
  | typ & TyAll { ident=ident, info=info }
  | typ & TyUse { ident=ident, info=info } ->
    let filename = file.filename in
    let info = infoWithFilename filename info in
    join [
      [
        LsHover {
          location = info,
          toString = lam. Some (join [
            mcoreCode (join ["type ", nameGetStr ident, ": ", type2str typ]),
            getSym ident
          ])
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
  | TmUse { ident=ident, ty=ty, info=info, langEnv={
    varEnv = varEnv,
    conEnv = conEnv,
    tyVarEnv = tyVarEnv,
    tyConEnv = tyConEnv,
    reprEnv = reprEnv
  } } ->
    let env2str: Map String Name -> String = lam env.
      strJoin "\n" (
        map
        (lam x. join [x.0, ": \n\t", nameGetStr x.1])
        (mapToSeq env)
      )
    in
    (
      env,
      [
        LsHover {
          location = info,
          toString = lam. Some (join [
            mcoreCode (join [nameGetStr ident, ": ", type2str ty]),
            "\n\n varEnv",
            env2str varEnv,
            "\n\n conEnv",
            env2str conEnv,
            "\n\n tyVarEnv",
            env2str tyVarEnv,
            "\n\n tyConEnv",
            env2str tyConEnv,
            "\n\n reprEnv",
            env2str reprEnv,
            "\n\n",
            getSym ident
          ])
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
      join [
        -- [
        --   LsHover {
        --     location = info,
        --     toString = lam. Some (join ["`record`"])
        --   }
        -- ],
        -- map (
        --   lam binding. match binding with (sid, expr) in
        --     let name = sidToString sid in
        --     let location = infoTm expr in
        --     LsHover {
        --       location = location,
        --       toString = lam. Some (join [
        --         mcoreCode (join ["{ ", name, " }"])
        --       ])
        --     }
        -- ) (mapToSeq bindings)
      ]
    )
  | TmRecLets { bindings=bindings } ->
    (
      env,
      map (lam binding.
        LsDefinition {
          documentation=lam. None (),
          kind = SymbolFunction (),
          location = Some binding.info,
          name = binding.ident,
          exported = false
        }
      ) bindings
    )
  | TmLet { ident=ident, tyAnnot=tyAnnot, tyBody=tyBody, info=info } ->
    let documentation = join [
      mcoreCode (join ["let ", nameGetStr ident, ": ", type2str (getAnyType [tyAnnot, tyBody])]),
      getSym ident
    ] in
    (
      env,
      [
        LsHover {
          location = info,
          toString = lam. Some documentation
        },
        LsDefinition {
          documentation=lam. Some documentation,
          exported = false,
          kind = SymbolVariable (),
          location = Some info,
          name = ident
        }
      ]
    )
  | TmLam { ident=ident, ty=ty, info=info, body=body, tyParam=tyParam } ->
    (
      env,
      [
        LsHover {
          location = info,
          toString = lam. Some (join [
            mcoreCode (join ["lam ", nameGetStr ident, ": ", type2str ty]),
            getSym ident
          ])
        },
        LsDefinition {
          documentation=lam. None (),
          exported = false,
          kind = SymbolTypeParameter (),
          location = Some info,
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
          toString = lam. Some (join [
            mcoreCode (join ["type ", nameGetStr ident, " = ", type2str ty]),
            getSym ident
          ])
        },
        LsDefinition {
          documentation=lam. None (),
          exported = false,
          kind = SymbolTypeParameter (),
          location = Some info,
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
          toString = lam. Some (join [
            mcoreCode (join ["let ", nameGetStr ident, ": ", type2str ty]),
            getSym ident
          ])
        },
        LsUsage {
          location = info,
          name = ident
        }
      ]
    )
  | TmConDef { ident=ident, tyIdent=ty, info=info } ->
    (
      env,
      [
        let super = getTypeNamesRecursively ty in
        -- eprintln (join ["Supers: ", strJoin ", " (map nameGetStr super)]);
        LsType {
          location = info,
          ident = ident,
          superIdents = super
        },
        LsDefinition {
          documentation=lam. None (),
          exported = false,
          kind = SymbolConstructor (),
          location = Some info,
          name = ident
        },
        LsHover {
          location = info,
          toString = lam. Some (join [
            mcoreCode (join ["con ", nameGetStr ident, ": ", type2str ty]),
            getSym ident
          ])
        }
      ]
    )
  | TmConApp { ident=ident, ty=ty, info=info }
  | TmExt { ident=ident, ty=ty, info=info } ->
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
          toString = lam. Some (join [
            mcoreCode (join ["type ", nameGetStr ident, ": ", type2str ty]),
            getSym ident
          ])
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
        documentation=lam. None (),
        kind = SymbolConstructor (),
        location = Some info,
        name = ident,
        exported = false
      },
      LsHover {
        location = info,
        toString = lam. Some (join [mcoreCode (nameGetStr ident), getSym ident])
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
        toString = lam. Some (join [mcoreCode (nameGetStr ident), getSym ident])
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
        toString = lam. Some (join ["`", path, "`"])
      }
    ]
  | DeclType { ident=ident, info=info, tyIdent=tyIdent }
  | DeclConDef { ident=ident, info=info, tyIdent=tyIdent }
  | DeclExt { ident=ident, info=info, tyIdent=tyIdent } ->
    join [
      [
        LsHover {
          location = info,
          toString = lam. Some (join [
            mcoreCode (join ["type ", nameGetStr ident, ": ", type2str tyIdent]),
            getSym ident
          ])
        },
        LsDefinition {
          documentation=lam. None (),
          kind = SymbolTypeParameter (),
          location = Some info,
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
          toString = lam. Some (join [
            mcoreCode (nameGetStr ident),
            getSym ident
          ])
        },
        LsDefinition {
          documentation=lam. None (),
          kind = SymbolTypeParameter (),
          location = Some info,
          name = ident
        }
      ],
      flatMap (lam def.
        [
          LsDefinition {
            documentation=lam. None (),
            kind = SymbolEnumMember (),
            location = Some info,
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
          toString = lam. Some (join [
            mcoreCode (join ["recursive let ", nameGetStr binding.ident, ": ", type2str binding.tyAnnot]),
            getSym binding.ident
          ])
        },
        LsDefinition {
          documentation=lam. None (),
          kind = SymbolVariable (),
          location = Some (infoWithFilename filename info),
          name = binding.ident
        }
      ]
    ) bindings
  | DeclLang { ident=ident, includes=includes, info=info } ->
    [
      LsHover {
        location = info,
        toString = lam. Some (join [
          mcoreCode (join ["lang ", nameGetStr ident]),
          getSym ident
        ])
      },
      LsDefinition {
        documentation=lam. None (),
        kind = SymbolModule (),
        location = Some info,
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
          toString = lam. Some (join [
            mcoreCode (nameGetStr ident),
            getSym ident
          ])
        },
        LsDefinition {
          documentation=lam. None (),
          kind = SymbolFunction (),
          location = Some info,
          name = ident
        }
      ],
      map (lam arg. 
        LsDefinition {
          documentation=lam. None (),
          kind = SymbolVariable (),
          location = Some info,
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
    let res = switch (file.status, file.symbolized, file.typeChecked)
      case (Changed (), _, _) then
        None ()
      case (TypeChecked (), Some symbolized, Some typeChecked) then
        Some (symbolized.symEnv, symbolized.program, typeChecked.expr)
      case (Symbolized (), Some symbolized, _) then
        Some (symbolized.symEnv, symbolized.program, None ())
      case (_, Some symbolized, _) then
        Some (symEnvEmpty, symbolized.program, None ())
      case _ then
        error "Unhandeled case in programToLanguageSupport"
    end in

    let languageSupport = optionMap (lam res. match res with (symEnv, program, expr) in join [
      optionGetOr [] (optionMap (lam expr. recursiveExprLookup file symEnv emptyLSEnv expr) expr)
    ]) res in

    optionGetOr [] languageSupport

  sem linksToLanguageSupport: MLangFile -> [LanguageServerPayload]
  sem linksToLanguageSupport =
  | _ -> []
  | file & { status = TypeChecked () | Symbolized () | Linked (), linked=Some linked } ->
    flatMap (
      lam link.
        match link with (info, path) in
        let fileName = nameSym path in
        [
          LsHover {
            location = info,
            toString = lam. Some (join ["[", path, "](", path, ")"])
          },
          LsDefinition {
            documentation=lam. None (),
            exported = false,
            kind = SymbolFile (),
            location = Some (makeInfo (posVal path 1 0) (posVal path 1 0)),
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
    
    let diagnostics = map (
      lam diagnostic.
        LsDiagnostic diagnostic
    ) (getFileDiagnostics file) in

    join [
      programLanguageSupport,
      linksLanguageSupport,
      diagnostics
    ]
end