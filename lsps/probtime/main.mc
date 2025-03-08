include "mexpr/mexpr.mc"

include "../../lsp-server/lsp/root.mc"

let debugSym = false

let mcoreCode = lam code. join ["```probtime\n", code, "\n```"]

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

lang MExprLanguageServerCompiler = MExpr + MExprAst + MExprPrettyPrint + LSPRoot
  type LSCompileResult = [LanguageServerPayload]

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
  | TyAlias { display=TyCon { ident=ident, info=info }, content=typ }
  | TyCon { ident=ident, info=info, data=typ }
  | typ & TyVar { ident=ident, info=info }
  | typ & TyAll { ident=ident, info=info } ->
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

  sem exprLookup: String -> Expr -> LSCompileResult
  sem exprLookup filename =
  | _ -> []
  | TmRecord { bindings=bindings, info=info } ->
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
  | TmRecLets { bindings=bindings } ->
    map (lam binding.
      LsDefinition {
        documentation=lam. None (),
        kind = SymbolFunction (),
        location = binding.info,
        name = binding.ident
      }
    ) bindings
  | TmLet { ident=ident, tyAnnot=tyAnnot, tyBody=tyBody, info=info } ->
    let documentation = join [
      mcoreCode (join ["let ", nameGetStr ident, ": ", type2str (getAnyType [tyAnnot, tyBody])]),
      getSym ident
    ] in
    [
      LsHover {
        location = info,
        toString = lam. Some documentation
      },
      LsDefinition {
        documentation=lam. Some documentation,
        kind = SymbolVariable (),
        location = info,
        name = ident
      }
    ]
  | TmLam { ident=ident, ty=ty, info=info, body=body, tyParam=tyParam } ->
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
        kind = SymbolTypeParameter (),
        location = info,
        name = ident
      }
    ]
  | TmType { ident=ident, ty=ty, info=info, inexpr=inexpr } ->
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
        kind = SymbolTypeParameter (),
        location = info,
        name = ident
      }
    ]
  | TmVar { ident=ident, ty=ty, info=info } ->
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
  | TmConDef { ident=ident, tyIdent=ty, info=info } ->
    [
      let super = getTypeNamesRecursively ty in
      LsType {
        location = info,
        ident = ident,
        superIdents = super
      },
      LsDefinition {
        documentation=lam. None (),
        kind = SymbolConstructor (),
        location = info,
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
  | TmConApp { ident=ident, ty=ty, info=info }
  | TmExt { ident=ident, ty=ty, info=info } ->
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

  sem patLookup: String -> Pat -> [LanguageServerPayload]
  sem patLookup filename =
  | _ -> []
  | PatSeqEdge { middle=PName ident, info=info }
  | PatNamed { ident=PName ident, info=info } ->
    let info = infoWithFilename filename info in
    [
      LsDefinition {
        documentation=lam. None (),
        kind = SymbolConstructor (),
        location = info,
        name = ident
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

  sem fileToLanguageSupport: String -> Expr -> [LanguageServerPayload]
  sem fileToLanguageSupport filename =| expr ->
    recursiveExprLookup filename expr
end