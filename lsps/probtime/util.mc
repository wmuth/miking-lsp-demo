include "mexpr/mexpr.mc"

let debugSym = true

lang ProbTimeLanguageServerPrettyPrint = MExprPrettyPrint + MExprAst
  sem probtimeCode =| code -> join ["```probtime\n", code, "\n```\n"]

  sem probtimeDefinition types prefix =| name ->
    probtimeCode ((compose join filterOption) [
      Some (join [prefix, " "]),
      Some (nameGetStr name),
      optionMap (
        lam ty. join [": ", type2str ty]
      ) (mapLookup name types)
    ])
end

-- Get first non-unknown type from list
recursive let concreteType: use MExprAst in [Type] -> Type =
  lam types.
  use MExprAst in
  switch types
    case [] then
      TyUnknown { info = NoInfo () }
    case [t] ++ _ then
      t
    case [TyUnknown _] ++ rest then
      concreteType rest
  end
end

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
  if debugSym then lam name.
    join [
      "\n", optionGetOr
        "No symbol"
        (optionMap (compose int2string sym2hash) (nameGetSym name))
    ]
  else lam. ""

-- Basically do this, but with less boilerplate:
-- let childTypes = sfold_Decl_Type (lam acc. lam ty.
--   join [acc, recursiveTypeLookup file env ty]
-- ) [] decl
let createAccumulator: all a. all b. all acc. (([acc] -> a -> [acc]) -> [acc] -> b -> [acc]) -> (a -> [acc]) -> b -> [acc] =
  lam sfold. lam generator. lam item.
    sfold (lam acc. lam pat.
      join [acc, generator pat]
    ) [] item

let fapply = lam x. lam f. f x
let createAccumulators =
  lam accumulators. lam item.
    join (map (fapply item) accumulators)