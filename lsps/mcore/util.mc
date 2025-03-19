include "mlang/ast.mc"

let ssMapToString = lam m.
  let f = lam acc. lam key. lam val. join [acc, key, " -> ", val, "\n"] in
  mapFoldWithKey f "" m
  
-- Set the filename of the info for an error
let errorWithFilename: String -> Diagnostic -> Diagnostic =
  lam filename. lam err.
    match err with (info, msg) in
    (infoWithFilename filename info, msg)

lang MLangAndMExpr = MLangAst + MExprAst
end

let flattenErrors: all w. all e. all a. [Result w e a] -> Result w e [a] =
  use MLangAst in
  lam results.
    foldl (lam acc. lam val. result.map2 (lam a1. lam a2. join [a1, [a2]]) acc val) (result.ok []) results

recursive let populateMLangExprInfoWithFilename: use MLangAndMExpr in String -> Expr -> Expr =
  use MLangAndMExpr in
  lam filename. lam expr.
    let expr = withInfo (infoWithFilename filename (infoTm expr)) expr in
    smap_Expr_Expr (populateMLangExprInfoWithFilename filename) expr
end

recursive let populateMLangDeclInfoWithFilename: use MLangAst in String -> Decl -> Decl =
  use MLangAst in
  lam filename. lam decl.
    let decl = declWithInfo (infoWithFilename filename (infoDecl decl)) decl in 
    let decl = smap_Decl_Decl (populateMLangDeclInfoWithFilename filename) decl in
    smap_Decl_Expr (populateMLangExprInfoWithFilename filename) decl
end

let populateMLangProgramInfoWithFilename: use MLangAst in String -> MLangProgram -> MLangProgram =
  lam filename. lam program.
    let decls = map (populateMLangDeclInfoWithFilename filename) program.decls in
    let expr = populateMLangExprInfoWithFilename filename program.expr in
    {
      program with
      decls = decls,
      expr = expr
    }

let filterMap: all a. all b. (a -> Option b) -> [a] -> [b] =
  lam f. lam xs.
    filterOption (map f xs)

let flattenErrors: all w. all e. all a. [Result w e a] -> Result w e [a] =
  use MLangAst in
  lam results.
    foldl (lam acc. lam val. result.map2 (lam a1. lam a2. join [a1, [a2]]) acc val) (result.ok []) results

recursive let populateMLangExprInfoWithFilename: use MLangAndMExpr in String -> Expr -> Expr =
  use MLangAndMExpr in
  lam filename. lam expr.
    let expr = withInfo (infoWithFilename filename (infoTm expr)) expr in
    smap_Expr_Expr (populateMLangExprInfoWithFilename filename) expr
end

recursive let populateMLangDeclInfoWithFilename: use MLangAst in String -> Decl -> Decl =
  use MLangAst in
  lam filename. lam decl.
    let decl = declWithInfo (infoWithFilename filename (infoDecl decl)) decl in 
    let decl = smap_Decl_Decl (populateMLangDeclInfoWithFilename filename) decl in
    smap_Decl_Expr (populateMLangExprInfoWithFilename filename) decl
end

let populateMLangProgramInfoWithFilename: use MLangAst in String -> MLangProgram -> MLangProgram =
  lam filename. lam program.
    let decls = map (populateMLangDeclInfoWithFilename filename) program.decls in
    let expr = populateMLangExprInfoWithFilename filename program.expr in
    {
      program with
      decls = decls,
      expr = expr
    }

let debugSym = false

let mcoreCode = lam code. join ["```mcore\n", code, "\n```"]

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

let fapply = lam x. lam f. f x
let createAccumulators =
  lam accumulators. lam item.
    join (map (fapply item) accumulators)

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