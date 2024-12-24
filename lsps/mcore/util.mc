type Diagnostic = (Info, String)

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
    let f = lam x.
      match f x with Some y then [y] else []
    in
    foldl (lam acc. lam x. join [acc, x]) [] (map f xs)

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