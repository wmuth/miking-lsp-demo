

-- type DefinitionInformation
-- con DefinitionInformation : {
--   start: Int,
--   definitions: Map String [(Int, Info)],
--   stop: Int
-- } -> DefinitionInformation

lang MExprLSP = MExprAst
  sem buildDefinitionTree: Expr -> Map String Info
  -- sem buildDefinitionTree =
  -- | _ -> mapEmpty cmpString

  sem buildDefinitionTree =
  | TmConst {} -> mapEmpty cmpString
  | TmVar {} -> mapEmpty cmpString
  | TmApp {
    lhs = lhs,
    rhs = rhs
  } -> 
    let lhs = buildDefinitionTree lhs in
    let rhs = buildDefinitionTree rhs in
    mapUnion lhs rhs
  | TmLet {
    ident = name,
    tyAnnot = _,
    tyBody = _,
    body = _,
    inexpr = inexpr,
    ty = _,
    info = info
  } -> 
    let childDefinitionTree = buildDefinitionTree inexpr in
    let definitionTree = mapFromSeq cmpString [
      (name.0, info)
    ] in
    mapUnion definitionTree childDefinitionTree
end