lang MLangUtestLenses = MLangAst + MExprAst + MLangFileHandler
  sem createUtestLens: Info -> Option CodeLens
  sem createUtestLens = 
  | info & Info r ->
    Some {
      title = "Run Test",
      ideCommand = "mcore.debugSingle",
      arguments = [
        JsonString r.filename,
        JsonString (info2str info)
      ],
      data = jsonKeyObject [
        ("customData", JsonString "A data entry field that is preserved on a code lens item between a code lens and a code lens resolve request.")
      ],
      location = info
    }
  | _ -> None ()

  sem getUtestLenses: MLangFile -> [CodeLens]
  sem getUtestLenses =
  | file ->
    let parsed = getParsed file.kind in

    let res = optionMap (
      lam parsed.
        join [
          filterOption (map getDeclLens parsed.decls),
          getExprLens parsed.expr
        ]
    ) parsed in

    optionGetOr [] res

  sem getDeclLens: Decl -> Option CodeLens
  sem getDeclLens =
  | DeclUtest { info = info } -> createUtestLens info
  | _ -> None ()

  sem getExprLens: Expr -> [CodeLens]
  sem getExprLens =
  | expr ->
      let arr = switch expr
        case TmUtest { info = info } then
          optionGetOr [] (optionMap (lam v. [v]) (createUtestLens info))
        case _ then
          []
      end in
  
      sfold_Expr_Expr (lam acc. lam e.
        let children = getExprLens e in
        concat acc children
      ) arr expr
end