lang MLangLookupIncludeLookup = MLangCompilation
  sem includesLookup: MLangFile -> [(Info, LookupResult)]
  sem includesLookup =
  | { kind = Parsed { includes = includes } } ->
    let f: (Info, Include) -> LookupResult = lam infoInclude.
      match infoInclude with (info, inc) in
      let lookupDefinition = match inc
        with ExistingFile path then Some (lam. makeInfo {filename=path, row=1, col=1} {filename=path, row=1, col=1})
        else None ()
      in
      {
        info = info,
        pprint = lam. use MLangCompilationKind in inc2str inc,
        lookupDefinition = lookupDefinition
      }
    in
    map (lam v. (v.0, f v)) includes
  | _ -> []
end