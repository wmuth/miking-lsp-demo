lang MLangLookupIncludeLookup = MLangFileHandler
  sem includesLookup: MLangFile -> [(Info, LookupResult)]
  sem includesLookup =| file ->
    let f: (Info, Include) -> LookupResult = lam infoInclude.
      match infoInclude with (info, inc) in
      let lookupDefinition = match inc
        with ExistingFile path then Some (lam. makeInfo {filename=path, row=1, col=1} {filename=path, row=1, col=1})
        else None ()
      in
      {
        info = info,
        pprint = lam. inc2str inc,
        lookupDefinition = lookupDefinition
      }
    in
    map (lam v. (v.0, f v)) (getIncludes file.kind)
end