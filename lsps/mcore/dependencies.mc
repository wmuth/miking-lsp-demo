recursive let getDirtyDependencies: Map Path (Set Path) -> Set Path -> Path -> Set Path =
	lam dependencies. lam dirty. lam path.
	  if setMem path dirty then
      dirty
	  else
      let dirty = setInsert path dirty in
      match mapLookup path dependencies with Some children then
        foldl (getDirtyDependencies dependencies) dirty (setToSeq children)
      else
        dirty
end