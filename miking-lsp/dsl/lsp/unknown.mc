include "./root.mc"

lang LSPUnknownMethod = LSPRoot
	syn Params =
	| UnknownMethod {}

	sem getParams request =
	| _method ->
		UnknownMethod {}

	sem execute context =
	| UnknownMethod {} -> None ()
end