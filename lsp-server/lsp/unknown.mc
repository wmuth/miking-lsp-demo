include "./root.mc"

lang LSPUnknownMethod = LSPRoot
	syn Message =
	| UnknownMethod {}

	sem getMessage request =
	| _method ->
		UnknownMethod {}

	sem execute context =
	| UnknownMethod {} -> {
		response = None (),
		environment = context.environment
	}
end