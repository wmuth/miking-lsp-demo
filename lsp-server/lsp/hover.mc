include "json.mc"
include "mexpr/pprint.mc"

include "../../lib/utils.mc"
include "./utils.mc"
include "./root.mc"

lang LSPHover = LSPRoot
  syn Message =
  | Hover {
    id: Int,
    textDocument: TextDocumentPositionParams
  }

  sem getMessage request =
  | "textDocument/hover" ->
    match request.id with Some id in
    Hover {
      id = id,
      textDocument = getTextDocumentPositionParams request.params
    }

  type HoverInformation = {
    location: Info,
    toString: [() -> Option String]
  }

  -- Todo: create middleware which bakes the linear information into a searchable tree
  sem findHoverLinearly: URI -> LanguageServerContext -> Int -> Int -> Option HoverInformation
  sem findHoverLinearly uri context line =| character ->
    let hovers = mapToSeq context.hover in

    let foundHovers = filterMap (
      lam hover.
        match hover with (info, toString) in
        if infoCollision info uri line character
          then Some { location = info, toString = toString }
          else None ()
    ) hovers in

    match foundHovers with [first] ++ rest then
      let f = lam hover1. lam hover2.
        if infoContainsInfo hover1.location hover2.location then hover1 else hover2
      in

      Some (foldl f first rest)
    else
      None()

  sem _LSPHover_findDefinitions: Map Name [LSPDefinition] -> UsageInformation -> [LSPDefinition]
  sem _LSPHover_findDefinitions definitions =| usage ->
    let locations = filterMap (lam name. mapLookup name definitions) usage.names in
    flatMap identity locations

  sem execute context =
    | Hover { id = id, textDocument = {
      uri = uri,
      line = line,
      character = character
    } } ->
      -- Add 1 to incoming line and character to match the 1-based indexing of LSP
      let line = addi line 1 in
      let uri = stripUriProtocol uri in
      
      match mapLookup uri context.environment.files with Some file then
        -- Get hover information
        let hoverResult: Option HoverInformation = findHoverLinearly uri file line character in

        -- Combine hover information with definition hover information, if enabled
        let hoverResult: Option HoverInformation = match context.parameters.options.hoverShowDefinitionPrefix with
          Some prefix then
            -- Get definition hover information
            let files = mapValues context.environment.files in
            let definitions = foldl (
              lam acc. lam file.
                mapUnionWith concat acc file.definitions
            ) (mapEmpty nameSymCmp) files in
            let usageResult = findUsageLinearly uri file.usages line character in
            let definitions: Option [() -> Option String] = optionMap (
              lam usageResult.
                let definitions = _LSPHover_findDefinitions definitions usageResult in
                map (lam definition. definition.documentation) definitions
            ) usageResult in
            let definitions = optionGetOr [] definitions in
            let definitions = map (lam documentation. lam. optionMap prefix (documentation ())) definitions in

            optionMap (
              lam hoverResult. {
                location = hoverResult.location,
                toString = join [
                  definitions,
                  hoverResult.toString
                ]
              }
            ) hoverResult
          else
            hoverResult
        in

        let res = optionMap (
          lam hoverResult.
            let contentToJsonString = lam content. JsonString content in
            let info = getFileInfo hoverResult.location in
            let contents = filterMap (lam toString. toString ()) hoverResult.toString in
            let contents = if context.parameters.options.filterHoverDuplicates then
              (compose setToSeq (setOfSeq cmpString)) contents
            else
              contents
            in
            let contents = JsonArray (map contentToJsonString contents) in

            jsonKeyObject [
              ("contents", contents),
              ("range", jsonKeyObject [
                ("start", jsonKeyObject [
                  ("line", JsonInt (subi info.lineStart 1)),
                  ("character", JsonInt info.colStart)
                ]),
                ("end", jsonKeyObject [
                  ("line", JsonInt (subi info.lineEnd 1)),
                  ("character", JsonInt info.colEnd)
                ])
              ])
            ]
        ) hoverResult in

        let response = optionGetOrElse (lam. JsonNull ()) res in

        {
          environment = context.environment,
          response = Some(jsonKeyObject [
            ("jsonrpc", JsonString "2.0"),
            ("id", JsonInt id),
            ("result", response)
          ])
        }
      else
        {
          environment = context.environment,
          response = Some(jsonKeyObject [
            ("jsonrpc", JsonString "2.0"),
            ("id", JsonInt id),
            ("result", JsonNull ())
          ])
        }
end