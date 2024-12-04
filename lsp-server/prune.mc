include "./lsp/lsp.mc"

type MessagePruningEnvironment = {
  cancelled: Map Int Bool,
  overwrittenDidChange: Map String Bool
}

-- TODO: request request pruning, e.g. removing concurrent didChange notifications
recursive let pruneMessages: MessagePruningEnvironment -> use LSP in [MessageWithContext] -> [MessageWithContext] =
  lam environment. lam messages.
    match messages with [message] ++ messages then
      use LSP in
      switch (message.id, message.message)
        case (_, CancelRequest { id = id }) then
          -- When cancelling a request, we remove the message with the same id
          -- The `CancelRequest` will during execution respond with a
          -- `ErrorCodes.RequestCancelled=32800` error, satisfying the LSP JSON-RPC spec.
          let environment = {
            environment with
            cancelled = mapInsert id true environment.cancelled
          } in
          concat [message] (pruneMessages environment messages)
        case (Some id, _) then
          let messages = pruneMessages environment messages in
          match mapLookup id environment.cancelled with Some _ then
            messages
          else
            concat [message] messages
        case (_, DidChange { uri = uri }) then
          -- When a `DidChange` notification is received, we check if we have already received one for the same URI.
          -- If we have, we don't include this one, as it has been overwritten (we are in a reversed message array).
          -- If we haven't, we include it and mark the URI as having received a `DidChange` notification.
          match mapLookup uri environment.overwrittenDidChange with Some _ then
            pruneMessages environment messages
          else
            let environment = {
              environment with
              overwrittenDidChange = mapInsert uri true environment.overwrittenDidChange
            } in
            concat [message] (pruneMessages environment messages)
        case (_, _) then
          concat [message] (pruneMessages environment messages)
      end
    else
      messages
end

let pruneMessages: use LSP in [MessageWithContext] -> [MessageWithContext] =
  lam messages.
    let messages = reverse messages in
    let environment: MessagePruningEnvironment = {
      cancelled = mapEmpty subi,
      overwrittenDidChange = mapEmpty cmpString
    } in
    reverse (pruneMessages environment messages)