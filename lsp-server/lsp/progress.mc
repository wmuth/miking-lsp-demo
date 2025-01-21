include "./root.mc"

lang LSPProgress = LSPRoot
  syn ProgressReport =
  | Begin {
    token: String,
    message: Option String,
    percentage: Int
  }
  | Report {
    token: String,
    message: Option String,
    percentage: Int
  }
  | End {
    token: String,
    message: Option String
  }

  sem getProgressToken : ProgressReport -> String
  sem getProgressToken =
  | Begin {token = token}
  | Report {token = token}
  | End {token = token} -> token

  type Progress = {
    report: Float -> (),
    reportMsg: Float -> String -> (),
    finish: Option String -> ()
  }

  sem createProgress : (JsonValue -> ()) -> Progress
  sem createProgress =| sendNotification ->
    let token = int2string ((compose sym2hash gensym) ()) in
    let lastReport: Ref (Option ProgressReport) = ref (None ()) in

    let sendReport: ProgressReport -> () = lam report.
      modref lastReport (Some report);
      sendNotification (getProgressNotification report)
    in

    let reportMsg = lam percentage. lam message.
      let percentage = (compose floorfi (mulf 100.0)) percentage in

      switch deref lastReport
        case None _ then
          sendNotification (getCreateProgressNotification token);
          sendReport (Begin { token=token, percentage=percentage, message=message })
        case Some (Begin _ | Report _) then
          sendReport (Report { token=token, percentage=percentage, message=message })
        case _ then ()
      end
    in

    let report = (flip reportMsg) (None ()) in
    let reportMsg = lam percentage. lam message. reportMsg percentage (Some message) in

    {
      report = report,
      reportMsg = reportMsg,
      finish = lam message.
        switch deref lastReport
          case Some (Report _ | Begin _) then
            sendReport (End { token=token, message=message })
          case _ then ()
        end
    }

  sem getProgressNotificationPayload : ProgressReport -> JsonValue
  sem getProgressNotificationPayload =
  | Begin { message = message, percentage = percentage } ->
    jsonKeyObject (filterOption [
      Some ("kind", JsonString "begin"),
      optionMap (lam message. ("message", JsonString message)) message,
      Some ("percentage", JsonInt percentage)
    ])
  | Report { message = message, percentage = percentage } ->
    jsonKeyObject (filterOption [
      Some ("kind", JsonString "report"),
      optionMap (lam message. ("message", JsonString message)) message,
      Some ("percentage", JsonInt percentage)
    ])
  | End { message = message } ->
    jsonKeyObject (filterOption [
      Some ("kind", JsonString "end"),
      optionMap (lam message. ("message", JsonString message)) message
    ])

  sem getProgressNotification : ProgressReport -> JsonValue
  sem getProgressNotification =| progress ->
    jsonKeyObject [
      ("jsonrpc", JsonString "2.0"),
      ("method", JsonString "$/progress"),
      ("params", jsonKeyObject [
        ("token", JsonString (getProgressToken progress)),
        ("value", getProgressNotificationPayload progress)
      ])
    ]

  sem getCreateProgressNotification : String -> JsonValue
  sem getCreateProgressNotification =| token ->
    jsonKeyObject [
      ("id", JsonString token), -- heuristic: use the token as the id for the create progress request
      ("jsonrpc", JsonString "2.0"),
      ("method", JsonString "window/workDoneProgress/create"),
      ("params", jsonKeyObject [
        ("token", JsonString token)
      ])
    ]
end