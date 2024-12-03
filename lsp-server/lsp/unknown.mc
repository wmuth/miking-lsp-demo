include "./root.mc"

lang LSPUnknownMethod = LSPRoot
  syn Message =
  | UnknownMethod {
    method: String
  }

  sem getMessage request =
  | method ->
    UnknownMethod {
      method = method
    }

  sem execute context =
  | UnknownMethod { method = method } ->
    eprintln (join ["[Unknown method] ", method]);
    {
      response = None (),
      environment = context.environment
    }

end