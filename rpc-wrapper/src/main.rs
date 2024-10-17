mod request;
mod response;

use std::io::{self, Write};

use request::{get_raw_request, get_request, Params, Request};
use response::{NotificationMessage, RPCResponseResult, ResponseMessage};

fn respond_raw(content: String) {
    eprint!("Content-Length: {}\r\n\r\n{}", content.len(), content);
    print!("Content-Length: {}\r\n\r\n{}", content.len(), content);
    io::stdout().flush().unwrap();
}

fn notify(result: NotificationMessage) {
    let response = serde_json::to_string(&result).unwrap();
    respond_raw(response);
}

fn respond(request: Request, result: RPCResponseResult) {
    let response = ResponseMessage {
        jsonrpc: "2.0".to_string(),
        id: request.id.unwrap(),
        result,
    };

    let response = serde_json::to_string(&response).unwrap();
    respond_raw(response);
}

fn rust_server() -> io::Result<()> {
    loop {
        let request = match get_request() {
            Ok(request) => request,
            Err(_) => {
                eprintln!("Error: {:?}", "Failed to parse request");
                continue;
            }
        };

        eprintln!("{:?}", request);

        let diagnostics = serde_json::json!([
            {
                "range": {
                    "start": {
                        "line": 2,
                        "character": 0,
                    },
                    "end": {
                        "line": 2,
                        "character": 15,
                    },
                },
                "severity": 1,
                "source": "diagnostics-server",
                "message": "This is an error",
            },
        ]);

        match &request.params {
            Params::Initialize(initialize) => {
                eprintln!("Initialize: {:?}", initialize);

                let result = response::RPCResponseResult(Ok(
                    response::ResponseResult::InitializeResponse(response::InitializeResult {
                        capabilities: serde_json::json!({
                            "hoverProvider": true,
                            "textDocumentSync": 1,
                            "diagnosticProvider": {
                                "interFileDependencies": false,
                                "workspaceDiagnostics": false,
                            },
                        }),
                        server_info: Some(response::ServerInfo {
                            name: "rpc-wrapper".to_string(),
                            version: Some("0.1.0".to_string()),
                        }),
                    }),
                ));

                respond(request, result);
            }
            Params::Hover(hover) => {
                eprintln!("Hover: {:?}", hover);

                let result = response::RPCResponseResult(Ok(
                    response::ResponseResult::HoverResponse(response::HoverResponse {
                        contents: "This is information!".to_string(),
                    }),
                ));

                respond(request, result);
            }
            Params::DocumentDiagnostics(document_diagnostics) => {
                eprintln!("DocumentDiagnostics: {:?}", document_diagnostics);

                let result = response::RPCResponseResult(Ok(response::ResponseResult::Json(
                    serde_json::json!({
                        "kind": "full",
                        "items": diagnostics
                    }),
                )));

                respond(request, result);
            }
            Params::DidChange(did_change) => {
                eprintln!("DidChange: {:?}", did_change);

                let result = NotificationMessage {
                    jsonrpc: "2.0".to_string(),
                    method: "textDocument/publishDiagnostics".to_string(),
                    params: serde_json::json!({
                        "uri": did_change.text_document.uri,
                        "version": did_change.text_document.version,
                        "diagnostics": diagnostics
                    }),
                };

                notify(result);
            }
            Params::Error(error) => {
                eprintln!("Error: {:?}", error)
            }
        }
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() <= 1 {
        eprintln!("Starting rust LSP server");
        rust_server()?;

        return Ok(());
    }

    let flags = args
        .iter()
        .skip(1)
        .map(|s| s.as_str())
        .collect::<Vec<&str>>();

    let quit_after_one_request = flags.contains(&"--quit-after-one-request");

    if flags.contains(&"--stdin") {
        // eprintln!("Starting rpc-wrapper with stdin");

        loop {
            let content = match get_raw_request() {
                Ok(request) => request,
                Err(_) => {
                    eprintln!("Error: {:?}", "Failed to parse request");
                    continue;
                }
            };

            println!("{content}");

            if quit_after_one_request {
                return Ok(());
            }
        }
    } else if flags.contains(&"--stdout"){
        // eprintln!("Starting rpc-wrapper with stdout");

        loop {
            let mut content = String::new();
            match std::io::stdin().read_line(&mut content) {
                Ok(v) => {
                    if v == 0 {
                        return Ok(());
                    }
                }
                Err(_) => {
                    eprintln!("Error: {:?}", "Failed to read from stdin");

                    return Ok(());
                }
            }

            respond_raw(content);

            if quit_after_one_request {
                return Ok(());
            }
        }
    } else {
        eprintln!("Error: Invalid argument, use either --stdin or --stdout");

        return Ok(());
    }
}
