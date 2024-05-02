use serde::{Deserialize, Deserializer, Serialize};
use serde_json::{from_value, Value};
use std::io::{self, prelude::*};

macro_rules! deserialize_params {
    ($method:expr, $raw:expr, $( $method_name:expr => $variant:ident ),* $(,)?) => {
        match $method {
            $(
                $method_name => {
                    let params = serde_json::from_value($raw.params)
                        .map_err(serde::de::Error::custom)?;
                    Ok(Params::$variant(params))
                }
            )*
            _ => Err(serde::de::Error::custom("unsupported method")),
        }
    };
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Initialize {
    process_id: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Error {
    code: i32,
    message: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TextDocumentIdentifier {
    pub uri: String,
    pub version: Option<usize>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Position {
    line: u32,
    character: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Hover {
    pub text_document: TextDocumentIdentifier,
    pub position: Position,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContentChange {
    pub text: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DidChange {
    pub text_document: TextDocumentIdentifier,
    pub content_changes: Vec<ContentChange>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DocumentDiagnostics {
    pub text_document: TextDocumentIdentifier,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Params {
    Initialize(Initialize),
    Error(Error),
    Hover(Hover),
    DocumentDiagnostics(DocumentDiagnostics),
    DidChange(DidChange),
}

#[derive(Debug, Clone, Serialize)]
pub struct Request {
    pub jsonrpc: String,
    pub id: Option<usize>,
    pub method: Option<String>,
    pub params: Params,
}

#[derive(Deserialize)]
struct RawRequest {
    jsonrpc: String,
    id: Option<usize>,
    method: Option<String>,
    params: Value,
}

impl<'de> Deserialize<'de> for Request {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = RawRequest::deserialize(deserializer)?;

        let Some(ref method) = raw.method else {
            let init_params = from_value(raw.params).map_err(serde::de::Error::custom)?;

            return Ok(Request {
                jsonrpc: raw.jsonrpc,
                id: raw.id,
                method: None,
                params: Params::Error(init_params),
            });
        };

        let params = deserialize_params!(method.as_str(), raw,
            "initialize" => Initialize,
            "textDocument/hover" => Hover,
            "textDocument/didChange" => DidChange,
            "textDocument/diagnostic" => DocumentDiagnostics,
        )?;

        Ok(Request {
            jsonrpc: raw.jsonrpc,
            id: raw.id,
            method: raw.method,
            params,
        })
    }
}

pub enum RequestError {
    Serde(serde_json::Error),
    Io(io::Error),
}

impl From<serde_json::Error> for RequestError {
    fn from(error: serde_json::Error) -> Self {
        RequestError::Serde(error)
    }
}

impl From<io::Error> for RequestError {
    fn from(error: io::Error) -> Self {
        RequestError::Io(error)
    }
}

pub fn get_raw_request() -> Result<String, io::Error> {
    let mut content_length = String::new();
    std::io::stdin().read_line(&mut content_length)?;

    let content_length: usize = content_length.split(": ").collect::<Vec<&str>>()[1]
        .trim()
        .parse()
        .unwrap();

    let mut empty_line = String::new();
    std::io::stdin().read_line(&mut empty_line)?;

    let mut buffer = vec![0; content_length];
    std::io::stdin().read_exact(&mut buffer)?;

    let content = String::from_utf8(buffer).unwrap();

    eprintln!("[Content]: {}", content);

    Ok(content)
}

pub fn get_request() -> Result<Request, RequestError> {
    let content = get_raw_request()?;
    let request: Request = serde_json::from_str(&content)?;

    Ok(request)
}
