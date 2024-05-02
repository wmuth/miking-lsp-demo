use serde::{ser::SerializeMap, Serialize, Serializer};

#[derive(Debug, Clone, Serialize)]
pub struct ServerInfo {
    pub name: String,
    pub version: Option<String>,
}

// #[derive(Debug, Clone, Serialize)]
// #[serde(rename_all = "camelCase")]
// pub struct Capabilities {
//     pub hover_provider: Option<bool>,
//     pub text_document_sync: Option<TextDocumentSync>,
//     // pub diagnostic_provider: Option<DiagnosticProvider>,
// }

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InitializeResult {
    pub capabilities: serde_json::Value,
    pub server_info: Option<ServerInfo>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HoverResponse {
    pub contents: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum ResponseResult {
    InitializeResponse(InitializeResult),
    HoverResponse(HoverResponse),

    Json(serde_json::Value),
}

#[derive(Debug, Clone, Serialize)]
pub struct ResponseError {
    pub code: i32,
    pub message: String,
    pub data: Option<serde_json::Value>,
}

#[derive(Debug, Clone)]
pub struct RPCResponseResult(pub Result<ResponseResult, ResponseError>);

impl Serialize for RPCResponseResult {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;

        match &self.0 {
            Ok(value) => map.serialize_entry("result", value)?,
            Err(error) => map.serialize_entry("error", error)?,
        }

        map.end()
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct ResponseMessage {
    pub jsonrpc: String,
    pub id: usize,

    #[serde(flatten)]
    pub result: RPCResponseResult,
}

#[derive(Debug, Clone, Serialize)]
pub struct NotificationMessage {
    pub jsonrpc: String,
    pub method: String,
    pub params: serde_json::Value,
}