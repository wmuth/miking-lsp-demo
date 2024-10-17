include "json.mc"
include "ext/file-ext.mc"

let eprint: String -> () = lam s.
  writeString stderr s;
  flushStderr()
  
let eprintln: String -> () = lam s.
  writeString stderr (join [s, "\n"]);
  flushStderr()

let print: String -> () = lam s.
  writeString stdout s;
  flushStdout()

let println: String -> () = lam s.
  writeString stdout (join [s, "\n"]);
  flushStdout()

let rpcprint = lam s.
  let len = addi 1 (length s) in
  println (join ["Content-Length: ", int2string len, "\r\n\r\n", s]);
  -- eprintln (join ["Content-Length: ", int2string len, "\r\n\r\n", s]);
  ()

let jsonKeyObject: [(String, JsonValue)] -> JsonValue = lam content.
  JsonObject (
    mapFromSeq cmpString content
  )