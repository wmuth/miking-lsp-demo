include "json.mc"
include "ext/file-ext.mc"

-- returns Option content if the requested number of bytes could be read
-- otherwise, None is returned
recursive
  let readBytesBuffered : ReadChannel -> Int -> Option [Int] =
    lam rc. lam len. switch fileReadBytes rc len
      case Some s then (
        let actual_length = length s in
        if eqi actual_length len then Some s
        else match readBytesBuffered rc (subi len actual_length)
          with Some s2 then Some (join [s, s2])
          else None ()
      )
      case None () then None ()
    end
  end

let eprint: String -> () = lam s.
  fileWriteString fileStderr s;
  flushStderr()
  
let eprintln: String -> () = lam s.
  fileWriteString fileStderr (join [s, "\n"]);
  flushStderr()

let print: String -> () = lam s.
  fileWriteString fileStdout s;
  flushStdout()

let println: String -> () = lam s.
  fileWriteString fileStdout (join [s, "\n"]);
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