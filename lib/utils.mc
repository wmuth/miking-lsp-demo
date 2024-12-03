include "json.mc"
include "ext/file-ext.mc"

-- returns Option content if the requested number of bytes could be read
-- otherwise, None is returned
recursive
  let readBytesBuffered : ReadChannel -> Int -> Option [Int] =
    lam rc. lam len. switch fileReadBytes rc len
      case Some s then (
        let actualLength = length s in
        if eqi actualLength len then Some s
        else match readBytesBuffered rc (subi len actualLength)
          with Some s2 then Some (join [s, s2])
          else None ()
      )
      case None () then None ()
    end
  end

let groupBy: all k. all v. (k -> k -> Int) -> (v -> k) -> [v] -> Map k [v] =
  lam cmp. lam f. lam arr.
    foldl
      (lam m. lam v. mapInsertWith concat (f v) [v] m)
      (mapEmpty cmp)
      arr

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