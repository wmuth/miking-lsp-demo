recursive let _pprintjson2string: Int -> JsonValue -> String = lam indent. lam value.
  let indAmount = 2 in
  let indStr = strJoin "" (make (muli indent indAmount) " ") in
  switch value
  case JsonObject properties then
    let indStrChildren = strJoin "" (make (muli (addi indent 1) indAmount) " ") in
    let proplist = mapFoldWithKey (lam acc. lam k. lam v.
      snoc acc (join [indStrChildren, _pprintjson2string (addi indent 1) (JsonString k), ": ", _pprintjson2string (addi indent 1) v])
    ) [] properties in
    join [
      "{\n",
      strJoin ",\n" proplist,
      "\n",
      indStr,
      "}"
    ]
  case JsonArray values then
    cons '[' (snoc (strJoin ",\n" (map (_pprintjson2string (addi indent 1)) values)) ']')
  case JsonString s then
    let escape: [Char] -> Char -> String = lam acc. lam c.
      let cval: Int = char2int c in
      if eqi cval 8 then
        concat acc "\\b"
      else if eqi cval 12 then
        concat acc "\\f"
      else if or (lti cval 32) (eqi cval 127) then
        let tohex: Int -> Char = lam x.
          if lti x 10 then
            int2char (addi x (char2int '0'))
          else
            int2char (addi (subi x 10) (char2int 'a'))
        in
        concat acc ['\\', 'u', '0', '0', tohex (divi cval 16), tohex (modi cval 16)]
      else
        switch c
        case '\"' then concat acc "\\\""
        case '\\' then concat acc "\\\\"
        case '/' then concat acc "\\/"
        case '\n' then concat acc "\\n"
        case '\r' then concat acc "\\r"
        case '\t' then concat acc "\\t"
        case _ then
          -- NOTE(johnwikman, 2022-05-13): Ignoring the upper bound on JSON
          -- character size here.
          snoc acc c
        end
    in
    (snoc (foldl escape "\"" s) '\"')
  case JsonFloat f then
    if neqf f f then
      "{\"__float__\": \"nan\"}"
    else if eqf f inf then
      "{\"__float__\": \"inf\"}"
    else if eqf f (negf inf) then
      "{\"__float__\": \"-inf\"}"
    else
      -- NOTE(vsenderov, 2023-09-14): Need to append/prepend 0 to conform to the
      -- JSON standard.  What is the situation in locales that don't use a dot
      -- to delimit decimals?
      let str = float2string f in
      switch str
          case _ ++ "." then snoc str '0'
          case "." ++ _ then cons '0' str
          case _ then str
      end

  case JsonInt i then
    int2string i
  case JsonBool b then
    if b then "true" else "false"
  case JsonNull () then
    "null"
  end
end

recursive let pprintjson2string: JsonValue -> String = lam value.
  _pprintjson2string 0 value
end