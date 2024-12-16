let parseMcoreError = lam err.
  -- Handler for parsing the following error format:
  -- ERROR </Users/didrik/projects/miking/lsp-demo/miking-lsp/test.mc 6:7-6:10>: Unknown variable 'abc'
  
  let parsePos = lam uri. lam pos.
    let parts = strSplit ":" pos in
    let row = string2int (head parts) in
    let col = string2int (head (tail parts)) in
    
    {filename = uri, row = row, col = col}
  in
  
  let parseLocation = lam location.
    let locationParts = strSplit " " location in
    let uri = head locationParts in
    let positions = strSplit "-" (join (tail locationParts)) in
    let startPos = parsePos uri (head positions) in
    let endPos = parsePos uri (head (tail positions)) in
    
    makeInfo startPos endPos
  in
  
  match err with "ERROR <" ++ rest then
    let parts = strSplit ">:" rest in
    let info = parseLocation (head parts) in
    let msg = join (tail parts) in
    
    (info, msg)
  else
    error "Invalid error format in `parseMcoreError`"
    