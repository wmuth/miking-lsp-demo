type Option a
in
con Some: all a. a -> Option a in
con None: all a. () -> Option a in
type MyType =
  {a: [Char], b: Int}
in
let myLam =
  lam abc.
    let obj: MyType = { a = "hello", b = 123 } in
    let thing = obj.a in
    match
      obj
    with
      {a = a, b = b}
    then
      (dprint a)
      ; (dprint b)
      ; {}
    else
      {}
in
let b = myLam 123 in
addi 2 3
