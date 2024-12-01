type Option a
con Some : all a. a -> Option a
con None : all a. () -> Option a

type MyType = {
  a: String,
  b: Int
}

let myLam = lam abc.
  let obj: MyType = {a = "hello", b = 123} in

  let thing = obj.a in
  dprint thing;
  ()



mexpr

let b = myLam 123 in

addi 2 3