type Option a
con Some : all a. a -> Option a
con None : all a. () -> Option a

let myLam = lam abc.
  dprint abc;
  ()

let myLam2 = lam abc.
  let a = Some 2 in
  let b = None () in
  dprint abc;
  ()

mexpr

let b = myLam2 123 in

addi 2 2