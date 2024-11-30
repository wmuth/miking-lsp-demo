type Option a
in
con Some: all a. a -> Option a in
con None: all a. () -> Option a in
let myLam2 =
  lam abc.
    let a = Some
        2 in
    (dprint abc)
    ; {}
in
let b = myLam2 123 in
addi 2 2
