let myLam = lam abc.
  dprint abc;
  ()

let myLam2 = lam abc.
  dprint abc;
  ()

mexpr
dprint (addi myLam myLam2);

()