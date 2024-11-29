let not: Bool -> Bool =
  lam a.
    match
      a
    with
      true
    then
      false
    else
      true
in
let and: Bool -> Bool -> Bool =
  lam a.
    lam b.
      match
        a
      with
        true
      then
        b
      else
        false
in
let or: Bool -> Bool -> Bool =
  lam a.
    lam b.
      match
        a
      with
        true
      then
        true
      else
        b
in
let eqBool: Bool -> Bool -> Bool =
  lam b1: Bool.
    lam b2: Bool.
      match
        b1
      with
        true
      then
        b2
      else match
        b2
      with
        true
      then
        false
      else
        true
in
type Option a
in
con Some: all a. a -> Option a in
con None: all a. () -> Option a in
type These a b
in
con This: all a. all b. a -> These a b in
con That: all a. all b. b -> These a b in
con These: all a. all b. (a, b) -> These a b in
type Either a b
in
con Left: all a. all b. a -> Either a b in
con Right: all a. all b. b -> Either a b in
let optionMap: all a. all b. (a -> b) -> Option a -> Option b =
  lam f.
    lam o.
      match
        o
      with
        Some t
      then
        Some
          (f t)
      else
        None
          {}
in
let optionMapAccum: all a. all b. all acc. (acc -> a -> (acc, b)) -> acc -> Option a -> (acc, Option b) =
  lam f.
    lam acc.
      lam o.
        match
          o
        with
          Some a
        then
          match
            f acc a
          with
            (acc, b)
          in
          (acc, Some
              b)
        else
          (acc, None
            {})
in
let optionJoin: all a. Option (Option a) -> Option a =
  lam o.
    match
      o
    with
      Some t
    then
      t
    else
      None
        {}
in
let optionBind: all a. all b. Option a -> (a -> Option b) -> Option b = lam o.
    lam f.
      optionJoin (optionMap f o)
in
let optionGetOrElse: all a. (() -> a) -> Option a -> a =
  lam d.
    lam o.
      match
        o
      with
        Some t
      then
        t
      else
        d {}
in
let optionGetOr: all a. a -> Option a -> a = lam d.
    optionGetOrElse (lam #var"".
         d)
in
let optionMapOrElse: all a. all b. (() -> b) -> (a -> b) -> Option a -> b =
  lam d.
    lam f.
      lam o.
        optionGetOrElse d (optionMap f o)
in
let optionMapOr: all a. all b. b -> (a -> b) -> Option a -> b =
  lam d.
    lam f.
      lam o.
        optionGetOr d (optionMap f o)
in
let optionMapM: all a. all b. (a -> Option b) -> [a] -> Option [b] =
  lam f.
    lam l.
      recursive
        let g =
          lam l.
            lam acc.
              match
                l
              with
                [ hd ] ++ rest
              then
                match
                  f hd
                with
                  Some x
                then
                  g rest (snoc acc x)
                else
                  None
                    {}
              else
                Some
                  acc
      in
      g l ""
in
let optionContains: all a. Option a -> (a -> Bool) -> Bool = lam o.
    lam p.
      optionMapOr false p o
in
let optionIsSome: all a. Option a -> Bool = lam o.
    optionContains o (lam #var"".
         true)
in
let optionCombine: all a. (a -> a -> Option a) -> Option a -> Option a -> Option a =
  lam f.
    lam o1.
      lam o2.
        let #var"X" = (o1, o2) in
        match
          #var"X"
        with
          (None {}, rhs)
        then
          rhs
        else match
          #var"X"
        with
          (lhs, None {})
        then
          lhs
        else match
          #var"X"
        with
          (Some a, Some b)
        in
        f a b
in
let optionOr: all a. Option a -> Option a -> Option a =
  lam o1.
    lam o2.
      optionCombine
        (lam x.
           lam #var"".
             Some
               x)
        o1
        o2
in
let make: all a. Int -> a -> [a] = lam n.
    lam v.
      create n (lam #var"".
           v)
in
let last: all a. [a] -> a = lam seq.
    get seq (subi (length seq) 1)
in
let init: all a. [a] -> [a] = lam seq.
    subsequence seq 0 (subi (length seq) 1)
in
let eqSeq: all a. all b. (a -> b -> Bool) -> [a] -> [b] -> Bool =
  lam eq.
    lam s1.
      lam s2.
        recursive
          let work =
            lam s1.
              lam s2.
                match
                  (s1, s2)
                with
                  ([ h1 ] ++ t1, [ h2 ] ++ t2)
                then
                  match
                    eq h1 h2
                  with
                    true
                  then
                    work t1 t2
                  else
                    false
                else
                  true
        in
        let n1 = length s1 in
        let n2 = length s2 in
        let ndiff = subi n1 n2 in
        match
          eqi ndiff 0
        with
          true
        then
          work s1 s2
        else
          false
in
let mapOption: all a. all b. (a -> Option b) -> [a] -> [b] =
  lam f.
    recursive
      let work =
        lam as.
          match
            as
          with
            [ a ] ++ as
          then
            match
              f a
            with
              Some b
            then
              cons b (work as)
            else
              work as
          else
            ""
    in
    work
in
let for_: all a. [a] -> (a -> ()) -> () = lam xs.
    lam f.
      iter f xs
in
let foldl1: all a. (a -> a -> a) -> [a] -> a = lam f.
    lam l.
      foldl f (head l) (tail l)
in
let foldr1: all a. (a -> a -> a) -> [a] -> a = lam f.
    lam seq.
      foldr f (last seq) (init seq)
in
recursive
  let unfoldr: all a. all c. (a -> Option (c, a)) -> a -> [c] =
    lam f.
      lam b0.
        let fb = f b0 in
        match
          fb
        with
          None _
        then
          ""
        else match
          fb
        with
          Some (x, b1)
        in
        cons x (unfoldr f b1)
in
let range: Int -> Int -> Int -> [Int] =
  lam s.
    lam e.
      lam by.
        unfoldr
          (lam b.
             match
               leqi e b
             with
               true
             then
               None
                 {}
             else
               Some
                 (b, addi b by))
          s
in
let mapAccumL: all a. all b. all c. (a -> b -> (a, c)) -> a -> [b] -> (a, [c]) =
  lam f: a -> b -> (a, c).
    lam acc.
      lam seq.
        foldl
          (lam tacc: (a, [c]).
             lam x.
               match
                 f tacc.0 x
               with
                 (acc, y)
               in
               (acc, snoc tacc.1 y))
          (acc, "")
          seq
in
let unzip: all a. all b. [(a, b)] -> ([a], [b]) =
  lam l.
    mapAccumL
      (lam l.
         lam p: (a, b).
           (snoc l p.0, p.1))
      ""
      l
in
recursive
  let any: all a. (a -> Bool) -> [a] -> Bool =
    lam p.
      lam seq.
        match
          null seq
        with
          true
        then
          false
        else match
          p (head seq)
        with
          true
        then
          true
        else
          any p (tail seq)
in
recursive
  let forAll: all a. (a -> Bool) -> [a] -> Bool =
    lam p.
      lam seq.
        match
          null seq
        with
          true
        then
          true
        else match
          p (head seq)
        with
          true
        then
          forAll p (tail seq)
        else
          false
in
let join: all a. [[a]] -> [a] = lam seqs.
    foldl concat "" seqs
in
let joinMap: all a. all b. (a -> [b]) -> [a] -> [b] =
  lam f.
    lam a.
      foldl (lam s.
           lam x.
             concat s (f x)) "" a
in
let seqLiftA2: all a. all b. all c. (a -> b -> c) -> [a] -> [b] -> [c] =
  lam f.
    lam as.
      lam bs.
        match
          null bs
        with
          true
        then
          ""
        else
          joinMap (lam a.
               map (f a) bs) as
in
recursive
  let filter: all a. (a -> Bool) -> [a] -> [a] =
    lam p.
      lam seq.
        match
          null seq
        with
          true
        then
          ""
        else match
          p (head seq)
        with
          true
        then
          cons (head seq) (filter p (tail seq))
        else
          filter p (tail seq)
in
recursive
  let findMap: all a. all b. (a -> Option b) -> [a] -> Option b =
    lam f.
      lam seq.
        match
          seq
        with
          [ h ] ++ t
        then
          match
            f h
          with
            Some x
          then
            Some
              x
          else
            findMap f t
        else
          None
            {}
in
let partition: all a. (a -> Bool) -> [a] -> ([a], [a]) =
  lam p.
    lam seq.
      recursive
        let work =
          lam l.
            lam r.
              lam seq.
                match
                  seq
                with
                  ""
                then
                  (l, r)
                else match
                  seq
                with
                  [ s ] ++ seq
                in
                match
                    p s
                  with
                    true
                  then
                    work (cons s l) r seq
                  else
                    work l (cons s r) seq
      in
      work "" "" (reverse seq)
in
recursive
  let quickSort: all a. (a -> a -> Int) -> [a] -> [a] =
    lam cmp.
      lam seq.
        match
          null seq
        with
          true
        then
          seq
        else
          let h = head seq in
          let t = tail seq in
          let lr = partition (lam x.
                 lti (cmp x h) 0) t
          in
          concat (quickSort cmp lr.0) (cons h (quickSort cmp lr.1))
in
let sort = quickSort in
let minIdx: all a. (a -> a -> Int) -> [a] -> Option (Int, a) =
  lam cmp: a -> a -> Int.
    lam seq: [a].
      match
        null seq
      with
        true
      then
        None
          {}
      else match
        foldl
          (lam acc: (Int, Int, a).
             lam e: a.
               match
                 acc
               with
                 (curi, mini, m)
               in
               match
                   lti (cmp m e) 0
                 with
                   true
                 then
                   (addi curi 1, mini, m)
                 else
                   (addi curi 1, curi, e))
          (1, 0, head seq)
          (tail seq)
      with
        (_, i, m)
      in
      Some
          (i, m)
in
let min: all a. (a -> a -> Int) -> [a] -> Option a =
  lam cmp.
    lam seq.
      optionMap
        (lam r.
           match
             r
           with
             (_, m)
           in
           m)
        (minIdx cmp seq)
in
let minOrElse: all a. (() -> a) -> (a -> a -> Int) -> [a] -> a =
  lam d.
    lam cmp.
      lam seq.
        optionGetOrElse d (min cmp seq)
in
let maxOrElse: all a. (() -> a) -> (a -> a -> Int) -> [a] -> a =
  lam d.
    lam cmp.
      minOrElse d (lam l.
           lam r.
             cmp r l)
in
let seqCmp: all a. (a -> a -> Int) -> [a] -> [a] -> Int =
  lam cmp.
    lam s1.
      lam s2.
        recursive
          let work =
            lam s1.
              lam s2.
                match
                  (s1, s2)
                with
                  ([ h1 ] ++ t1, [ h2 ] ++ t2)
                then
                  let c = cmp h1 h2 in
                  match
                    eqi c 0
                  with
                    true
                  then
                    work t1 t2
                  else
                    c
                else
                  0
        in
        let n1 = length s1 in
        let n2 = length s2 in
        let ndiff = subi n1 n2 in
        match
          eqi ndiff 0
        with
          true
        then
          work s1 s2
        else
          ndiff
in
recursive
  let seqJoin: all a. [a] -> [[a]] -> [a] =
    lam delim.
      lam ss.
        match
          null ss
        with
          true
        then
          ""
        else match
          eqi (length ss) 1
        with
          true
        then
          head ss
        else
          concat (concat (head ss) delim) (seqJoin delim (tail ss))
in
let eqChar = lam c1.
    lam c2.
      eqc c1 c2 in
let leqChar = lam c1.
    lam c2.
      leqi (char2int c1) (char2int c2)
in
let geqChar = lam c1.
    lam c2.
      geqi (char2int c1) (char2int c2)
in
let cmpChar = lam c1.
    lam c2.
      subi (char2int c1) (char2int c2)
in
let isWhitespace = lam c.
    any (eqChar c) " \n\t\r" in
let isLowerAlpha =
  lam c.
    let i = char2int c in
    match
      leqi (char2int 'a') i
    with
      true
    then
      leqi i (char2int 'z')
    else
      false
in
let isUpperAlpha =
  lam c.
    let i = char2int c in
    match
      leqi (char2int 'A') i
    with
      true
    then
      leqi i (char2int 'Z')
    else
      false
in
let isAlpha =
  lam c.
    match
      isLowerAlpha c
    with
      true
    then
      true
    else
      isUpperAlpha c
in
let isLowerAlphaOrUnderscore =
  lam c.
    match
      isLowerAlpha c
    with
      true
    then
      true
    else
      eqChar c '_'
in
let isDigit =
  lam c.
    let i = char2int c in
    match
      leqi (char2int '0') i
    with
      true
    then
      leqi i (char2int '9')
    else
      false
in
let isAlphanum =
  lam c.
    match
      isAlpha c
    with
      true
    then
      true
    else
      isDigit c
in
let randAlphanum: () -> Char =
  lam #var"".
    let r = randIntU 0 62 in
    match
      lti r 10
    with
      true
    then
      int2char (addi r 48)
    else match
      lti r 36
    with
      true
    then
      int2char (addi r 55)
    else
      int2char (addi r 61)
in
let eqString = lam s1.
    lam s2.
      eqSeq eqc s1 s2 in
let eqStringSlice =
  lam s1.
    lam s2.
      lam o2.
        lam n2.
          recursive
            let work =
              lam i.
                match
                  eqi i n2
                with
                  true
                then
                  true
                else match
                  eqc (get s1 i) (get s2 (addi o2 i))
                with
                  true
                then
                  work (addi i 1)
                else
                  false
          in
          match
            eqi (length s1) n2
          with
            true
          then
            work 0
          else
            false
in
let cmpString: [Char] -> [Char] -> Int = seqCmp cmpChar in
let string2int =
  lam s.
    recursive
      let string2int_rechelper =
        lam s.
          lam acc.
            match
              null s
            with
              true
            then
              acc
            else
              let fsd = subi (char2int (head s)) (char2int '0') in
              string2int_rechelper (tail s) (addi (muli 10 acc) fsd)
    in
    match
      s
    with
      ""
    then
      0
    else match
      eqChar '-' (head s)
    with
      true
    then
      negi (string2int_rechelper (tail s) 0)
    else
      string2int_rechelper s 0
in
let digit2char = lam d.
    int2char (addi d (char2int '0')) in
let int2string =
  lam n.
    recursive
      let int2string_rechelper =
        lam n.
          lam acc.
            match
              lti n 10
            with
              true
            then
              cons (digit2char n) acc
            else
              int2string_rechelper (divi n 10) (cons (digit2char (modi n 10)) acc)
    in
    match
      lti n 0
    with
      true
    then
      cons '-' (int2string_rechelper (negi n) "")
    else
      int2string_rechelper n ""
in
let strSplit =
  lam delim.
    lam s.
      let n = length s in
      let m = length delim in
      recursive
        let work =
          lam acc.
            lam lastMatch.
              lam i.
                match
                  lti (subi n m) i
                with
                  true
                then
                  snoc acc (subsequence s lastMatch n)
                else match
                  eqStringSlice delim s i m
                with
                  true
                then
                  let nexti = addi i m in
                  work
                    (snoc acc (subsequence s lastMatch (subi i lastMatch)))
                    nexti
                    nexti
                else
                  work acc lastMatch (addi i 1)
      in
      match
        null delim
      with
        true
      then
        [ s ]
      else
        work "" 0 0
in
let strTrim =
  lam s.
    recursive
      let strTrim_init =
        lam s.
          match
            eqString s ""
          with
            true
          then
            s
          else match
            isWhitespace (head s)
          with
            true
          then
            strTrim_init (tail s)
          else
            s
    in
    reverse (strTrim_init (reverse (strTrim_init s)))
in
let stringIsInt =
  lam s.
    match
      null s
    with
      true
    then
      false
    else
      let s =
        match
          eqChar (get s 0) '-'
        with
          true
        then
          tail s
        else
          s
      in
      forAll isDigit s
in
let strJoin: [Char] -> [[Char]] -> [Char] = seqJoin in
type AVLTreeImpl_AuxTree a a
in
type AVLTreeImpl_AVL a a
in
con AVLTreeImpl_Cont: all k. all v. {r: AVLTreeImpl_AVL k v, key: k, next: AVLTreeImpl_AuxTree k v, value: v} -> AVLTreeImpl_AuxTree k v in
con AVLTreeImpl_End: all k. all v. () -> AVLTreeImpl_AuxTree k v in
con AVLTreeImpl_Node: all k. all v. {h: Int, l: AVLTreeImpl_AVL k v, r: AVLTreeImpl_AVL k v, key: k, value: v} -> AVLTreeImpl_AVL k v in
con AVLTreeImpl_Leaf: all k. all v. () -> AVLTreeImpl_AVL k v in
recursive
  let vAVLTreeImpl_avlCmp: all k. all v. (k -> k -> Int) -> (v -> v -> Int) -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v -> Int =
    lam cmpk.
      lam cmpv.
        lam l.
          lam __sem_target.
            match
              __sem_target
            with
              r
            in
            vAVLTreeImpl_avlCmpH
                cmpk
                cmpv
                (vAVLTreeImpl_avlToAux (AVLTreeImpl_End
                     {}) l, vAVLTreeImpl_avlToAux (AVLTreeImpl_End
                     {}) r)
  let vAVLTreeImpl_avlMap: all k. all a. all b. (k -> a -> b) -> AVLTreeImpl_AVL k a -> AVLTreeImpl_AVL k b =
    lam f.
      lam __sem_target.
        match
          __sem_target
        with
          AVLTreeImpl_Leaf _
        then
          AVLTreeImpl_Leaf
            {}
        else match
          __sem_target
        with
          AVLTreeImpl_Node t
        in
        AVLTreeImpl_Node
            { r = vAVLTreeImpl_avlMap f t.r,
              key = t.key,
              value = f t.key t.value,
              h = t.h,
              l = vAVLTreeImpl_avlMap f t.l }
  let vAVLTreeImpl_avlCmpH: all k. all v. (k -> k -> Int) -> (v -> v -> Int) -> (AVLTreeImpl_AuxTree k v, AVLTreeImpl_AuxTree k v) -> Int =
    lam cmpk.
      lam cmpv.
        lam __sem_target.
          match
            __sem_target
          with
            (AVLTreeImpl_End _, AVLTreeImpl_End _)
          then
            0
          else match
            __sem_target
          with
            (AVLTreeImpl_End _, AVLTreeImpl_Cont _)
          then
            negi 1
          else match
            __sem_target
          with
            (AVLTreeImpl_Cont _, AVLTreeImpl_End _)
          then
            1
          else match
            __sem_target
          with
            (AVLTreeImpl_Cont l, AVLTreeImpl_Cont r)
          in
          let dk = cmpk l.key r.key in
            match
              neqi dk 0
            with
              true
            then
              dk
            else
              let dv = cmpv l.value r.value in
              match
                neqi dv 0
              with
                true
              then
                dv
              else
                vAVLTreeImpl_avlCmpH
                  cmpk
                  cmpv
                  (vAVLTreeImpl_avlToAux l.next l.r, vAVLTreeImpl_avlToAux r.next r.r)
  let vAVLTreeImpl_avlFold: all a. all k. all v. (a -> k -> v -> a) -> a -> AVLTreeImpl_AVL k v -> a =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            AVLTreeImpl_Leaf _
          then
            acc
          else match
            __sem_target
          with
            AVLTreeImpl_Node t
          in
          let acc = vAVLTreeImpl_avlFold f acc t.l in
            let acc = f acc t.key t.value in
            vAVLTreeImpl_avlFold f acc t.r
  let vAVLTreeImpl_avlJoin: all k. all v. k -> v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v =
    lam k.
      lam v.
        lam l.
          lam __sem_target.
            match
              __sem_target
            with
              r
            in
            let lh = vAVLTreeImpl_avlHeight l in
              let rh = vAVLTreeImpl_avlHeight r in
              match
                gti lh (addi rh 1)
              with
                true
              then
                vAVLTreeImpl_avlJoinRight k v r l
              else match
                gti rh (addi lh 1)
              with
                true
              then
                vAVLTreeImpl_avlJoinLeft k v l r
              else
                vAVLTreeImpl_avlCreate k v l r
  let vAVLTreeImpl_avlSize: all k. all v. AVLTreeImpl_AVL k v -> Int =
    lam __sem_target.
      match
        __sem_target
      with
        AVLTreeImpl_Leaf _
      then
        0
      else match
        __sem_target
      with
        AVLTreeImpl_Node {r = r, l = l}
      in
      addi (addi (vAVLTreeImpl_avlSize l) (vAVLTreeImpl_avlSize r)) 1
  let vAVLTreeImpl_avlEmpty: all k. all v. () -> AVLTreeImpl_AVL k v =
    lam __sem_target.
      match
        __sem_target
      with
        {}
      in
      AVLTreeImpl_Leaf
          {}
  let vAVLTreeImpl_avlJoin2: all k. all v. AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v =
    lam l.
      lam __sem_target.
        match
          __sem_target
        with
          r
        in
        let #var"X" = (l, r) in
          match
            #var"X"
          with
            (_, AVLTreeImpl_Leaf _)
          then
            l
          else match
            #var"X"
          with
            (AVLTreeImpl_Leaf _, _)
          then
            r
          else match
            #var"X"
          with
            _
          in
          match
              vAVLTreeImpl_avlSplitFirst r
            with
              (rk, rv, r)
            in
            vAVLTreeImpl_avlJoin rk rv l r
  let vAVLTreeImpl_avlSplit: all k. all v. (k -> k -> Int) -> k -> AVLTreeImpl_AVL k v -> (AVLTreeImpl_AVL k v, Option v, AVLTreeImpl_AVL k v) =
    lam cmp.
      lam k.
        lam __sem_target.
          match
            __sem_target
          with
            AVLTreeImpl_Leaf _
          then
            (AVLTreeImpl_Leaf
              {}, None
              {}, AVLTreeImpl_Leaf
              {})
          else match
            __sem_target
          with
            AVLTreeImpl_Node t
          in
          let d = cmp k t.key in
            match
              lti d 0
            with
              true
            then
              match
                vAVLTreeImpl_avlSplit cmp k t.l
              with
                (ll, v, lr)
              in
              (ll, v, vAVLTreeImpl_avlJoin t.key t.value lr t.r)
            else match
              gti d 0
            with
              true
            then
              match
                vAVLTreeImpl_avlSplit cmp k t.r
              with
                (rl, v, rr)
              in
              (vAVLTreeImpl_avlJoin t.key t.value t.l rl, v, rr)
            else
              (t.l, Some
                t.value, t.r)
  let vAVLTreeImpl_avlToAux: all k. all v. AVLTreeImpl_AuxTree k v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AuxTree k v =
    lam acc.
      lam __sem_target.
        match
          __sem_target
        with
          AVLTreeImpl_Node t
        then
          vAVLTreeImpl_avlToAux
            (AVLTreeImpl_Cont
               { r = t.r, key = t.key, next = acc, value = t.value })
            t.l
        else match
          __sem_target
        with
          AVLTreeImpl_Leaf _
        in
        acc
  let vAVLTreeImpl_avlToSeq: all k. all v. [(k, v)] -> AVLTreeImpl_AVL k v -> [(k, v)] =
    lam acc.
      lam __sem_target.
        match
          __sem_target
        with
          AVLTreeImpl_Leaf _
        then
          acc
        else match
          __sem_target
        with
          AVLTreeImpl_Node t
        in
        let acc = vAVLTreeImpl_avlToSeq acc t.r in
          let acc = cons (t.key, t.value) acc in
          vAVLTreeImpl_avlToSeq acc t.l
  let vAVLTreeImpl_avlCreate: all k. all v. k -> v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v =
    lam k.
      lam v.
        lam l.
          lam __sem_target.
            match
              __sem_target
            with
              r
            in
            let lh = vAVLTreeImpl_avlHeight l in
              let rh = vAVLTreeImpl_avlHeight r in
              let h =
                addi
                  (match
                     geqi lh rh
                   with
                     true
                   then
                     lh
                   else
                     rh)
                  1
              in
              AVLTreeImpl_Node
                { r = r, key = k, value = v, h = h, l = l }
  let vAVLTreeImpl_avlHeight: all k. all v. AVLTreeImpl_AVL k v -> Int =
    lam __sem_target.
      match
        __sem_target
      with
        AVLTreeImpl_Leaf _
      then
        0
      else match
        __sem_target
      with
        AVLTreeImpl_Node {h = h}
      in
      h
  let vAVLTreeImpl_avlInsert: all k. all v. (k -> k -> Int) -> k -> v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v =
    lam cmp.
      lam k.
        lam v.
          lam __sem_target.
            match
              __sem_target
            with
              AVLTreeImpl_Leaf _
            then
              AVLTreeImpl_Node
                { r = AVLTreeImpl_Leaf
                      {},
                  key = k,
                  value = v,
                  h = 1,
                  l = AVLTreeImpl_Leaf
                      {} }
            else match
              __sem_target
            with
              AVLTreeImpl_Node t
            in
            let d = cmp k t.key in
              match
                lti d 0
              with
                true
              then
                vAVLTreeImpl_avlJoin t.key t.value (vAVLTreeImpl_avlInsert cmp k v t.l) t.r
              else match
                gti d 0
              with
                true
              then
                vAVLTreeImpl_avlJoin t.key t.value t.l (vAVLTreeImpl_avlInsert cmp k v t.r)
              else
                AVLTreeImpl_Node
                  { t with value = v }
  let vAVLTreeImpl_avlLookup: all k. all v. (k -> k -> Int) -> k -> AVLTreeImpl_AVL k v -> Option v =
    lam cmp.
      lam k.
        lam __sem_target.
          match
            __sem_target
          with
            AVLTreeImpl_Leaf _
          then
            None
              {}
          else match
            __sem_target
          with
            AVLTreeImpl_Node t
          in
          let d = cmp k t.key in
            match
              lti d 0
            with
              true
            then
              vAVLTreeImpl_avlLookup cmp k t.l
            else match
              gti d 0
            with
              true
            then
              vAVLTreeImpl_avlLookup cmp k t.r
            else
              Some
                t.value
  let vAVLTreeImpl_avlRemove: all k. all v. (k -> k -> Int) -> k -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v =
    lam cmp.
      lam k.
        lam __sem_target.
          match
            __sem_target
          with
            AVLTreeImpl_Leaf _
          then
            AVLTreeImpl_Leaf
              {}
          else match
            __sem_target
          with
            AVLTreeImpl_Node t
          in
          let d = cmp k t.key in
            match
              lti d 0
            with
              true
            then
              vAVLTreeImpl_avlJoin t.key t.value (vAVLTreeImpl_avlRemove cmp k t.l) t.r
            else match
              gti d 0
            with
              true
            then
              vAVLTreeImpl_avlJoin t.key t.value t.l (vAVLTreeImpl_avlRemove cmp k t.r)
            else
              vAVLTreeImpl_avlJoin2 t.l t.r
  let vAVLTreeImpl_avlFromSeq: all k. all v. (k -> k -> Int) -> [(k, v)] -> AVLTreeImpl_AVL k v =
    lam cmp.
      lam __sem_target.
        match
          __sem_target
        with
          ""
        then
          AVLTreeImpl_Leaf
            {}
        else match
          __sem_target
        with
          [ (k, v) ]
        then
          AVLTreeImpl_Node
            { r = AVLTreeImpl_Leaf
                  {},
              key = k,
              value = v,
              h = 1,
              l = AVLTreeImpl_Leaf
                  {} }
        else match
          __sem_target
        with
          s
        in
        let mid = divi (length s) 2 in
          match
            splitAt s mid
          with
            (lhs, rhs)
          in
          let l = vAVLTreeImpl_avlFromSeq cmp lhs in
            let r = vAVLTreeImpl_avlFromSeq cmp rhs in
            vAVLTreeImpl_avlUnionWith
              cmp
              (lam #var"".
                 lam rv.
                   rv)
              l
              r
  let vAVLTreeImpl_avlIsEmpty: all k. all v. AVLTreeImpl_AVL k v -> Bool =
    lam __sem_target.
      match
        __sem_target
      with
        AVLTreeImpl_Leaf _
      then
        true
      else match
        __sem_target
      with
        AVLTreeImpl_Node _
      in
      false
  let vAVLTreeImpl_avlJoinLeft: all k. all v. k -> v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v =
    lam k.
      lam v.
        lam l.
          lam __sem_target.
            match
              __sem_target
            with
              AVLTreeImpl_Node tr
            then
              match
                leqi
                  (vAVLTreeImpl_avlHeight tr.l)
                  (addi (vAVLTreeImpl_avlHeight l) 1)
              with
                true
              then
                let t = vAVLTreeImpl_avlCreate k v l tr.l in
                match
                  leqi
                    (vAVLTreeImpl_avlHeight t)
                    (addi (vAVLTreeImpl_avlHeight tr.r) 1)
                with
                  true
                then
                  vAVLTreeImpl_avlCreate tr.key tr.value t tr.r
                else
                  vAVLTreeImpl_avlRotateRight
                    tr.key tr.value tr.r (vAVLTreeImpl_avlRotateLeft k v l tr.l)
              else
                let tx = vAVLTreeImpl_avlJoinLeft k v l tr.l in
                match
                  leqi
                    (vAVLTreeImpl_avlHeight tx)
                    (addi (vAVLTreeImpl_avlHeight tr.r) 1)
                with
                  true
                then
                  vAVLTreeImpl_avlCreate tr.key tr.value tx tr.r
                else
                  vAVLTreeImpl_avlRotateRight tr.key tr.value tr.r tx
            else match
              __sem_target
            with
              AVLTreeImpl_Leaf _
            in
            error "avlJoinLeft: empty tree"
  let vAVLTreeImpl_avlJoinRight: all k. all v. k -> v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v =
    lam k.
      lam v.
        lam r.
          lam __sem_target.
            match
              __sem_target
            with
              AVLTreeImpl_Node tl
            then
              match
                leqi
                  (vAVLTreeImpl_avlHeight tl.r)
                  (addi (vAVLTreeImpl_avlHeight r) 1)
              with
                true
              then
                let t = vAVLTreeImpl_avlCreate k v tl.r r in
                match
                  leqi
                    (vAVLTreeImpl_avlHeight t)
                    (addi (vAVLTreeImpl_avlHeight tl.l) 1)
                with
                  true
                then
                  vAVLTreeImpl_avlCreate tl.key tl.value tl.l t
                else
                  vAVLTreeImpl_avlRotateLeft
                    tl.key tl.value tl.l (vAVLTreeImpl_avlRotateRight k v r tl.r)
              else
                let tx = vAVLTreeImpl_avlJoinRight k v r tl.r in
                match
                  leqi
                    (vAVLTreeImpl_avlHeight tx)
                    (addi (vAVLTreeImpl_avlHeight tl.l) 1)
                with
                  true
                then
                  vAVLTreeImpl_avlCreate tl.key tl.value tl.l tx
                else
                  vAVLTreeImpl_avlRotateLeft tl.key tl.value tl.l tx
            else match
              __sem_target
            with
              AVLTreeImpl_Leaf _
            in
            error "avlJoinRight: empty tree"
  let vAVLTreeImpl_avlUnionWith: all k. all v. (k -> k -> Int) -> (v -> v -> v) -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v =
    lam cmp.
      lam f.
        lam l.
          lam __sem_target.
            match
              __sem_target
            with
              r
            in
            match
                l
              with
                AVLTreeImpl_Leaf _
              then
                r
              else match
                r
              with
                AVLTreeImpl_Leaf _
              then
                l
              else match
                geqi (vAVLTreeImpl_avlHeight l) (vAVLTreeImpl_avlHeight r)
              with
                true
              then
                match
                  l
                with
                  AVLTreeImpl_Node lt
                then
                  match
                    vAVLTreeImpl_avlSplit cmp lt.key r
                  with
                    (rl, rv, rr)
                  in
                  let lhs = vAVLTreeImpl_avlUnionWith cmp f lt.l rl in
                    let rhs = vAVLTreeImpl_avlUnionWith cmp f lt.r rr in
                    let value =
                      match
                        rv
                      with
                        Some x
                      then
                        f lt.value x
                      else lt.value
                    in
                    vAVLTreeImpl_avlJoin lt.key value lhs rhs
                else
                  error "avlUnionWith: empty left tree"
              else match
                r
              with
                AVLTreeImpl_Node rt
              then
                match
                  vAVLTreeImpl_avlSplit cmp rt.key l
                with
                  (ll, lv, lr)
                in
                let lhs = vAVLTreeImpl_avlUnionWith cmp f ll rt.l in
                  let rhs = vAVLTreeImpl_avlUnionWith cmp f lr rt.r in
                  let value =
                    match
                      lv
                    with
                      Some x
                    then
                      f x rt.value
                    else rt.value
                  in
                  vAVLTreeImpl_avlJoin rt.key value lhs rhs
              else
                error "avlUnionWith: empty right tree"
  let vAVLTreeImpl_avlDifference: all k. all a. all b. (k -> k -> Int) -> AVLTreeImpl_AVL k a -> AVLTreeImpl_AVL k b -> AVLTreeImpl_AVL k a =
    lam cmp.
      lam l.
        lam __sem_target.
          match
            __sem_target
          with
            r
          in
          match
              l
            with
              AVLTreeImpl_Leaf _
            then
              AVLTreeImpl_Leaf
                {}
            else match
              r
            with
              AVLTreeImpl_Leaf _
            then
              l
            else match
              l
            with
              AVLTreeImpl_Node lt
            then
              match
                vAVLTreeImpl_avlSplit cmp lt.key r
              with
                (rl, rv, rr)
              in
              let lhs = vAVLTreeImpl_avlDifference cmp lt.l rl in
                let rhs = vAVLTreeImpl_avlDifference cmp lt.r rr in
                match
                  rv
                with
                  Some x
                then
                  vAVLTreeImpl_avlJoin2 lhs rhs
                else
                  vAVLTreeImpl_avlJoin lt.key lt.value lhs rhs
            else
              error "avlDifference: empty left tree"
  let vAVLTreeImpl_avlRotateLeft: all k. all v. k -> v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v =
    lam k.
      lam v.
        lam l.
          lam __sem_target.
            match
              __sem_target
            with
              AVLTreeImpl_Node (rt & {r = rr, l = rl})
            then
              vAVLTreeImpl_avlCreate rt.key rt.value (vAVLTreeImpl_avlCreate k v l rl) rr
            else match
              __sem_target
            with
              AVLTreeImpl_Leaf _
            in
            error "avlRotateLeft: empty tree"
  let vAVLTreeImpl_avlSplitFirst: all k. all v. AVLTreeImpl_AVL k v -> (k, v, AVLTreeImpl_AVL k v) =
    lam __sem_target.
      match
        __sem_target
      with
        AVLTreeImpl_Node (t & {l = AVLTreeImpl_Leaf _})
      then
        (t.key, t.value, t.r)
      else match
        __sem_target
      with
        AVLTreeImpl_Node t
      then
        match
          vAVLTreeImpl_avlSplitFirst t.l
        with
          (k, v, l)
        in
        let hd = subi (vAVLTreeImpl_avlHeight l) (vAVLTreeImpl_avlHeight t.r)
          in
          match
            lti hd (negi 1)
          with
            true
          then
            (k, v, vAVLTreeImpl_avlBalanceRight t.key t.value l t.r)
          else
            (k, v, vAVLTreeImpl_avlCreate t.key t.value l t.r)
      else match
        __sem_target
      with
        AVLTreeImpl_Leaf _
      in
      error "avlSplitLast: empty tree"
  let vAVLTreeImpl_avlRotateRight: all k. all v. k -> v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v =
    lam k.
      lam v.
        lam r.
          lam __sem_target.
            match
              __sem_target
            with
              AVLTreeImpl_Node (lt & {r = lr, l = ll})
            then
              vAVLTreeImpl_avlCreate lt.key lt.value ll (vAVLTreeImpl_avlCreate k v lr r)
            else match
              __sem_target
            with
              AVLTreeImpl_Leaf _
            in
            error "avlRotateRight: empty tree"
  let vAVLTreeImpl_avlBalanceRight: all k. all v. k -> v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v =
    lam k.
      lam v.
        lam l.
          lam __sem_target.
            match
              __sem_target
            with
              r & AVLTreeImpl_Node rt
            then
              match
                geqi (vAVLTreeImpl_avlHeight rt.r) (vAVLTreeImpl_avlHeight rt.l)
              with
                true
              then
                vAVLTreeImpl_avlRotateLeft k v l r
              else
                vAVLTreeImpl_avlRotateRightLeft k v l r
            else match
              __sem_target
            with
              AVLTreeImpl_Leaf _
            in
            error "avlBalanceRight: empty tree"
  let vAVLTreeImpl_avlIntersectWith: all k. all a. all b. all c. (k -> k -> Int) -> (a -> b -> c) -> AVLTreeImpl_AVL k a -> AVLTreeImpl_AVL k b -> AVLTreeImpl_AVL k c =
    lam cmp.
      lam f.
        lam l.
          lam __sem_target.
            match
              __sem_target
            with
              r
            in
            match
                l
              with
                AVLTreeImpl_Leaf _
              then
                AVLTreeImpl_Leaf
                  {}
              else match
                r
              with
                AVLTreeImpl_Leaf _
              then
                AVLTreeImpl_Leaf
                  {}
              else match
                geqi (vAVLTreeImpl_avlHeight l) (vAVLTreeImpl_avlHeight r)
              with
                true
              then
                match
                  l
                with
                  AVLTreeImpl_Node lt
                then
                  match
                    vAVLTreeImpl_avlSplit cmp lt.key r
                  with
                    (rl, rv, rr)
                  in
                  let lhs = vAVLTreeImpl_avlIntersectWith cmp f lt.l rl in
                    let rhs = vAVLTreeImpl_avlIntersectWith cmp f lt.r rr in
                    match
                      rv
                    with
                      Some x
                    then
                      vAVLTreeImpl_avlJoin lt.key (f lt.value x) lhs rhs
                    else
                      vAVLTreeImpl_avlJoin2 lhs rhs
                else
                  error "avlIntersectWith: empty left tree"
              else match
                r
              with
                AVLTreeImpl_Node rt
              then
                match
                  vAVLTreeImpl_avlSplit cmp rt.key l
                with
                  (ll, lv, lr)
                in
                let lhs = vAVLTreeImpl_avlIntersectWith cmp f ll rt.l in
                  let rhs = vAVLTreeImpl_avlIntersectWith cmp f lr rt.r in
                  match
                    lv
                  with
                    Some x
                  then
                    vAVLTreeImpl_avlJoin rt.key (f x rt.value) lhs rhs
                  else
                    vAVLTreeImpl_avlJoin2 lhs rhs
              else
                error "avlIntersectWith: empty right tree"
  let vAVLTreeImpl_avlRotateRightLeft: all k. all v. k -> v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v -> AVLTreeImpl_AVL k v =
    lam k.
      lam v.
        lam l.
          lam __sem_target.
            match
              __sem_target
            with
              AVLTreeImpl_Node (rt & {r = rr, l = AVLTreeImpl_Node rlt})
            then
              vAVLTreeImpl_avlCreate
                rlt.key
                rlt.value
                (vAVLTreeImpl_avlCreate k v l rlt.l)
                (vAVLTreeImpl_avlCreate rt.key rt.value rlt.r rr)
            else match
              __sem_target
            with
              AVLTreeImpl_Node _
            then
              error "avlRotateRightLeft: invalid shape of tree"
            else match
              __sem_target
            with
              AVLTreeImpl_Leaf _
            in
            error "avlRotateRightLeft: empty tree"
in
type Map k v =
  {cmp: k -> k -> Int, root: AVLTreeImpl_AVL k v}
in
let mapEmpty: all k. all v. (k -> k -> Int) -> Map k v = lam cmp.
    { cmp = cmp, root = vAVLTreeImpl_avlEmpty {} }
in
let mapSize: all k. all v. Map k v -> Int = lam m.
    vAVLTreeImpl_avlSize m.root
in
let mapLength: all k. all v. Map k v -> Int = lam m.
    mapSize m
in
let mapIsEmpty: all k. all v. Map k v -> Bool = lam m.
    vAVLTreeImpl_avlIsEmpty m.root
in
let mapRemove: all k. all v. k -> Map k v -> Map k v =
  lam k.
    lam m.
      { m with root = vAVLTreeImpl_avlRemove m.cmp k m.root }
in
let mapFindExn: all k. all v. k -> Map k v -> v =
  lam k.
    lam m.
      optionGetOrElse
        (lam #var"".
           error "mapFindExn: key not found")
        (vAVLTreeImpl_avlLookup m.cmp k m.root)
in
let mapFindOrElse: all k. all v. (() -> v) -> k -> Map k v -> v =
  lam f.
    lam k.
      lam m.
        optionGetOrElse f (vAVLTreeImpl_avlLookup m.cmp k m.root)
in
let mapLookup: all k. all v. k -> Map k v -> Option v =
  lam k.
    lam m.
      vAVLTreeImpl_avlLookup m.cmp k m.root
in
let mapInsert: all k. all v. k -> v -> Map k v -> Map k v =
  lam k.
    lam v.
      lam m.
        { m with root = vAVLTreeImpl_avlInsert m.cmp k v m.root }
in
let mapInsertWith: all k. all v. (v -> v -> v) -> k -> v -> Map k v -> Map k v =
  lam f.
    lam k.
      lam v.
        lam m.
          match
            mapLookup k m
          with
            Some prev
          then
            mapInsert k (f prev v) m
          else
            mapInsert k v m
in
let mapMem: all k. all v. k -> Map k v -> Bool =
  lam k.
    lam m.
      optionIsSome (vAVLTreeImpl_avlLookup m.cmp k m.root)
in
let mapCmp: all k. all v. (v -> v -> Int) -> Map k v -> Map k v -> Int =
  lam cmpv.
    lam m1.
      lam m2.
        vAVLTreeImpl_avlCmp m1.cmp cmpv m1.root m2.root
in
let mapGetCmpFun: all k. all v. Map k v -> k -> k -> Int = lam m.
    m.cmp
in
let mapBindings: all k. all v. Map k v -> [(k, v)] = lam m.
    vAVLTreeImpl_avlToSeq "" m.root
in
let mapToSeq: all k. all v. Map k v -> [(k, v)] = lam m.
    mapBindings m
in
let mapFromSeq: all k. all v. (k -> k -> Int) -> [(k, v)] -> Map k v =
  lam cmp.
    lam bindings.
      { cmp = cmp, root = vAVLTreeImpl_avlFromSeq cmp bindings }
in
let mapFoldWithKey: all k. all v. all a. (a -> k -> v -> a) -> a -> Map k v -> a =
  lam f.
    lam acc.
      lam m.
        vAVLTreeImpl_avlFold f acc m.root
in
let mapMapWithKey: all k. all v1. all v2. (k -> v1 -> v2) -> Map k v1 -> Map k v2 =
  lam f.
    lam m.
      { cmp = m.cmp, root = vAVLTreeImpl_avlMap f m.root }
in
let mapMap: all k. all v1. all v2. (v1 -> v2) -> Map k v1 -> Map k v2 =
  lam f.
    lam m.
      mapMapWithKey (lam #var"".
           lam v.
             f v) m
in
let mapMapAccum: all k. all v1. all v2. all acc. (acc -> k -> v1 -> (acc, v2)) -> acc -> Map k v1 -> (acc, Map k v2) =
  lam f.
    lam acc.
      lam m.
        mapFoldWithKey
          (lam tacc: (acc, Map k v2).
             lam k.
               lam v1.
                 let fval: (acc, v2) = f tacc.0 k v1 in
                 match
                   fval
                 with
                   (acc, v2)
                 in
                 (acc, mapInsert k v2 tacc.1))
          (acc, mapEmpty (mapGetCmpFun m))
          m
in
let mapGetMin: all k. all v. Map k v -> Option (k, v) =
  lam m.
    match
      mapIsEmpty m
    with
      true
    then
      None
        {}
    else match
      vAVLTreeImpl_avlSplitFirst m.root
    with
      (k, v, _)
    in
    Some
        (k, v)
in
let mapKeys: all k. all v. Map k v -> [k] =
  lam m.
    mapFoldWithKey
      (lam ks.
         lam k.
           lam #var"".
             snoc ks k)
      ""
      m
in
let mapValues: all k. all v. Map k v -> [v] =
  lam m.
    mapFoldWithKey
      (lam vs.
         lam #var"".
           lam v.
             snoc vs v)
      ""
      m
in
let mapUnion: all k. all v. Map k v -> Map k v -> Map k v =
  lam l.
    lam r.
      { l
        with
        root =
          vAVLTreeImpl_avlUnionWith
            l.cmp
            (lam #var"".
               lam rv.
                 rv)
            l.root
            r.root }
in
let mapIntersectWith: all k. all a. all b. all c. (a -> b -> c) -> Map k a -> Map k b -> Map k c =
  lam f.
    lam l.
      lam r.
        { cmp = l.cmp,
          root = vAVLTreeImpl_avlIntersectWith l.cmp f l.root r.root }
in
let mapDifference: all k. all v. all v2. Map k v -> Map k v2 -> Map k v =
  lam l.
    lam r.
      { l with root = vAVLTreeImpl_avlDifference l.cmp l.root r.root }
in
let _prod = foldl muli 1 in
type TCreate a =
  [Int] -> ([Int] -> a) -> Tensor[a]
in
let tensorCreate = tensorCreateDense in
let tensorOfSeqOrElse: all a. (() -> Tensor[a]) -> ([Int] -> ([Int] -> a) -> Tensor[a]) -> [Int] -> [a] -> Tensor[a] =
  lam f.
    lam tcreate.
      lam shape.
        lam seq.
          let n = length seq in
          match
            neqi n (_prod shape)
          with
            true
          then
            f {}
          else
            let t =
              tcreate [ n ] (lam idx.
                   get seq (get idx 0))
            in
            tensorReshapeExn t shape
in
let tensorOfSeqExn: all a. ([Int] -> ([Int] -> a) -> Tensor[a]) -> [Int] -> [a] -> Tensor[a] =
  lam x.
    tensorOfSeqOrElse
      (lam #var"".
         error "Empty seq in tensorOfSeqExn") x
in
let test =
  let t1 = tensorOfSeqExn tensorCreateDense [ 3 ] [ 1, 2, 3 ] in
  let t2 = tensorCreateDense [ 3 ] (lam #var"".
         0) in
  {}
in
let inf: Float = divf 1. 0. in
type JsonValue
in
con JsonObject: Map [Char] JsonValue -> JsonValue in
con JsonArray: [JsonValue] -> JsonValue in
con JsonString: [Char] -> JsonValue in
con JsonFloat: Float -> JsonValue in
con JsonInt: Int -> JsonValue in
con JsonBool: Bool -> JsonValue in
con JsonNull: () -> JsonValue in
recursive
  let _jsonErrorString: Int -> [Char] -> [Char] =
    lam pos.
      lam msg.
        join
          [ "Error at position ",
            int2string pos,
            ": ",
            msg ]
  let _jsonError: Int -> [Char] -> Either (JsonValue, [Char], Int) [Char] =
    lam pos.
      lam msg.
        Right
          (_jsonErrorString pos msg)
  let _jsonEatWhitespace: [Char] -> Int -> ([Char], Int) =
    lam s.
      lam pos.
        match
          s
        with
          [ ' ' | '\n' | '\r' | '\t' ] ++ ws
        then
          _jsonEatWhitespace ws (addi pos 1)
        else
          (s, pos)
  let _jsonEatInt: [Char] -> [Char] -> Int -> ([Char], [Char], Int) =
    lam acc.
      lam s.
        lam pos.
          match
            s
          with
            [ c ] ++ ws
          then
            match
              and (geqChar c '0') (leqChar c '9')
            with
              true
            then
              _jsonEatInt (snoc acc c) ws (addi pos 1)
            else
              (acc, s, pos)
          else
            (acc, s, pos)
  let _jsonParse: [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam s.
      lam pos.
        match
          _jsonEatWhitespace s pos
        with
          (s, pos)
        in
        let #var"X" = s in
          match
            #var"X"
          with
            "{" ++ ws
          then
            _jsonParseObject ws (addi pos 1)
          else match
            #var"X"
          with
            "[" ++ ws
          then
            _jsonParseArray ws (addi pos 1)
          else match
            #var"X"
          with
            "\"" ++ ws
          then
            _jsonParseString "" ws (addi pos 1)
          else match
            #var"X"
          with
            "-" ++ ws
          then
            _jsonParseNegativeNumber ws (addi pos 1)
          else match
            #var"X"
          with
            [ '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ] ++ _
          then
            _jsonParseNumber s pos
          else match
            #var"X"
          with
            "true" ++ ws
          then
            Left
              (JsonBool
                true, ws, addi pos 4)
          else match
            #var"X"
          with
            "false" ++ ws
          then
            Left
              (JsonBool
                false, ws, addi pos 5)
          else match
            #var"X"
          with
            "null" ++ ws
          then
            Left
              (JsonNull
                {}, ws, addi pos 4)
          else match
            #var"X"
          with
            _
          in
          _jsonError pos "Invalid start to a JSON value."
  let _jsonParseObject: [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam s.
      lam pos.
        match
          _jsonEatWhitespace s pos
        with
          (s, pos)
        in
        let acc = mapEmpty cmpString in
          match
            s
          with
            "}" ++ ws
          then
            Left
              (JsonObject
                acc, ws, addi pos 1)
          else
            _jsonParseObjectContents acc s pos
  let _jsonParseObjectContents: Map [Char] JsonValue -> [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam acc.
      lam s.
        lam pos.
          match
            _jsonEatWhitespace s pos
          with
            (s, pos)
          in
          let #var"X" = _jsonParse s pos in
            match
              #var"X"
            with
              Left (JsonString key, s, pos)
            then
              match
                _jsonEatWhitespace s pos
              with
                (s, pos)
              in
              match
                  s
                with
                  ":" ++ ws
                then
                  match
                    _jsonEatWhitespace ws (addi pos 1)
                  with
                    (s, pos)
                  in
                  let #var"X" = _jsonParse s pos in
                    match
                      #var"X"
                    with
                      Left (value, s, pos)
                    then
                      let acc = mapInsert key value acc in
                      match
                        _jsonEatWhitespace s pos
                      with
                        (s, pos)
                      in
                      match
                          s
                        with
                          "," ++ ws
                        then
                          _jsonParseObjectContents acc ws (addi pos 1)
                        else match
                          s
                        with
                          "}" ++ ws
                        then
                          Left
                            (JsonObject
                              acc, ws, addi pos 1)
                        else
                          _jsonError pos "Expected comma or closing bracket for JSON object."
                    else match
                      #var"X"
                    with
                      Right err
                    in
                    Right
                        err
                else
                  _jsonError pos "Expected colon after property key."
            else match
              #var"X"
            with
              Left _
            then
              _jsonError pos "Expected string as property key."
            else match
              #var"X"
            with
              Right err
            in
            Right
                err
  let _jsonParseArray: [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam s.
      lam pos.
        match
          _jsonEatWhitespace s pos
        with
          (s, pos)
        in
        match
            s
          with
            "]" ++ ws
          then
            Left
              (JsonArray
                "", ws, addi pos 1)
          else
            _jsonParseArrayContents "" s pos
  let _jsonParseArrayContents: [JsonValue] -> [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam acc.
      lam s.
        lam pos.
          match
            _jsonEatWhitespace s pos
          with
            (s, pos)
          in
          let result = _jsonParse s pos in
            let #var"X" = result in
            match
              #var"X"
            with
              Left (value, s, pos)
            then
              let acc = snoc acc value in
              match
                _jsonEatWhitespace s pos
              with
                (s, pos)
              in
              match
                  s
                with
                  "," ++ ws
                then
                  _jsonParseArrayContents acc ws (addi pos 1)
                else match
                  s
                with
                  "]" ++ ws
                then
                  Left
                    (JsonArray
                      acc, ws, addi pos 1)
                else
                  _jsonError pos "Expected comma or closing bracket of JSON array."
            else match
              #var"X"
            with
              Right err
            in
            Right
                err
  let _jsonParseString: [Char] -> [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam acc.
      lam s.
        lam pos.
          match
            s
          with
            [ '\\',
              c ] ++ ws
          then
            let #var"X" = c in
            match
              #var"X"
            with
              '\"'
            then
              _jsonParseString (snoc acc '\"') ws (addi pos 2)
            else match
              #var"X"
            with
              '\\'
            then
              _jsonParseString (snoc acc '\\') ws (addi pos 2)
            else match
              #var"X"
            with
              '/'
            then
              _jsonParseString (snoc acc '/') ws (addi pos 2)
            else match
              #var"X"
            with
              'b'
            then
              _jsonParseString (snoc acc (int2char 8)) ws (addi pos 2)
            else match
              #var"X"
            with
              'f'
            then
              _jsonParseString (snoc acc (int2char 12)) ws (addi pos 2)
            else match
              #var"X"
            with
              'n'
            then
              _jsonParseString (snoc acc '\n') ws (addi pos 2)
            else match
              #var"X"
            with
              'r'
            then
              _jsonParseString (snoc acc '\r') ws (addi pos 2)
            else match
              #var"X"
            with
              't'
            then
              _jsonParseString (snoc acc '\t') ws (addi pos 2)
            else match
              #var"X"
            with
              'u'
            then
              match
                ws
              with
                [ h3,
                  h2,
                  h1,
                  h0 ] ++ ws
              then
                let hex2int: Char -> Option Int =
                  lam hc.
                    match
                      and (geqChar hc '0') (leqChar hc '9')
                    with
                      true
                    then
                      Some
                        (subi (char2int hc) (char2int '0'))
                    else match
                      and (geqChar hc 'A') (leqChar hc 'F')
                    with
                      true
                    then
                      Some
                        (addi (subi (char2int hc) (char2int 'A')) 10)
                    else match
                      and (geqChar hc 'a') (leqChar hc 'f')
                    with
                      true
                    then
                      Some
                        (addi (subi (char2int hc) (char2int 'a')) 10)
                    else
                      None
                        {}
                in
                let conv =
                  foldl
                    (lam acc: Option Int.
                       lam hc.
                         match
                           acc
                         with
                           Some accv
                         then
                           match
                             hex2int hc
                           with
                             Some hv
                           then
                             Some
                               (addi (muli accv 16) hv)
                           else
                             None
                               {}
                         else
                           None
                             {})
                    (Some
                       0)
                    [ h3,
                      h2,
                      h1,
                      h0 ]
                in
                match
                  conv
                with
                  Some v
                then
                  _jsonParseString (snoc acc (int2char v)) ws (addi pos 6)
                else
                  _jsonError (addi pos 2) "Expected 4 hexadecimal characters"
              else
                _jsonError (addi pos 2) "Expected 4 hexadecimal characters"
            else match
              #var"X"
            with
              _
            in
            _jsonError
                (addi pos 1)
                (join
                   [ "Invalid escape char \'",
                     [ c ],
                     "\' (decimal value: ",
                     int2string (char2int c),
                     ")" ])
          else match
            s
          with
            "\"" ++ ws
          then
            Left
              (JsonString
                acc, ws, addi pos 1)
          else match
            s
          with
            [ c ] ++ ws
          then
            _jsonParseString (snoc acc c) ws (addi pos 1)
          else
            _jsonError pos "Non-terminated string."
  let _jsonParseNegativeNumber: [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam s.
      lam pos.
        let num = _jsonParseNumber s pos in
        let #var"X" = num in
        match
          #var"X"
        with
          Left (JsonInt i, s, pos)
        then
          Left
            (JsonInt
              (negi i), s, pos)
        else match
          #var"X"
        with
          Left (JsonFloat f, s, pos)
        then
          Left
            (JsonFloat
              (negf f), s, pos)
        else match
          #var"X"
        with
          Left _
        then
          _jsonError pos "Internal error, did not get a number back."
        else match
          #var"X"
        with
          Right err
        in
        Right
            err
  let _jsonParseNumber: [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam s.
      lam pos.
        match
          s
        with
          "0" ++ ws
        then
          _jsonParseNumberDecimals "0" ws (addi pos 1)
        else match
          s
        with
          [ c & ('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') ] ++ ws
        then
          match
            _jsonEatInt [ c ] ws (addi pos 1)
          with
            (intStr, ws, pos)
          in
          _jsonParseNumberDecimals intStr ws pos
        else
          _jsonError pos "Expected a number."
  let _jsonParseNumberDecimals: [Char] -> [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam intStr.
      lam s.
        lam pos.
          match
            s
          with
            "." ++ ws
          then
            match
              _jsonEatInt "" ws (addi pos 1)
            with
              (decimals, ws, pos)
            in
            match
                null decimals
              with
                true
              then
                _jsonError pos "Expected decimals after dot in a number."
              else
                _jsonParseNumberExponent intStr decimals ws pos
          else match
            s
          with
            [ 'e' | 'E' ] ++ _
          then
            _jsonParseNumberExponent intStr "0" s pos
          else
            Left
              (JsonInt
                (string2int intStr), s, pos)
  let _jsonParseNumberExponent: [Char] -> [Char] -> [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam intStr.
      lam decimals.
        lam s.
          lam pos.
            match
              s
            with
              [ 'e' | 'E' ] ++ ws
            then
              match
                ws
              with
                [ c & ('+' | '-') ] ++ ws
              then
                match
                  _jsonEatInt "" ws (addi pos 2)
                with
                  (exponent, ws, pos)
                in
                match
                    null exponent
                  with
                    true
                  then
                    _jsonError pos "Expected exponent digits."
                  else
                    let floatStr =
                      join
                        [ intStr,
                          ".",
                          decimals,
                          "e",
                          [ c ],
                          exponent ]
                    in
                    Left
                      (JsonFloat
                        (string2float floatStr), ws, pos)
              else match
                _jsonEatInt "" ws (addi pos 1)
              with
                (exponent, ws, pos)
              in
              match
                  null exponent
                with
                  true
                then
                  _jsonError pos "Expected exponent digits."
                else
                  let floatStr =
                    join
                      [ intStr,
                        ".",
                        decimals,
                        "e",
                        exponent ]
                  in
                  Left
                    (JsonFloat
                      (string2float floatStr), ws, pos)
            else
              let floatStr =
                join
                  [ intStr,
                    ".",
                    decimals ]
              in
              Left
                (JsonFloat
                  (string2float floatStr), s, pos)
in
let jsonParse: [Char] -> Either JsonValue [Char] =
  lam s.
    let result = _jsonParse s 0 in
    let #var"X" = result in
    match
      #var"X"
    with
      Left (value, s, pos)
    then
      match
        _jsonEatWhitespace s pos
      with
        (s, pos)
      in
      match
          null s
        with
          true
        then
          Left
            value
        else
          Right
            (_jsonErrorString pos "Trailing JSON content.")
    else match
      #var"X"
    with
      Right err
    in
    Right
        err
in
let jsonParseExn: [Char] -> JsonValue =
  lam s.
    let result = jsonParse s in
    let #var"X" = result in
    match
      #var"X"
    with
      Left value
    then
      value
    else match
      #var"X"
    with
      Right errmsg
    in
    error errmsg
in
recursive
  let json2string: JsonValue -> [Char] =
    lam value.
      let #var"X" = value in
      match
        #var"X"
      with
        JsonObject properties
      then
        let proplist =
          mapFoldWithKey
            (lam acc.
               lam k.
                 lam v.
                   snoc
                     acc
                     (join
                        [ json2string (JsonString
                               k),
                          ":",
                          json2string v ]))
            ""
            properties
        in
        cons '{' (snoc (strJoin "," proplist) '}')
      else match
        #var"X"
      with
        JsonArray values
      then
        cons '[' (snoc (strJoin "," (map json2string values)) ']')
      else match
        #var"X"
      with
        JsonString s
      then
        let escape: [Char] -> Char -> [Char] =
          lam acc.
            lam c.
              let cval: Int = char2int c in
              match
                eqi cval 8
              with
                true
              then
                concat acc "\\b"
              else match
                eqi cval 12
              with
                true
              then
                concat acc "\\f"
              else match
                or (lti cval 32) (eqi cval 127)
              with
                true
              then
                let tohex: Int -> Char =
                  lam x.
                    match
                      lti x 10
                    with
                      true
                    then
                      int2char (addi x (char2int '0'))
                    else
                      int2char (addi (subi x 10) (char2int 'a'))
                in
                concat
                  acc
                  [ '\\',
                    'u',
                    '0',
                    '0',
                    tohex (divi cval 16),
                    tohex (modi cval 16) ]
              else
                let #var"X" = c in
                match
                  #var"X"
                with
                  '\"'
                then
                  concat acc "\\\""
                else match
                  #var"X"
                with
                  '\\'
                then
                  concat acc "\\\\"
                else match
                  #var"X"
                with
                  '/'
                then
                  concat acc "\\/"
                else match
                  #var"X"
                with
                  '\n'
                then
                  concat acc "\\n"
                else match
                  #var"X"
                with
                  '\r'
                then
                  concat acc "\\r"
                else match
                  #var"X"
                with
                  '\t'
                then
                  concat acc "\\t"
                else match
                  #var"X"
                with
                  _
                in
                snoc acc c
        in
        snoc (foldl escape "\"" s) '\"'
      else match
        #var"X"
      with
        JsonFloat f
      then
        match
          neqf f f
        with
          true
        then
          "{\"__float__\": \"nan\"}"
        else match
          eqf f inf
        with
          true
        then
          "{\"__float__\": \"inf\"}"
        else match
          eqf f (negf inf)
        with
          true
        then
          "{\"__float__\": \"-inf\"}"
        else
          let str = float2string f in
          let #var"X" = str in
          match
            #var"X"
          with
            _ ++ "."
          then
            snoc str '0'
          else match
            #var"X"
          with
            "." ++ _
          then
            cons '0' str
          else match
            #var"X"
          with
            _
          in
          str
      else match
        #var"X"
      with
        JsonInt i
      then
        int2string i
      else match
        #var"X"
      with
        JsonBool b
      then
        match
          b
        with
          true
        then
          "true"
        else
          "false"
      else match
        #var"X"
      with
        JsonNull {}
      in
      "null"
in
let jsonDeserializeInt: JsonValue -> Option Int =
  lam ji.
    match
      ji
    with
      JsonInt i
    then
      Some
        i
    else
      None
        {}
in
let jsonDeserializeSeq: all a. (JsonValue -> Option a) -> JsonValue -> Option [a] =
  lam f.
    lam jseq.
      match
        jseq
      with
        JsonArray jv
      then
        optionMapM f jv
      else
        None
          {}
in
let keyTensorShape = "__tensorShape__" in
let keyTensor = "__tensor__" in
let jsonDeserializeTensor: all a. ([Int] -> ([Int] -> a) -> Tensor[a]) -> (JsonValue -> Option a) -> JsonValue -> Option Tensor[a] =
  lam tcreate.
    lam f.
      lam jtensor.
        match
          jtensor
        with
          JsonObject m
        then
          match
            mapLookup keyTensorShape m
          with
            Some jshape
          then
            match
              mapLookup keyTensor m
            with
              Some jseq
            then
              match
                jsonDeserializeSeq jsonDeserializeInt jshape
              with
                Some shape
              then
                match
                  jsonDeserializeSeq f jseq
                with
                  Some seq
                then
                  Some
                    (tensorOfSeqExn tcreate shape seq)
                else
                  None
                    {}
              else
                None
                  {}
            else
              None
                {}
          else
            None
              {}
        else
          None
            {}
in
let jsonDeserializeTensorCArrayInt: (JsonValue -> Option Int) -> JsonValue -> Option Tensor[Int] = jsonDeserializeTensor tensorCreateCArrayInt
in
let jsonDeserializeTensorCArrayFloat: (JsonValue -> Option Float) -> JsonValue -> Option Tensor[Float] = jsonDeserializeTensor tensorCreateCArrayFloat
in
type Info
in
con Info: {col1: Int, col2: Int, row1: Int, row2: Int, filename: [Char]} -> Info in
con NoInfo: () -> Info in
type Pos =
  {col: Int, row: Int, filename: [Char]}
in
let initPos: [Char] -> Pos = lam filename.
    { filename = filename, col = 0, row = 1 }
in
let posVal: [Char] -> Int -> Int -> Pos =
  lam filename.
    lam row.
      lam col.
        { filename = filename, col = col, row = row }
in
let advanceCol: Pos -> Int -> Pos = lam p: Pos.
    lam n.
      { p with col = addi p.col n }
in
let advanceRow: Pos -> Int -> Pos =
  lam p: Pos.
    lam n.
      { p with row = addi p.row n, col = 0 }
in
let makeInfo: Pos -> Pos -> Info =
  lam p1: Pos.
    lam p2: Pos.
      Info
        { col1 = p1.col,
          col2 = p2.col,
          row1 = p1.row,
          row2 = p2.row,
          filename = p1.filename }
in
let mergeInfo: Info -> Info -> Info =
  lam fi1: Info.
    lam fi2: Info.
      match
        fi1
      with
        Info r1
      then
        match
          fi2
        with
          Info r2
        then
          Info
            { col1 = r1.col1,
              col2 = r2.col2,
              row1 = r1.row1,
              row2 = r2.row2,
              filename = r1.filename }
        else
          fi1
      else
        fi2
in
let infoVal: [Char] -> Int -> Int -> Int -> Int -> Info =
  lam filename.
    lam r1.
      lam c1.
        lam r2.
          lam c2.
            Info
              { col1 = c1, col2 = c2, row1 = r1, row2 = r2, filename = filename }
in
let info2str: Info -> [Char] =
  lam fi.
    match
      fi
    with
      NoInfo {}
    then
      "<No file info>"
    else match
      fi
    with
      Info (r & {row1 = 0})
    then
      join [ "<",
          r.filename,
          ">" ]
    else match
      fi
    with
      Info r
    in
    join
        [ "<",
          r.filename,
          " ",
          int2string r.row1,
          ":",
          int2string r.col1,
          "-",
          int2string r.row2,
          ":",
          int2string r.col2,
          ">" ]
in
let infoErrorString: Info -> [Char] -> [Char] =
  lam fi.
    lam str.
      join
        [ "ERROR ",
          info2str fi,
          ":\n",
          str ]
in
let infoErrorExit: Info -> [Char] -> Unknown =
  lam fi.
    lam str.
      (print
           (join
              [ "\n",
                infoErrorString fi str,
                "\n" ]))
      ; exit 1
in
let posErrorExit: Pos -> [Char] -> Unknown =
  lam p: Pos.
    lam str.
      infoErrorExit (infoVal p.filename p.row p.col p.row p.col) str
in
let infoCmp: Info -> Info -> Int =
  lam i1.
    lam i2.
      cmpString (info2str i1) (info2str i2)
in
type Name =
  ([Char], Symbol)
in
let _noSymbol = gensym {} in
let nameNoSym: [Char] -> Name = lam x.
    (x, _noSymbol) in
let nameSym: [Char] -> Name = lam x.
    (x, gensym {}) in
let nameEqStr: Name -> Name -> Bool = lam n1: Name.
    lam n2: Name.
      eqString n1.0 n2.0
in
let _t1 = nameNoSym "foo" in
let _t2 = nameSym "foo" in
let _t3 = nameSym "bar" in
let nameHasSym: Name -> Bool = lam n: Name.
    not (eqsym n.1 _noSymbol)
in
let nameEqSym: Name -> Name -> Bool =
  lam n1: Name.
    lam n2: Name.
      match
        nameHasSym n1
      with
        true
      then
        match
          nameHasSym n2
        with
          true
        then
          eqsym n1.1 n2.1
        else
          false
      else
        false
in
let _t1 = nameNoSym "foo" in
let _t2 = nameSym "foo" in
let _t3 = nameSym "foo" in
let nameEq: Name -> Name -> Bool =
  lam n1: Name.
    lam n2: Name.
      match
        nameEqSym n1 n2
      with
        true
      then
        true
      else match
        nameHasSym n1
      with
        true
      then
        false
      else match
        nameHasSym n2
      with
        true
      then
        false
      else
        nameEqStr n1 n2
in
let _t1 = nameNoSym "foo" in
let _t2 = nameSym "foo" in
let _t3 = nameSym "foo" in
let nameSetNewSym: Name -> Name = lam n: Name.
    (n.0, gensym {})
in
let _t1 = nameNoSym "foo" in
let _t2 = nameSym "foo" in
let _t1 = nameNoSym "foo" in
let _t2 = nameSym "foo" in
let _s = gensym {} in
let _t1 = nameSym "Foo" in
let _t2 = nameNoSym "Foo" in
let _t1 = nameNoSym "foo" in
let _t2 = nameSym "bar" in
let nameGetStr: Name -> [Char] = lam n: Name.
    n.0 in
let nameGetSym: Name -> Option Symbol =
  lam n: Name.
    match
      eqsym n.1 _noSymbol
    with
      true
    then
      None
        {}
    else
      Some
        n.1
in
let _s = gensym {} in
let nameCmp: Name -> Name -> Int =
  lam n1: Name.
    lam n2: Name.
      match
        nameGetSym n1
      with
        Some a
      then
        match
          nameGetSym n2
        with
          Some b
        then
          subi (sym2hash a) (sym2hash b)
        else
          negi 1
      else match
        nameGetSym n2
      with
        Some _
      then
        1
      else
        cmpString (nameGetStr n1) (nameGetStr n2)
in
let _s1 = gensym {} in
let _s2 = gensym {} in
type SID =
  Int
in
let cmpSID = subi in
let _sidCounter = ref 0 in
let _mapStringToSid = ref (mapEmpty cmpString) in
let _mapSidToString = ref (mapEmpty subi) in
let sidToString: SID -> [Char] =
  lam sid.
    mapFindOrElse
      (lam #var"".
         error "SID is not defined")
      sid
      (deref _mapSidToString)
in
let stringToSid: [Char] -> SID =
  lam str.
    mapFindOrElse
      (lam #var"".
         (modref _sidCounter (addi (deref _sidCounter) 1))
         ; let sid = deref _sidCounter in
         (modref _mapStringToSid (mapInsert str sid (deref _mapStringToSid)))
         ; (modref _mapSidToString (mapInsert sid str (deref _mapSidToString)))
         ; sid)
      str
      (deref _mapStringToSid)
in
type Set a =
  Map a ()
in
let setEmpty: all a. (a -> a -> Int) -> Set a = lam cmp.
    mapEmpty cmp
in
let setIsEmpty: all a. Set a -> Bool = mapIsEmpty in
let setInsert: all a. a -> Set a -> Set a = lam e.
    lam s.
      mapInsert e {} s
in
let setMem: all a. a -> Set a -> Bool = lam e.
    lam s.
      mapMem e s
in
let setUnion: all a. Set a -> Set a -> Set a = lam s1.
    lam s2.
      mapUnion s1 s2
in
let setIntersect: all a. Set a -> Set a -> Set a =
  lam s1.
    lam s2.
      mapIntersectWith (lam #var"".
           lam #var"".
             {}) s1 s2
in
let setSubtract: all a. Set a -> Set a -> Set a = lam s1.
    lam s2.
      mapDifference s1 s2
in
let setOfSeq: all a. (a -> a -> Int) -> [a] -> Set a =
  lam cmp.
    lam seq.
      foldr setInsert (setEmpty cmp) seq
in
let setFold: all a. all acc. (acc -> a -> acc) -> acc -> Set a -> acc =
  lam f.
    lam acc.
      lam s.
        mapFoldWithKey
          (lam acc.
             lam k.
               lam #var"".
                 f acc k)
          acc
          s
in
let setToSeq: all a. Set a -> [a] = lam s.
    mapKeys s in
let setCmp: all a. Set a -> Set a -> Int =
  lam m1.
    lam m2.
      mapCmp (lam #var"".
           lam #var"".
             0) m1 m2
in
type Ast_Type
in
type Ast_Kind
in
type Ast_Expr
in
type Ast_Pat
in
con VarAst_TmVar: {ty: Ast_Type, info: Info, ident: Name, frozen: Bool} -> Ast_Expr in
con AppAst_TmApp: {ty: Ast_Type, lhs: Ast_Expr, rhs: Ast_Expr, info: Info} -> Ast_Expr in
con LamAst_TmLam: {ty: Ast_Type, body: Ast_Expr, info: Info, ident: Name, tyAnnot: Ast_Type, tyParam: Ast_Type} -> Ast_Expr in
con LetAst_TmLet: {ty: Ast_Type, body: Ast_Expr, info: Info, ident: Name, inexpr: Ast_Expr, tyBody: Ast_Type, tyAnnot: Ast_Type} -> Ast_Expr in
type RecLetsAst_RecLetBinding =
  {body: Ast_Expr, info: Info, ident: Name, tyBody: Ast_Type, tyAnnot: Ast_Type}
in
con RecLetsAst_TmRecLets: {ty: Ast_Type, info: Info, inexpr: Ast_Expr, bindings: [RecLetsAst_RecLetBinding]} -> Ast_Expr in
type ConstAst_Const
in
con ConstAst_TmConst: {ty: Ast_Type, val: ConstAst_Const, info: Info} -> Ast_Expr in
con SeqAst_TmSeq: {ty: Ast_Type, tms: [Ast_Expr], info: Info} -> Ast_Expr in
con RecordAst_TmRecordUpdate: {ty: Ast_Type, key: SID, rec: Ast_Expr, info: Info, value: Ast_Expr} -> Ast_Expr in
con RecordAst_TmRecord: {ty: Ast_Type, info: Info, bindings: Map SID Ast_Expr} -> Ast_Expr in
con TypeAst_TmType: {ty: Ast_Type, info: Info, ident: Name, inexpr: Ast_Expr, params: [Name], tyIdent: Ast_Type} -> Ast_Expr in
con DataAst_TmConDef: {ty: Ast_Type, info: Info, ident: Name, inexpr: Ast_Expr, tyIdent: Ast_Type} -> Ast_Expr in
con DataAst_TmConApp: {ty: Ast_Type, body: Ast_Expr, info: Info, ident: Name} -> Ast_Expr in
con MatchAst_TmMatch: {ty: Ast_Type, els: Ast_Expr, pat: Ast_Pat, thn: Ast_Expr, info: Info, target: Ast_Expr} -> Ast_Expr in
con UtestAst_TmUtest: {ty: Ast_Type, info: Info, next: Ast_Expr, test: Ast_Expr, tusing: Option Ast_Expr, tonfail: Option Ast_Expr, expected: Ast_Expr} -> Ast_Expr in
con NeverAst_TmNever: {ty: Ast_Type, info: Info} -> Ast_Expr in
con ExtAst_TmExt: {ty: Ast_Type, info: Info, ident: Name, effect: Bool, inexpr: Ast_Expr, tyIdent: Ast_Type} -> Ast_Expr in
con UnsafeCoerceAst_CUnsafeCoerce: () -> ConstAst_Const in
con IntAst_CInt: {val: Int} -> ConstAst_Const in
con ArithIntAst_CSubi: () -> ConstAst_Const in
con ArithIntAst_CNegi: () -> ConstAst_Const in
con ArithIntAst_CMuli: () -> ConstAst_Const in
con ArithIntAst_CModi: () -> ConstAst_Const in
con ArithIntAst_CDivi: () -> ConstAst_Const in
con ArithIntAst_CAddi: () -> ConstAst_Const in
con ShiftIntAst_CSrli: () -> ConstAst_Const in
con ShiftIntAst_CSrai: () -> ConstAst_Const in
con ShiftIntAst_CSlli: () -> ConstAst_Const in
con FloatAst_CFloat: {val: Float} -> ConstAst_Const in
con ArithFloatAst_CSubf: () -> ConstAst_Const in
con ArithFloatAst_CNegf: () -> ConstAst_Const in
con ArithFloatAst_CMulf: () -> ConstAst_Const in
con ArithFloatAst_CDivf: () -> ConstAst_Const in
con ArithFloatAst_CAddf: () -> ConstAst_Const in
con FloatIntConversionAst_CInt2float: () -> ConstAst_Const in
con FloatIntConversionAst_CRoundfi: () -> ConstAst_Const in
con FloatIntConversionAst_CFloorfi: () -> ConstAst_Const in
con FloatIntConversionAst_CCeilfi: () -> ConstAst_Const in
con BoolAst_CBool: {val: Bool} -> ConstAst_Const in
con CmpIntAst_CNeqi: () -> ConstAst_Const in
con CmpIntAst_CLeqi: () -> ConstAst_Const in
con CmpIntAst_CGeqi: () -> ConstAst_Const in
con CmpIntAst_CLti: () -> ConstAst_Const in
con CmpIntAst_CGti: () -> ConstAst_Const in
con CmpIntAst_CEqi: () -> ConstAst_Const in
con CmpFloatAst_CNeqf: () -> ConstAst_Const in
con CmpFloatAst_CLeqf: () -> ConstAst_Const in
con CmpFloatAst_CGeqf: () -> ConstAst_Const in
con CmpFloatAst_CLtf: () -> ConstAst_Const in
con CmpFloatAst_CGtf: () -> ConstAst_Const in
con CmpFloatAst_CEqf: () -> ConstAst_Const in
con CharAst_CChar: {val: Char} -> ConstAst_Const in
con CmpCharAst_CEqc: () -> ConstAst_Const in
con IntCharConversionAst_CInt2Char: () -> ConstAst_Const in
con IntCharConversionAst_CChar2Int: () -> ConstAst_Const in
con FloatStringConversionAst_CStringIsFloat: () -> ConstAst_Const in
con FloatStringConversionAst_CString2float: () -> ConstAst_Const in
con FloatStringConversionAst_CFloat2string: () -> ConstAst_Const in
con SymbAst_CSym2hash: () -> ConstAst_Const in
con SymbAst_CGensym: () -> ConstAst_Const in
con SymbAst_CSymb: {val: Symbol} -> ConstAst_Const in
con CmpSymbAst_CEqsym: () -> ConstAst_Const in
con SeqOpAst_CSubsequence: () -> ConstAst_Const in
con SeqOpAst_CCreateRope: () -> ConstAst_Const in
con SeqOpAst_CCreateList: () -> ConstAst_Const in
con SeqOpAst_CSplitAt: () -> ConstAst_Const in
con SeqOpAst_CReverse: () -> ConstAst_Const in
con SeqOpAst_CLength: () -> ConstAst_Const in
con SeqOpAst_CIsRope: () -> ConstAst_Const in
con SeqOpAst_CIsList: () -> ConstAst_Const in
con SeqOpAst_CCreate: () -> ConstAst_Const in
con SeqOpAst_CConcat: () -> ConstAst_Const in
con SeqOpAst_CIteri: () -> ConstAst_Const in
con SeqOpAst_CFoldr: () -> ConstAst_Const in
con SeqOpAst_CFoldl: () -> ConstAst_Const in
con SeqOpAst_CTail: () -> ConstAst_Const in
con SeqOpAst_CSnoc: () -> ConstAst_Const in
con SeqOpAst_CNull: () -> ConstAst_Const in
con SeqOpAst_CMapi: () -> ConstAst_Const in
con SeqOpAst_CIter: () -> ConstAst_Const in
con SeqOpAst_CHead: () -> ConstAst_Const in
con SeqOpAst_CCons: () -> ConstAst_Const in
con SeqOpAst_CSet: () -> ConstAst_Const in
con SeqOpAst_CMap: () -> ConstAst_Const in
con SeqOpAst_CGet: () -> ConstAst_Const in
con FileOpAst_CFileExists: () -> ConstAst_Const in
con FileOpAst_CFileDelete: () -> ConstAst_Const in
con FileOpAst_CFileWrite: () -> ConstAst_Const in
con FileOpAst_CFileRead: () -> ConstAst_Const in
con IOAst_CReadBytesAsString: () -> ConstAst_Const in
con IOAst_CFlushStdout: () -> ConstAst_Const in
con IOAst_CFlushStderr: () -> ConstAst_Const in
con IOAst_CPrintError: () -> ConstAst_Const in
con IOAst_CReadLine: () -> ConstAst_Const in
con IOAst_CDPrint: () -> ConstAst_Const in
con IOAst_CPrint: () -> ConstAst_Const in
con RandomNumberGeneratorAst_CRandSetSeed: () -> ConstAst_Const in
con RandomNumberGeneratorAst_CRandIntU: () -> ConstAst_Const in
con SysAst_CCommand: () -> ConstAst_Const in
con SysAst_CError: () -> ConstAst_Const in
con SysAst_CExit: () -> ConstAst_Const in
con SysAst_CArgv: () -> ConstAst_Const in
con TimeAst_CWallTimeMs: () -> ConstAst_Const in
con TimeAst_CSleepMs: () -> ConstAst_Const in
con ConTagAst_CConstructorTag: () -> ConstAst_Const in
con RefOpAst_CModRef: () -> ConstAst_Const in
con RefOpAst_CDeRef: () -> ConstAst_Const in
con RefOpAst_CRef: () -> ConstAst_Const in
con TensorOpAst_CTensorCreateUninitFloat: () -> ConstAst_Const in
con TensorOpAst_CTensorCreateUninitInt: () -> ConstAst_Const in
con TensorOpAst_CTensorTransposeExn: () -> ConstAst_Const in
con TensorOpAst_CTensorLinearSetExn: () -> ConstAst_Const in
con TensorOpAst_CTensorLinearGetExn: () -> ConstAst_Const in
con TensorOpAst_CTensorCreateFloat: () -> ConstAst_Const in
con TensorOpAst_CTensorReshapeExn: () -> ConstAst_Const in
con TensorOpAst_CTensorIterSlice: () -> ConstAst_Const in
con TensorOpAst_CTensorCreateInt: () -> ConstAst_Const in
con TensorOpAst_CTensorToString: () -> ConstAst_Const in
con TensorOpAst_CTensorSliceExn: () -> ConstAst_Const in
con TensorOpAst_CTensorSubExn: () -> ConstAst_Const in
con TensorOpAst_CTensorSetExn: () -> ConstAst_Const in
con TensorOpAst_CTensorGetExn: () -> ConstAst_Const in
con TensorOpAst_CTensorCreate: () -> ConstAst_Const in
con TensorOpAst_CTensorShape: () -> ConstAst_Const in
con TensorOpAst_CTensorRank: () -> ConstAst_Const in
con TensorOpAst_CTensorCopy: () -> ConstAst_Const in
con TensorOpAst_CTensorEq: () -> ConstAst_Const in
con BootParserAst_CBootParserParseMLangString: () -> ConstAst_Const in
con BootParserAst_CBootParserParseMExprString: () -> ConstAst_Const in
con BootParserAst_CBootParserParseMLangFile: () -> ConstAst_Const in
con BootParserAst_CBootParserParseMCoreFile: () -> ConstAst_Const in
con BootParserAst_CBootParserGetListLength: () -> ConstAst_Const in
con BootParserAst_CBootParserGetString: () -> ConstAst_Const in
con BootParserAst_CBootParserGetFloat: () -> ConstAst_Const in
con BootParserAst_CBootParserGetConst: () -> ConstAst_Const in
con BootParserAst_CBootParserGetType: () -> ConstAst_Const in
con BootParserAst_CBootParserGetTerm: () -> ConstAst_Const in
con BootParserAst_CBootParserGetInfo: () -> ConstAst_Const in
con BootParserAst_CBootParserGetDecl: () -> ConstAst_Const in
con BootParserAst_CBootParserGetTop: () -> ConstAst_Const in
con BootParserAst_CBootParserGetPat: () -> ConstAst_Const in
con BootParserAst_CBootParserGetInt: () -> ConstAst_Const in
con BootParserAst_CBootParserGetId: () -> ConstAst_Const in
type PatName
in
con PName: Name -> PatName in
con PWildcard: () -> PatName in
con NamedPat_PatNamed: {ty: Ast_Type, info: Info, ident: PatName} -> Ast_Pat in
con SeqTotPat_PatSeqTot: {ty: Ast_Type, info: Info, pats: [Ast_Pat]} -> Ast_Pat in
con SeqEdgePat_PatSeqEdge: {ty: Ast_Type, info: Info, middle: PatName, prefix: [Ast_Pat], postfix: [Ast_Pat]} -> Ast_Pat in
con RecordPat_PatRecord: {ty: Ast_Type, info: Info, bindings: Map SID Ast_Pat} -> Ast_Pat in
con DataPat_PatCon: {ty: Ast_Type, info: Info, ident: Name, subpat: Ast_Pat} -> Ast_Pat in
con IntPat_PatInt: {ty: Ast_Type, val: Int, info: Info} -> Ast_Pat in
con CharPat_PatChar: {ty: Ast_Type, val: Char, info: Info} -> Ast_Pat in
con BoolPat_PatBool: {ty: Ast_Type, val: Bool, info: Info} -> Ast_Pat in
con AndPat_PatAnd: {ty: Ast_Type, info: Info, lpat: Ast_Pat, rpat: Ast_Pat} -> Ast_Pat in
con OrPat_PatOr: {ty: Ast_Type, info: Info, lpat: Ast_Pat, rpat: Ast_Pat} -> Ast_Pat in
con NotPat_PatNot: {ty: Ast_Type, info: Info, subpat: Ast_Pat} -> Ast_Pat in
con UnknownTypeAst_TyUnknown: {info: Info} -> Ast_Type in
con BoolTypeAst_TyBool: {info: Info} -> Ast_Type in
con IntTypeAst_TyInt: {info: Info} -> Ast_Type in
con FloatTypeAst_TyFloat: {info: Info} -> Ast_Type in
con CharTypeAst_TyChar: {info: Info} -> Ast_Type in
con FunTypeAst_TyArrow: {to: Ast_Type, from: Ast_Type, info: Info} -> Ast_Type in
con SeqTypeAst_TySeq: {ty: Ast_Type, info: Info} -> Ast_Type in
con TensorTypeAst_TyTensor: {ty: Ast_Type, info: Info} -> Ast_Type in
con RecordTypeAst_TyRecord: {info: Info, fields: Map SID Ast_Type} -> Ast_Type in
con VariantTypeAst_TyVariant: {info: Info, constrs: Map Name Ast_Type} -> Ast_Type in
con ConTypeAst_TyCon: {data: Ast_Type, info: Info, ident: Name} -> Ast_Type in
type DataTypeAst_DataRec =
  {cons: Set Name, info: Info, positive: Bool, universe: Map Name (Set Name)}
in
con DataTypeAst_TyData: DataTypeAst_DataRec -> Ast_Type in
con VarTypeAst_TyVar: {info: Info, ident: Name} -> Ast_Type in
con AllTypeAst_TyAll: {ty: Ast_Type, info: Info, kind: Ast_Kind, ident: Name} -> Ast_Type in
con AppTypeAst_TyApp: {lhs: Ast_Type, rhs: Ast_Type, info: Info} -> Ast_Type in
con AliasTypeAst_TyAlias: {content: Ast_Type, display: Ast_Type} -> Ast_Type in
con PolyKindAst_Poly: () -> Ast_Kind in
con MonoKindAst_Mono: () -> Ast_Kind in
con RecordKindAst_Record: {fields: Map SID Ast_Type} -> Ast_Kind in
con DataKindAst_Data: {types: Map Name {lower: Set Name, upper: Option (Set Name)}} -> Ast_Kind in
type AssocMap k v =
  [(k, v)]
in
type AssocTraits k =
  {eq: k -> k -> Bool}
in
let printLn = lam s.
    (print (concat s "\n"))
    ; flushStdout {}
in
let dprintLn = lam x.
    (dprint x)
    ; printLn "" in
type Highlight
in
con Irrelevant: Info -> Highlight in
con Relevant: Info -> Highlight in
con Added: {content: [Char], ensureSurroundedBySpaces: Bool} -> Highlight in
type HighlightConfig =
  {added: [Char] -> [Char], relevant: [Char] -> [Char], irrelevant: [Char] -> [Char], afterSection: [Char] -> [Char], beforeSection: [Char] -> [Char]}
in
type InnerHighlight
in
con IBefore: () -> InnerHighlight in
con IAfter: () -> InnerHighlight in
con IRelevant: () -> InnerHighlight in
con IIrrelevant: () -> InnerHighlight in
con IAdded: {ensureSurroundedBySpaces: Bool} -> InnerHighlight in
type HPos =
  {col: Int, row: Int}
in
type HInput =
  {pos: HPos, rest: [Char]}
in
let _advanceRow: HPos -> HPos = lam pos.
    { pos with row = addi pos.row 1, col = 0 }
in
let _advanceCol: HPos -> HPos = lam pos.
    { pos with col = addi pos.col 1 }
in
let _hposLessThan: HPos -> HPos -> Bool =
  lam a.
    lam b.
      or (lti a.row b.row) (and (eqi a.row b.row) (lti a.col b.col))
in
let _advanceInput: HInput -> Option (Char, HInput) =
  lam input.
    let #var"X" = input.rest in
    match
      #var"X"
    with
      "\n" ++ rest
    then
      Some
        ('\n', { pos = _advanceRow input.pos, rest = rest })
    else match
      #var"X"
    with
      [ c ] ++ rest
    then
      Some
        (c, { pos = _advanceCol input.pos, rest = rest })
    else match
      #var"X"
    with
      ""
    in
    None
        {}
in
let _splitInput: HPos -> HInput -> ([Char], HInput) =
  lam target.
    lam input.
      recursive
        let work =
          lam acc.
            lam input: HInput.
              match
                _hposLessThan input.pos target
              with
                true
              then
                match
                  _advanceInput input
                with
                  Some (c, input)
                then
                  work (snoc acc c) input
                else
                  (acc, input)
              else
                (acc, input)
      in
      work "" input
in
let _getRange: Highlight -> Option (HPos, HPos) =
  lam h.
    let #var"X" = h in
    match
      #var"X"
    with
      Irrelevant (Info x)
    then
      Some
        ({ col = x.col1, row = x.row1 }, { col = x.col2, row = x.row2 })
    else match
      #var"X"
    with
      Relevant (Info x)
    then
      Some
        ({ col = x.col1, row = x.row1 }, { col = x.col2, row = x.row2 })
    else match
      #var"X"
    with
      Added _
    then
      None
        {}
    else match
      #var"X"
    with
      _
    in
    (printLn
           "WARNING: (implementation error) missing info field in _getRange")
      ; None
        {}
in
let formatHighlights: HighlightConfig -> [Char] -> [Highlight] -> [Char] =
  lam config.
    lam content.
      lam highlights.
        let contentTooShort =
          lam #var"".
            error
              "The file isn\'t long enough, some of the highlight is outside"
        in
        let input: HInput = { pos = { col = 0, row = 1 }, rest = content }
        in
        let startPos: HPos =
          match
            findMap _getRange highlights
          with
            Some (startPos, _)
          then
            startPos
          else
            error "This highlight list doesn\'t have any info fields in it"
        in
        let endPos: HPos =
          match
            findMap _getRange (reverse highlights)
          with
            Some (_, endPos)
          then
            endPos
          else
            error "This highlight list doesn\'t have any info fields in it"
        in
        match
          _splitInput { startPos with col = 0 } input
        with
          (_, input)
        in
        match
            _splitInput startPos input
          with
            (before, input)
          in
          let sections = [ (before, IBefore
                  {}) ] in
            recursive
              let work =
                lam sections.
                  lam input.
                    lam highlights.
                      match
                        highlights
                      with
                        [ h ] ++ highlights
                      then
                        match
                          _getRange h
                        with
                          Some (startPos, endPos)
                        then
                          match
                            _splitInput startPos input
                          with
                            (irr, input)
                          in
                          match
                              _splitInput endPos input
                            with
                              (sec, input)
                            in
                            let label =
                                let #var"X" = h in
                                match
                                  #var"X"
                                with
                                  Relevant _
                                then
                                  IRelevant
                                    {}
                                else match
                                  #var"X"
                                with
                                  Irrelevant _
                                then
                                  IIrrelevant
                                    {}
                                else match
                                  #var"X"
                                with
                                  _
                                in
                                error "impossible"
                              in
                              work
                                (concat
                                   sections
                                   [ (irr, IIrrelevant
                                       {}),
                                     (sec, label) ])
                                input
                                highlights
                        else match
                          h
                        with
                          Added x
                        then
                          work
                            (snoc
                               sections
                               (x.content, IAdded
                                 { ensureSurroundedBySpaces = x.ensureSurroundedBySpaces }))
                            input
                            highlights
                        else
                          work
                            (snoc
                               sections
                               ("<NoInfo>", IAdded
                                 { ensureSurroundedBySpaces = true }))
                            input
                            highlights
                      else
                        (sections, input)
            in
            match
              work sections input highlights
            with
              (sections, input)
            in
            match
                _splitInput (_advanceRow endPos) input
              with
                (after, _)
              in
              let after =
                  match
                    after
                  with
                    after ++ "\n"
                  then
                    after
                  else
                    after
                in
                let sections = snoc sections (after, IAfter
                      {})
                in
                let sections =
                  filter
                    (lam x.
                       match
                         x
                       with
                         ([ _ ] ++ _, _)
                       then
                         true
                       else
                         false)
                    sections
                in
                recursive
                  let work =
                    lam acc.
                      lam needsPreSpace.
                        lam sections.
                          match
                            sections
                          with
                            [ (content, label) ] ++ sections
                          then
                            let needsPadding =
                              match
                                label
                              with
                                IAdded {ensureSurroundedBySpaces = true}
                              then
                                true
                              else
                                false
                            in
                            let needsPostSpace =
                              match
                                sections
                              with
                                [ ([ c ] ++ _, _) ] ++ _
                              then
                                match
                                  isWhitespace c
                                with
                                  true
                                then
                                  false
                                else
                                  true
                              else
                                false
                            in
                            let pre =
                              match
                                and needsPadding needsPreSpace
                              with
                                true
                              then
                                config.irrelevant " "
                              else
                                ""
                            in
                            let post =
                              match
                                and needsPadding needsPostSpace
                              with
                                true
                              then
                                config.irrelevant " "
                              else
                                ""
                            in
                            let f =
                              let #var"X" = label in
                              match
                                #var"X"
                              with
                                IBefore _
                              then
                                config.beforeSection
                              else match
                                #var"X"
                              with
                                IAfter _
                              then
                                config.afterSection
                              else match
                                #var"X"
                              with
                                IRelevant _
                              then
                                config.relevant
                              else match
                                #var"X"
                              with
                                IIrrelevant _
                              then
                                config.irrelevant
                              else match
                                #var"X"
                              with
                                IAdded _
                              in
                              config.added
                            in
                            let nextNeedsPreSpace =
                              match
                                concat content post
                              with
                                _ ++ [ c ]
                              then
                                match
                                  isWhitespace c
                                with
                                  true
                                then
                                  false
                                else
                                  true
                              else
                                error "impossible"
                            in
                            work
                              (join
                                 [ acc,
                                   pre,
                                   f content,
                                   post ])
                              nextNeedsPreSpace
                              sections
                          else
                            acc
                in
                work "" false sections
in
let terminalHighlightAddedConfig: HighlightConfig =
  { added = lam str.
        concat (concat "[31m" str) "[0m",
    relevant = lam str.
        concat (concat "[37m" str) "[0m",
    irrelevant = lam str.
        concat "[0m" str,
    afterSection = lam str.
        concat "[0m" str,
    beforeSection = lam str.
        concat "[0m" str }
in
let terminalHighlightErrorConfig: HighlightConfig =
  { added = lam str.
        concat (concat "[31m" str) "[0m",
    relevant = lam str.
        concat (concat "[31m" str) "[0m",
    irrelevant = lam str.
        concat "[0m" str,
    afterSection = lam str.
        concat "[0m" str,
    beforeSection = lam str.
        concat "[0m" str }
in
type ErrorSection =
  {msg: [Char], info: Info, infos: [Info], multi: [Char]}
in
let errorDefault: ErrorSection =
  { info = NoInfo
        {}, msg = "", infos = "", multi = "" }
in
let _cachedContent: Ref (Map [Char] [Char]) = ref (mapEmpty cmpString)
in
let _readContent: [Char] -> Option [Char] =
  lam filename.
    match
      mapLookup filename (deref _cachedContent)
    with
      c & Some content
    then
      c
    else match
      fileExists filename
    with
      true
    then
      let content = readFile filename in
      (modref
           _cachedContent
           (mapInsert filename content (deref _cachedContent)))
      ; Some
        content
    else
      None
        {}
in
let _emptyOrNewlineTerm: [Char] -> [Char] =
  lam str.
    let #var"X" = str in
    match
      #var"X"
    with
      ""
    then
      str
    else match
      #var"X"
    with
      _ ++ "\n"
    then
      str
    else match
      #var"X"
    with
      str
    in
    snoc str '\n'
in
let _highlightSection: ErrorSection -> (Info, [Char]) =
  lam section.
    let info =
      match
        section.info
      with
        NoInfo {}
      then
        foldl mergeInfo (NoInfo
             {}) section.infos
      else section.info
    in
    let infos =
      match
        section.infos
      with
        ""
      then
        [ section.info ]
      else section.infos
    in
    let infos = map (lam x.
           Relevant
             x) infos
    in
    let infos =
      match
        section.info
      with
        Info x
      then
        let first = infoVal x.filename x.row1 x.col1 x.row1 x.col1 in
        let last = infoVal x.filename x.row2 x.col2 x.row2 x.col2 in
        snoc
          (cons (Irrelevant
                first) infos)
          (Irrelevant
             last)
      else
        infos
    in
    let msg =
      match
        section.infos
      with
        !"" & ![ _ ]
      then
        match
          section.multi
        with
          !""
        then
          section.multi
        else section.msg
      else section.msg
    in
    let msg = _emptyOrNewlineTerm msg in
    let msg =
      match
        info
      with
        Info {filename = filename}
      then
        match
          _readContent filename
        with
          Some content
        then
          concat
            msg
            (formatHighlights terminalHighlightErrorConfig content infos)
        else
          join
            [ msg,
              "<Couldn\'t read \'",
              filename,
              "\', no highlight available>" ]
      else
        msg
    in
    (info, _emptyOrNewlineTerm msg)
in
let errorMsg: [ErrorSection] -> {multi: [Char], single: [Char]} -> (Info, [Char]) =
  lam sections.
    lam msg.
      let #var"X" = map _highlightSection sections in
      match
        #var"X"
      with
        [ (info, inner) ]
      then
        (info, concat (_emptyOrNewlineTerm msg.single) inner)
      else match
        #var"X"
      with
        sections
      in
      let msg =
          match
            msg.multi
          with
            !""
          then
            msg.multi
          else msg.single
        in
        match
          unzip sections
        with
          (infos, inners)
        in
        let info = foldl mergeInfo (NoInfo
                 {}) infos
          in
          let msg = strJoin "\n" (cons (_emptyOrNewlineTerm msg) inners)
          in
          (info, msg)
in
let _partitionInfosByFile: [Info] -> [[Info]] =
  lam infos.
    recursive
      let work =
        lam acc.
          lam info.
            match
              info
            with
              Info x
            then
              mapInsertWith concat x.filename [ info ] acc
            else
              acc
    in
    mapValues (foldl work (mapEmpty cmpString) infos)
in
let _die: all a. (Info, [Char]) -> a =
  lam msg.
    (printError
         (join
            [ "\n",
              infoErrorString msg.0 msg.1,
              "\n" ]))
    ; (flushStderr {})
    ; exit 1
in
let _single =
  lam f.
    lam infos.
      lam msg.
        let mkSection = lam infos.
            { errorDefault with infos = infos }
        in
        f
          (errorMsg
             (map mkSection (_partitionInfosByFile infos))
             { multi = "", single = msg })
in
let errorSingle: all a. [Info] -> [Char] -> a = lam x.
    _single _die x
in
let _multi =
  lam f.
    lam sections.
      lam msg.
        f
          (errorMsg
             (map
                (lam sec.
                   { errorDefault with info = sec.0, msg = sec.1 })
                sections)
             { multi = "", single = msg })
in
let errorMulti: all a. [(Info, [Char])] -> [Char] -> a = lam x.
    _multi _die x
in
let tyint_ = IntTypeAst_TyInt
    { info = NoInfo
          {} }
in
let tyfloat_ = FloatTypeAst_TyFloat
    { info = NoInfo
          {} }
in
let tybool_ = BoolTypeAst_TyBool
    { info = NoInfo
          {} }
in
let tychar_ = CharTypeAst_TyChar
    { info = NoInfo
          {} }
in
let tyunknown_ = UnknownTypeAst_TyUnknown
    { info = NoInfo
          {} }
in
let ityarrow_ =
  lam info.
    lam from.
      lam to.
        FunTypeAst_TyArrow
          { info = info, to = to, from = from }
in
let tyarrow_ =
  lam from.
    lam to.
      ityarrow_ (NoInfo
           {}) from to
in
let tyarrows_ =
  lam tys.
    foldr1
      (lam e.
         lam acc.
           FunTypeAst_TyArrow
             { info = NoInfo
                   {}, to = acc, from = e })
      tys
in
let tyRecord: Info -> [([Char], Ast_Type)] -> Ast_Type =
  lam info.
    lam fields.
      let fieldMapFunc = lam b: ([Char], Ast_Type).
          (stringToSid b.0, b.1)
      in
      RecordTypeAst_TyRecord
        { info = info,
          fields = mapFromSeq cmpSID (map fieldMapFunc fields) }
in
let tyrecord_ = tyRecord (NoInfo
       {}) in
let tytuple_ =
  lam tys.
    tyrecord_
      (mapi
         (lam i.
            lam ty.
              (int2string i, ty))
         tys)
in
let tyunit_ = tyrecord_ "" in
let tyvariant_ =
  lam constrs.
    VariantTypeAst_TyVariant
      { info = NoInfo
            {},
        constrs = mapFromSeq nameCmp constrs }
in
recursive
  let bindF_ =
    lam f: Ast_Expr -> Ast_Expr -> Ast_Expr.
      lam letexpr.
        lam expr.
          match
            letexpr
          with
            LetAst_TmLet t
          then
            LetAst_TmLet
              { t with inexpr = bindF_ f t.inexpr expr }
          else match
            letexpr
          with
            RecLetsAst_TmRecLets t
          then
            RecLetsAst_TmRecLets
              { t with inexpr = bindF_ f t.inexpr expr }
          else match
            letexpr
          with
            DataAst_TmConDef t
          then
            DataAst_TmConDef
              { t with inexpr = bindF_ f t.inexpr expr }
          else match
            letexpr
          with
            TypeAst_TmType t
          then
            TypeAst_TmType
              { t with inexpr = bindF_ f t.inexpr expr }
          else match
            letexpr
          with
            ExtAst_TmExt t
          then
            ExtAst_TmExt
              { t with inexpr = bindF_ f t.inexpr expr }
          else match
            letexpr
          with
            UtestAst_TmUtest t
          then
            UtestAst_TmUtest
              { t with next = bindF_ f t.next expr }
          else
            f letexpr expr
in
let bind_ = bindF_ (lam #var"".
       lam expr.
         expr)
in
let uunit_ =
  RecordAst_TmRecord
    { ty = tyunknown_,
      info = NoInfo
          {},
      bindings = mapEmpty cmpSID }
in
let unit_ =
  RecordAst_TmRecord
    { ty = tyunit_,
      info = NoInfo
          {},
      bindings = mapEmpty cmpSID }
in
let nlet_ =
  lam n.
    lam ty.
      lam body.
        LetAst_TmLet
          { ty = tyunknown_,
            info = NoInfo
                {},
            ident = n,
            body = body,
            tyAnnot = ty,
            inexpr = uunit_,
            tyBody = ty }
in
let let_ =
  lam s.
    lam ty.
      lam body.
        nlet_ (nameNoSym s) ty body
in
let ulet_ = lam s.
    lam body.
      let_ s tyunknown_ body
in
let nreclets_ =
  lam bs.
    let bindingMapFunc =
      lam t: (Name, Ast_Type, Ast_Expr).
        { info = NoInfo
              {},
          ident = t.0,
          body = t.2,
          tyAnnot = t.1,
          tyBody = t.1 }
    in
    RecLetsAst_TmRecLets
      { ty = tyunknown_,
        info = NoInfo
            {},
        inexpr = uunit_,
        bindings = map bindingMapFunc bs }
in
let reclets_ =
  lam bs.
    let bindingMapFunc =
      lam b: ([Char], Ast_Type, Ast_Expr).
        (nameNoSym b.0, b.1, b.2)
    in
    nreclets_ (map bindingMapFunc bs)
in
let reclets_empty = reclets_ "" in
let const_ =
  lam ty.
    lam c.
      ConstAst_TmConst
        { ty = ty, info = NoInfo
              {}, val = c }
in
let uconst_ = const_ tyunknown_ in
let record_empty = unit_ in
let semi_ = lam expr1.
    lam expr2.
      bind_ (ulet_ "" expr1) expr2
in
let bindSemi_ = bindF_ semi_ in
type List a
in
con Cons: all a. (a, List a) -> List a in
con Nil: all a. () -> List a in
let builtin =
  [ ("unsafeCoerce", UnsafeCoerceAst_CUnsafeCoerce
      {}),
    ("addi", ArithIntAst_CAddi
      {}),
    ("subi", ArithIntAst_CSubi
      {}),
    ("muli", ArithIntAst_CMuli
      {}),
    ("divi", ArithIntAst_CDivi
      {}),
    ("modi", ArithIntAst_CModi
      {}),
    ("negi", ArithIntAst_CNegi
      {}),
    ("lti", CmpIntAst_CLti
      {}),
    ("leqi", CmpIntAst_CLeqi
      {}),
    ("gti", CmpIntAst_CGti
      {}),
    ("geqi", CmpIntAst_CGeqi
      {}),
    ("eqi", CmpIntAst_CEqi
      {}),
    ("neqi", CmpIntAst_CNeqi
      {}),
    ("slli", ShiftIntAst_CSlli
      {}),
    ("srli", ShiftIntAst_CSrli
      {}),
    ("srai", ShiftIntAst_CSrai
      {}),
    ("addf", ArithFloatAst_CAddf
      {}),
    ("subf", ArithFloatAst_CSubf
      {}),
    ("mulf", ArithFloatAst_CMulf
      {}),
    ("divf", ArithFloatAst_CDivf
      {}),
    ("negf", ArithFloatAst_CNegf
      {}),
    ("ltf", CmpFloatAst_CLtf
      {}),
    ("leqf", CmpFloatAst_CLeqf
      {}),
    ("gtf", CmpFloatAst_CGtf
      {}),
    ("geqf", CmpFloatAst_CGeqf
      {}),
    ("eqf", CmpFloatAst_CEqf
      {}),
    ("neqf", CmpFloatAst_CNeqf
      {}),
    ("floorfi", FloatIntConversionAst_CFloorfi
      {}),
    ("ceilfi", FloatIntConversionAst_CCeilfi
      {}),
    ("roundfi", FloatIntConversionAst_CRoundfi
      {}),
    ("int2float", FloatIntConversionAst_CInt2float
      {}),
    ("stringIsFloat", FloatStringConversionAst_CStringIsFloat
      {}),
    ("string2float", FloatStringConversionAst_CString2float
      {}),
    ("float2string", FloatStringConversionAst_CFloat2string
      {}),
    ("eqc", CmpCharAst_CEqc
      {}),
    ("char2int", IntCharConversionAst_CChar2Int
      {}),
    ("int2char", IntCharConversionAst_CInt2Char
      {}),
    ("create", SeqOpAst_CCreate
      {}),
    ("createList", SeqOpAst_CCreateList
      {}),
    ("createRope", SeqOpAst_CCreateRope
      {}),
    ("isList", SeqOpAst_CIsList
      {}),
    ("isRope", SeqOpAst_CIsRope
      {}),
    ("length", SeqOpAst_CLength
      {}),
    ("concat", SeqOpAst_CConcat
      {}),
    ("get", SeqOpAst_CGet
      {}),
    ("set", SeqOpAst_CSet
      {}),
    ("cons", SeqOpAst_CCons
      {}),
    ("snoc", SeqOpAst_CSnoc
      {}),
    ("splitAt", SeqOpAst_CSplitAt
      {}),
    ("reverse", SeqOpAst_CReverse
      {}),
    ("head", SeqOpAst_CHead
      {}),
    ("tail", SeqOpAst_CTail
      {}),
    ("null", SeqOpAst_CNull
      {}),
    ("map", SeqOpAst_CMap
      {}),
    ("mapi", SeqOpAst_CMapi
      {}),
    ("iter", SeqOpAst_CIter
      {}),
    ("iteri", SeqOpAst_CIteri
      {}),
    ("foldl", SeqOpAst_CFoldl
      {}),
    ("foldr", SeqOpAst_CFoldr
      {}),
    ("subsequence", SeqOpAst_CSubsequence
      {}),
    ("randIntU", RandomNumberGeneratorAst_CRandIntU
      {}),
    ("randSetSeed", RandomNumberGeneratorAst_CRandSetSeed
      {}),
    ("wallTimeMs", TimeAst_CWallTimeMs
      {}),
    ("sleepMs", TimeAst_CSleepMs
      {}),
    ("print", IOAst_CPrint
      {}),
    ("printError", IOAst_CPrintError
      {}),
    ("dprint", IOAst_CDPrint
      {}),
    ("flushStdout", IOAst_CFlushStdout
      {}),
    ("flushStderr", IOAst_CFlushStderr
      {}),
    ("readLine", IOAst_CReadLine
      {}),
    ("readBytesAsString", IOAst_CReadBytesAsString
      {}),
    ("argv", SysAst_CArgv
      {}),
    ("readFile", FileOpAst_CFileRead
      {}),
    ("writeFile", FileOpAst_CFileWrite
      {}),
    ("fileExists", FileOpAst_CFileExists
      {}),
    ("deleteFile", FileOpAst_CFileDelete
      {}),
    ("command", SysAst_CCommand
      {}),
    ("error", SysAst_CError
      {}),
    ("exit", SysAst_CExit
      {}),
    ("constructorTag", ConTagAst_CConstructorTag
      {}),
    ("eqsym", CmpSymbAst_CEqsym
      {}),
    ("gensym", SymbAst_CGensym
      {}),
    ("sym2hash", SymbAst_CSym2hash
      {}),
    ("ref", RefOpAst_CRef
      {}),
    ("deref", RefOpAst_CDeRef
      {}),
    ("modref", RefOpAst_CModRef
      {}),
    ("tensorCreateUninitInt", TensorOpAst_CTensorCreateUninitInt
      {}),
    ("tensorCreateUninitFloat", TensorOpAst_CTensorCreateUninitFloat
      {}),
    ("tensorCreateCArrayInt", TensorOpAst_CTensorCreateInt
      {}),
    ("tensorCreateCArrayFloat", TensorOpAst_CTensorCreateFloat
      {}),
    ("tensorCreateDense", TensorOpAst_CTensorCreate
      {}),
    ("tensorGetExn", TensorOpAst_CTensorGetExn
      {}),
    ("tensorSetExn", TensorOpAst_CTensorSetExn
      {}),
    ("tensorLinearGetExn", TensorOpAst_CTensorLinearGetExn
      {}),
    ("tensorLinearSetExn", TensorOpAst_CTensorLinearSetExn
      {}),
    ("tensorRank", TensorOpAst_CTensorRank
      {}),
    ("tensorShape", TensorOpAst_CTensorShape
      {}),
    ("tensorReshapeExn", TensorOpAst_CTensorReshapeExn
      {}),
    ("tensorCopy", TensorOpAst_CTensorCopy
      {}),
    ("tensorTransposeExn", TensorOpAst_CTensorTransposeExn
      {}),
    ("tensorSliceExn", TensorOpAst_CTensorSliceExn
      {}),
    ("tensorSubExn", TensorOpAst_CTensorSubExn
      {}),
    ("tensorIterSlice", TensorOpAst_CTensorIterSlice
      {}),
    ("tensorEq", TensorOpAst_CTensorEq
      {}),
    ("tensor2string", TensorOpAst_CTensorToString
      {}),
    ("bootParserParseMExprString", BootParserAst_CBootParserParseMExprString
      {}),
    ("bootParserParseMLangString", BootParserAst_CBootParserParseMLangString
      {}),
    ("bootParserParseMLangFile", BootParserAst_CBootParserParseMLangFile
      {}),
    ("bootParserParseMCoreFile", BootParserAst_CBootParserParseMCoreFile
      {}),
    ("bootParserGetId", BootParserAst_CBootParserGetId
      {}),
    ("bootParserGetTerm", BootParserAst_CBootParserGetTerm
      {}),
    ("bootParserGetTop", BootParserAst_CBootParserGetTop
      {}),
    ("bootParserGetDecl", BootParserAst_CBootParserGetDecl
      {}),
    ("bootParserGetType", BootParserAst_CBootParserGetType
      {}),
    ("bootParserGetString", BootParserAst_CBootParserGetString
      {}),
    ("bootParserGetInt", BootParserAst_CBootParserGetInt
      {}),
    ("bootParserGetFloat", BootParserAst_CBootParserGetFloat
      {}),
    ("bootParserGetListLength", BootParserAst_CBootParserGetListLength
      {}),
    ("bootParserGetConst", BootParserAst_CBootParserGetConst
      {}),
    ("bootParserGetPat", BootParserAst_CBootParserGetPat
      {}),
    ("bootParserGetInfo", BootParserAst_CBootParserGetInfo
      {}) ]
in
let builtinTypes: [([Char], [[Char]])] =
  [ ("Symbol", ""),
    ("Ref", [ "a" ]),
    ("BootParseTree", "") ]
in
type OpCost =
  Float
in
type ReprContent
in
type ReprVar =
  Ref ReprContent
in
con UninitRepr: () -> ReprContent in
con BotRepr: {sym: Symbol, scope: Int} -> ReprContent in
con LinkRepr: ReprVar -> ReprContent in
con TyWildAst_TyWild: {info: Info} -> Ast_Type in
con ReprTypeAst_TyRepr: {arg: Ast_Type, info: Info, repr: ReprVar} -> Ast_Type in
con ReprSubstAst_TySubst: {arg: Ast_Type, info: Info, subst: Name} -> Ast_Type in
con OpDeclAst_TmOpDecl: {ty: Ast_Type, info: Info, ident: Name, inexpr: Ast_Expr, tyAnnot: Ast_Type} -> Ast_Expr in
type ImplId =
  Int
in
type OpImplAst_TmOpImplRec =
  {ty: Ast_Type, body: Ast_Expr, info: Info, ident: Name, implId: ImplId, inexpr: Ast_Expr, selfCost: OpCost, specType: Ast_Type, metaLevel: Int, reprScope: Int, delayedReprUnifications: [(ReprVar, ReprVar)]}
in
con OpImplAst_TmOpImpl: OpImplAst_TmOpImplRec -> Ast_Expr in
type OpVarAst_TmOpVarRec =
  {ty: Ast_Type, info: Info, ident: Name, frozen: Bool, scaling: OpCost}
in
con OpVarAst_TmOpVar: OpVarAst_TmOpVarRec -> Ast_Expr in
con ReprDeclAst_TmReprDecl: {ty: Ast_Type, pat: Ast_Type, info: Info, repr: Ast_Type, vars: [Name], ident: Name, inexpr: Ast_Expr} -> Ast_Expr in
type CollectedImpl =
  {body: Ast_Expr, info: Info, selfCost: OpCost, specType: Ast_Type}
in
type ImplData =
  {impls: Map SID [CollectedImpl], reprs: Map Name {pat: Ast_Type, repr: Ast_Type, vars: [Name]}}
in
let emptyImplData: ImplData = { impls = mapEmpty cmpSID, reprs = mapEmpty nameCmp }
in
let recordOrderedLabels =
  lam labels: [SID].
    let isTupleLabel =
      lam sid.
        let l = sidToString sid in
        match
          null l
        with
          true
        then
          false
        else match
          eqChar (get l 0) '0'
        with
          true
        then
          eqi (length l) 1
        else
          forAll
            (lam c.
               and (geqChar c '0') (leqChar c '9'))
            l
    in
    let cmpLabel =
      lam a.
        lam b.
          cmpString (sidToString a) (sidToString b)
    in
    let sortLabel = quickSort cmpLabel in
    match
      partition isTupleLabel labels
    with
      (tupLabels, recLabels)
    in
    concat (sortLabel tupLabels) (sortLabel recLabels)
in
type PprintEnv =
  {count: Map [Char] Int, nameMap: Map Name [Char], strings: Set [Char], optSingleLineLimit: Int, optCompactMatchElse: Bool, optSingleLineConstSeq: Bool, optCompactRecordUpdate: Bool}
in
let pprintEnvEmpty =
  { count = mapEmpty cmpString,
    nameMap = mapEmpty nameCmp,
    strings = setEmpty cmpString,
    optSingleLineLimit = 60,
    optCompactMatchElse = true,
    optSingleLineConstSeq = true,
    optCompactRecordUpdate = true }
in
let pprintEnvLookup: Name -> PprintEnv -> Option [Char] =
  lam name.
    lam env: PprintEnv.
      match
        env
      with
        {nameMap = nameMap}
      in
      mapLookup name nameMap
in
let pprintEnvFree: [Char] -> PprintEnv -> Bool =
  lam str.
    lam env: PprintEnv.
      match
        env
      with
        {strings = strings}
      in
      not (setMem str strings)
in
let pprintEnvAdd: Name -> [Char] -> Int -> PprintEnv -> PprintEnv =
  lam name.
    lam str.
      lam i.
        lam env: PprintEnv.
          match
            env
          with
            {count = count, nameMap = nameMap, strings = strings}
          in
          let baseStr = nameGetStr name in
            let count = mapInsert baseStr i count in
            let nameMap = mapInsert name str nameMap in
            let strings = setInsert str strings in
            { env with nameMap = nameMap, count = count, strings = strings }
in
let _parserStr =
  lam str.
    lam prefix.
      lam cond.
        match
          eqi (length str) 0
        with
          true
        then
          concat prefix "\"\""
        else match
          cond str
        with
          true
        then
          str
        else
          join
            [ prefix,
              "\"",
              str,
              "\"" ]
in
let _isValidIdentContents =
  lam str.
    forAll (lam c.
         or (isAlphanum c) (eqc c '_')) str
in
let _isValidLowerIdent =
  lam str.
    match
      str
    with
      [ x ]
    then
      isLowerAlpha x
    else match
      isLowerAlphaOrUnderscore (head str)
    with
      true
    then
      _isValidIdentContents (tail str)
    else
      false
in
let pprintVarString = lam str.
    _parserStr str "#var" _isValidLowerIdent
in
let pprintConString =
  lam str.
    _parserStr str "#con" (lam str.
         isUpperAlpha (head str))
in
let pprintTypeString =
  lam str.
    _parserStr str "#type" (lam str.
         isUpperAlpha (head str))
in
type NameEnv =
  {conEnv: Map [Char] Name, varEnv: Map [Char] Name, reprEnv: Map [Char] Name, tyConEnv: Map [Char] Name, tyVarEnv: Map [Char] Name}
in
let _nameEnvEmpty: NameEnv =
  { conEnv = mapEmpty cmpString,
    varEnv = mapEmpty cmpString,
    reprEnv = mapEmpty cmpString,
    tyConEnv = mapEmpty cmpString,
    tyVarEnv = mapEmpty cmpString }
in
type SymEnv =
  {langEnv: Map [Char] NameEnv, allowFree: Bool, currentEnv: NameEnv, namespaceEnv: Map [Char] Name, ignoreExternals: Bool}
in
let symbolizeUpdateVarEnv =
  lam env: SymEnv.
    lam varEnv: Map [Char] Name.
      { env with currentEnv = { env.currentEnv with varEnv = varEnv } }
in
let symbolizeUpdateConEnv =
  lam env: SymEnv.
    lam conEnv: Map [Char] Name.
      { env with currentEnv = { env.currentEnv with conEnv = conEnv } }
in
let symbolizeUpdateTyVarEnv =
  lam env: SymEnv.
    lam tyVarEnv: Map [Char] Name.
      { env with currentEnv = { env.currentEnv with tyVarEnv = tyVarEnv } }
in
let symbolizeUpdateTyConEnv =
  lam env: SymEnv.
    lam tyConEnv: Map [Char] Name.
      { env with currentEnv = { env.currentEnv with tyConEnv = tyConEnv } }
in
let _symEnvEmpty: SymEnv =
  { langEnv = mapEmpty cmpString,
    allowFree = false,
    currentEnv = _nameEnvEmpty,
    namespaceEnv = mapEmpty cmpString,
    ignoreExternals = false }
in
let symEnvAddBuiltinTypes: all a. SymEnv -> [([Char], a)] -> SymEnv =
  lam env.
    lam tys.
      symbolizeUpdateTyConEnv
        env
        (foldl
           (lam env.
              lam t.
                mapInsert t.0 (nameNoSym t.0) env)
           env.currentEnv.tyConEnv
           tys)
in
let symEnvDefault = symEnvAddBuiltinTypes _symEnvEmpty builtinTypes
in
let symEnvEmpty = symEnvDefault in
type SymLookup_LookupParams =
  {info: [Info], kind: [Char], allowFree: Bool}
in
recursive
  let vSymLookup_getSymbolWith: all a. all b. {absent: () -> b, hasSym: () -> b, present: a -> b} -> Map [Char] a -> Name -> b =
    lam cases.
      lam env.
        lam __sem_target.
          match
            __sem_target
          with
            ident
          in
          match
              nameHasSym ident
            with
              true
            then
              cases.hasSym {}
            else
              optionMapOrElse
                cases.absent cases.present (mapLookup (nameGetStr ident) env)
in
let _symbolizePatName: Map [Char] Name -> PatName -> (Map [Char] Name, PatName) =
  lam patEnv.
    lam pname.
      match
        pname
      with
        PName name
      then
        vSymLookup_getSymbolWith
          { absent =
              lam #var"".
                let name = nameSetNewSym name in
                (mapInsert (nameGetStr name) name patEnv, PName
                  name),
            hasSym =
              lam #var"".
                (patEnv, PName
                  name),
            present =
              lam name.
                (patEnv, PName
                  name) }
          patEnv
          name
      else
        (patEnv, pname)
in
type AssocSeq k v =
  [(k, v)]
in
type AssocTraits k =
  {eq: k -> k -> Bool}
in
type BiNameMap =
  [(Name, Name)]
in
type EqEnv =
  {conEnv: BiNameMap, varEnv: BiNameMap}
in
type EqTypeEnv =
  {tyVarEnv: BiNameMap}
in
type EqTypeFreeEnv =
  {freeTyFlex: BiNameMap, freeTyVars: BiNameMap}
in
type Eval_EvalEnv =
  List (Name, Ast_Expr)
in
type Eval_EvalCtx =
  {env: Eval_EvalEnv}
in
type ClosAst_Lazy a =
  () -> a
in
con ClosAst_TmClos: {env: ClosAst_Lazy Eval_EvalEnv, body: Ast_Expr, info: Info, ident: Name} -> Ast_Expr in
con ConstAppAst_TmConstApp: {args: [Ast_Expr], info: Info, const: ConstAst_Const} -> Ast_Expr in
con RefEval_TmRef: {ref: Ref Ast_Expr} -> Ast_Expr in
type T
in
con TInt: Tensor[Int] -> T in
con TFloat: Tensor[Float] -> T in
con TExpr: Tensor[Ast_Expr] -> T in
con TensorEval_TmTensor: {val: T} -> Ast_Expr in
con BootParserEval_CBootParserTree: {val: BootParseTree} -> ConstAst_Const in
recursive
  let vMExprCmp_infoTy: Ast_Type -> Info =
    lam __sem_target.
      match
        __sem_target
      with
        UnknownTypeAst_TyUnknown r
      then
        r.info
      else match
        __sem_target
      with
        BoolTypeAst_TyBool r
      then
        r.info
      else match
        __sem_target
      with
        IntTypeAst_TyInt r
      then
        r.info
      else match
        __sem_target
      with
        FloatTypeAst_TyFloat r
      then
        r.info
      else match
        __sem_target
      with
        CharTypeAst_TyChar r
      then
        r.info
      else match
        __sem_target
      with
        FunTypeAst_TyArrow r
      then
        r.info
      else match
        __sem_target
      with
        SeqTypeAst_TySeq r
      then
        r.info
      else match
        __sem_target
      with
        TensorTypeAst_TyTensor r
      then
        r.info
      else match
        __sem_target
      with
        RecordTypeAst_TyRecord r
      then
        r.info
      else match
        __sem_target
      with
        VariantTypeAst_TyVariant r
      then
        r.info
      else match
        __sem_target
      with
        ConTypeAst_TyCon r
      then
        r.info
      else match
        __sem_target
      with
        DataTypeAst_TyData r
      then
        r.info
      else match
        __sem_target
      with
        VarTypeAst_TyVar t
      then
        t.info
      else match
        __sem_target
      with
        AllTypeAst_TyAll t
      then
        t.info
      else match
        __sem_target
      with
        AppTypeAst_TyApp r
      then
        r.info
      else match
        __sem_target
      with
        AliasTypeAst_TyAlias t
      in
      vMExprCmp_infoTy t.display
  let vMExprCmp_cmpKind =
    lam __sem_target.
      match
        __sem_target
      with
        (MonoKindAst_Mono {}, MonoKindAst_Mono {})
      then
        0
      else match
        __sem_target
      with
        (PolyKindAst_Poly {}, PolyKindAst_Poly {})
      then
        0
      else match
        __sem_target
      with
        (RecordKindAst_Record l, RecordKindAst_Record r)
      then
        mapCmp vMExprCmp_cmpType l.fields r.fields
      else match
        __sem_target
      with
        (DataKindAst_Data l, DataKindAst_Data r)
      then
        let recCmp =
          lam r1.
            lam r2.
              let lowerDiff = setCmp r1.lower r2.lower in
              match
                eqi lowerDiff 0
              with
                true
              then
                let #var"X" = (r1.upper, r2.upper) in
                match
                  #var"X"
                with
                  (Some u1, Some u2)
                then
                  setCmp u1 u2
                else match
                  #var"X"
                with
                  (Some _, None _)
                then
                  1
                else match
                  #var"X"
                with
                  (None _, Some _)
                then
                  negi 1
                else match
                  #var"X"
                with
                  _
                in
                0
              else
                lowerDiff
        in
        mapCmp recCmp l.types r.types
      else match
        __sem_target
      with
        (lhs, rhs)
      in
      let res = subi (constructorTag lhs) (constructorTag rhs) in
        match
          eqi res 0
        with
          true
        then
          errorSingle "" "Missing case in cmpKind for types with equal indices."
        else
          res
  let vMExprCmp_cmpType =
    lam lhs: Ast_Type.
      lam __sem_target.
        match
          __sem_target
        with
          rhs
        in
        vMExprCmp_cmpTypeH (vMExprCmp_unwrapType lhs, vMExprCmp_unwrapType rhs)
  let vMExprCmp_cmpTypeH =
    lam __sem_target.
      match
        __sem_target
      with
        (UnknownTypeAst_TyUnknown _, UnknownTypeAst_TyUnknown _)
      then
        0
      else match
        __sem_target
      with
        (BoolTypeAst_TyBool _, BoolTypeAst_TyBool _)
      then
        0
      else match
        __sem_target
      with
        (IntTypeAst_TyInt _, IntTypeAst_TyInt _)
      then
        0
      else match
        __sem_target
      with
        (FloatTypeAst_TyFloat _, FloatTypeAst_TyFloat _)
      then
        0
      else match
        __sem_target
      with
        (CharTypeAst_TyChar _, CharTypeAst_TyChar _)
      then
        0
      else match
        __sem_target
      with
        (FunTypeAst_TyArrow t1, FunTypeAst_TyArrow t2)
      then
        let fromDiff = vMExprCmp_cmpType t1.from t2.from in
        match
          eqi fromDiff 0
        with
          true
        then
          vMExprCmp_cmpType t1.to t2.to
        else
          fromDiff
      else match
        __sem_target
      with
        (SeqTypeAst_TySeq {ty = t1}, SeqTypeAst_TySeq {ty = t2})
      then
        vMExprCmp_cmpType t1 t2
      else match
        __sem_target
      with
        (TensorTypeAst_TyTensor {ty = t1}, TensorTypeAst_TyTensor {ty = t2})
      then
        vMExprCmp_cmpType t1 t2
      else match
        __sem_target
      with
        (RecordTypeAst_TyRecord t1, RecordTypeAst_TyRecord t2)
      then
        mapCmp vMExprCmp_cmpType t1.fields t2.fields
      else match
        __sem_target
      with
        (VariantTypeAst_TyVariant t1, VariantTypeAst_TyVariant t2)
      then
        mapCmp vMExprCmp_cmpType t1.constrs t2.constrs
      else match
        __sem_target
      with
        (ConTypeAst_TyCon t1, ConTypeAst_TyCon t2)
      then
        let nameDiff = nameCmp t1.ident t2.ident in
        match
          eqi nameDiff 0
        with
          true
        then
          vMExprCmp_cmpType t1.data t2.data
        else
          nameDiff
      else match
        __sem_target
      with
        (DataTypeAst_TyData l, DataTypeAst_TyData r)
      then
        mapCmp setCmp (vMExprCmp_computeData l) (vMExprCmp_computeData r)
      else match
        __sem_target
      with
        (VarTypeAst_TyVar t1, VarTypeAst_TyVar t2)
      then
        nameCmp t1.ident t2.ident
      else match
        __sem_target
      with
        (AllTypeAst_TyAll t1, AllTypeAst_TyAll t2)
      then
        let identDiff = nameCmp t1.ident t2.ident in
        match
          eqi identDiff 0
        with
          true
        then
          let kindDiff = vMExprCmp_cmpKind (t1.kind, t2.kind) in
          match
            eqi kindDiff 0
          with
            true
          then
            vMExprCmp_cmpType t1.ty t2.ty
          else
            kindDiff
        else
          identDiff
      else match
        __sem_target
      with
        (AppTypeAst_TyApp t1, AppTypeAst_TyApp t2)
      then
        let lhsDiff = vMExprCmp_cmpType t1.lhs t2.lhs in
        match
          eqi lhsDiff 0
        with
          true
        then
          vMExprCmp_cmpType t1.rhs t2.rhs
        else
          lhsDiff
      else match
        __sem_target
      with
        (AliasTypeAst_TyAlias t1, ty2)
      then
        vMExprCmp_cmpTypeH (t1.content, ty2)
      else match
        __sem_target
      with
        (ty1 & !AliasTypeAst_TyAlias _, AliasTypeAst_TyAlias t2)
      then
        vMExprCmp_cmpTypeH (ty1, t2.content)
      else match
        __sem_target
      with
        (lhs, rhs)
      in
      let res = subi (constructorTag lhs) (constructorTag rhs) in
        match
          eqi res 0
        with
          true
        then
          errorMulti
            [ (vMExprCmp_infoTy lhs, ""),
              (vMExprCmp_infoTy rhs, "") ]
            "Missing case in cmpTypeH for types with equal indices."
        else
          res
  let vMExprCmp_unwrapType: Ast_Type -> Ast_Type =
    lam __sem_target.
      match
        __sem_target
      with
        ty
      in
      vMExprCmp_rapp_Type_Type vMExprCmp_unwrapType ty
  let vMExprCmp_computeData: DataTypeAst_DataRec -> Map Name (Set Name) =
    lam __sem_target.
      match
        __sem_target
      with
        r
      in
      match
          r.positive
        with
          true
        then
          mapMap (setIntersect r.cons) r.universe
        else
          mapMap (lam x.
               setSubtract x r.cons) r.universe
  let vMExprCmp_rapp_Type_Type: (Ast_Type -> Ast_Type) -> Ast_Type -> Ast_Type =
    lam f.
      lam __sem_target.
        match
          __sem_target
        with
          ty
        in
        let res =
            vMExprCmp_rappAccumL_Type_Type
              (lam #var"".
                 lam t.
                   ({}, f t))
              {}
              ty
          in
          res.1
  let vMExprCmp_rappAccumL_Type_Type: all acc. (acc -> Ast_Type -> (acc, Ast_Type)) -> acc -> Ast_Type -> (acc, Ast_Type) =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            AliasTypeAst_TyAlias t
          then
            f acc t.content
          else match
            __sem_target
          with
            ty
          in
          (acc, ty)
in
type TokenReprBase_TokenRepr
in
con TokenReprEOF_EOFRepr: () -> TokenReprBase_TokenRepr in
type ContextFreeGrammar_Term
in
type ContextFreeGrammar_Production =
  {nt: Name, terms: [ContextFreeGrammar_Term], action: Ast_Expr}
in
type ContextFreeGrammar_SyntaxDef =
  {entrypoint: Name, productions: [ContextFreeGrammar_Production], initActionState: Ast_Expr}
in
con ContextFreeGrammar_NonTerminal: Name -> ContextFreeGrammar_Term in
con ContextFreeGrammar_Terminal: TokenReprBase_TokenRepr -> ContextFreeGrammar_Term in
con GrammarTestLanguage_RParenRepr: () -> TokenReprBase_TokenRepr in
con GrammarTestLanguage_LParenRepr: () -> TokenReprBase_TokenRepr in
con GrammarTestLanguage_TimesRepr: () -> TokenReprBase_TokenRepr in
con GrammarTestLanguage_PlusRepr: () -> TokenReprBase_TokenRepr in
con GrammarTestLanguage_IntRepr: () -> TokenReprBase_TokenRepr in
let tabSpace = 2 in
type TokenParser_Token
in
type TokenParser_Stream =
  {pos: Pos, str: [Char]}
in
type TokenParser_NextTokenResult =
  {lit: [Char], info: Info, token: TokenParser_Token, stream: TokenParser_Stream}
in
con ErrorTokenParser_ErrorRepr: () -> TokenReprBase_TokenRepr in
con ErrorTokenParser_ErrorTok: {char: Char, info: Info} -> TokenParser_Token in
con EOFTokenParser_EOFTok: {info: Info} -> TokenParser_Token in
let parseIdentCont: Pos -> [Char] -> {pos: Pos, str: [Char], val: [Char]} =
  lam p.
    lam str.
      recursive
        let work =
          lam acc.
            lam p.
              lam str.
                match
                  str
                with
                  [ x ] ++ xs
                then
                  match
                    or (isAlphanum x) (eqChar '_' x)
                  with
                    true
                  then
                    work (snoc acc x) (advanceCol p 1) xs
                  else
                    { val = acc, pos = p, str = str }
                else
                  { val = acc, pos = p, str = str }
      in
      work "" p str
in
con LIdentTokenParser_LIdentRepr: () -> TokenReprBase_TokenRepr in
con LIdentTokenParser_LIdentTok: {val: [Char], info: Info} -> TokenParser_Token in
con UIdentTokenParser_UIdentRepr: () -> TokenReprBase_TokenRepr in
con UIdentTokenParser_UIdentTok: {val: [Char], info: Info} -> TokenParser_Token in
con UIntTokenParser_IntRepr: () -> TokenReprBase_TokenRepr in
con UIntTokenParser_IntTok: {val: Int, info: Info} -> TokenParser_Token in
con UFloatTokenParser_FloatRepr: () -> TokenReprBase_TokenRepr in
con UFloatTokenParser_FloatTok: {val: Float, info: Info} -> TokenParser_Token in
let parseOperatorCont: Pos -> [Char] -> {val: [Char], stream: {pos: Pos, str: [Char]}} =
  lam p.
    lam str.
      recursive
        let work =
          lam acc.
            lam p.
              lam str.
                match
                  str
                with
                  [ ('%' | '<' | '>' | '!' | '?' | '~' | ':' | '.' | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '@' | '^' | '|') & c ] ++ xs
                then
                  work (snoc acc c) (advanceCol p 1) xs
                else
                  { val = acc, stream = { pos = p, str = str } }
      in
      work "" p str
in
con OperatorTokenParser_OperatorRepr: () -> TokenReprBase_TokenRepr in
con OperatorTokenParser_OperatorTok: {val: [Char], info: Info} -> TokenParser_Token in
con BracketTokenParser_RBracketRepr: () -> TokenReprBase_TokenRepr in
con BracketTokenParser_LBracketRepr: () -> TokenReprBase_TokenRepr in
con BracketTokenParser_RParenRepr: () -> TokenReprBase_TokenRepr in
con BracketTokenParser_RBraceRepr: () -> TokenReprBase_TokenRepr in
con BracketTokenParser_LParenRepr: () -> TokenReprBase_TokenRepr in
con BracketTokenParser_LBraceRepr: () -> TokenReprBase_TokenRepr in
con BracketTokenParser_RBracketTok: {info: Info} -> TokenParser_Token in
con BracketTokenParser_LBracketTok: {info: Info} -> TokenParser_Token in
con BracketTokenParser_RParenTok: {info: Info} -> TokenParser_Token in
con BracketTokenParser_RBraceTok: {info: Info} -> TokenParser_Token in
con BracketTokenParser_LParenTok: {info: Info} -> TokenParser_Token in
con BracketTokenParser_LBraceTok: {info: Info} -> TokenParser_Token in
con SemiTokenParser_SemiRepr: () -> TokenReprBase_TokenRepr in
con SemiTokenParser_SemiTok: {info: Info} -> TokenParser_Token in
con CommaTokenParser_CommaRepr: () -> TokenReprBase_TokenRepr in
con CommaTokenParser_CommaTok: {info: Info} -> TokenParser_Token in
con StringTokenParser_StringRepr: () -> TokenReprBase_TokenRepr in
con StringTokenParser_StringTok: {val: [Char], info: Info} -> TokenParser_Token in
con CharTokenParser_CharRepr: () -> TokenReprBase_TokenRepr in
con CharTokenParser_CharTok: {val: Char, info: Info} -> TokenParser_Token in
con HashStringTokenParser_HashStringRepr: {hash: [Char]} -> TokenReprBase_TokenRepr in
con HashStringTokenParser_HashStringTok: {val: [Char], hash: [Char], info: Info} -> TokenParser_Token in
type ParseResult a =
  {pos: Pos, str: [Char], val: a}
in
type StrPos =
  {pos: Pos, str: [Char]}
in
let parseUInt: Pos -> [Char] -> {pos: Pos, str: [Char], val: [Char]} =
  lam p.
    lam str.
      recursive
        let work =
          lam p2.
            lam str.
              lam num.
                match
                  str
                with
                  [ x ] ++ xs
                then
                  let c = char2int x in
                  match
                    and (geqi c 48) (leqi c 57)
                  with
                    true
                  then
                    work (advanceCol p2 1) xs (snoc num x)
                  else
                    { val = num, pos = p2, str = str }
                else
                  { val = num, pos = p2, str = str }
      in
      work p str ""
in
let parseFloatExponent: Pos -> [Char] -> {pos: Pos, str: [Char], val: [Char]} =
  lam p.
    lam str.
      match
        str
      with
        ([ '+' | '-' ] ++ xs) & s
      then
        let n: ParseResult [Char] = parseUInt (advanceCol p 1) xs in
        match
          n.val
        with
          ""
        then
          n
        else
          { val = cons (head s) n.val, pos = n.pos, str = n.str }
      else
        parseUInt p str
in
let matchChar: Pos -> [Char] -> {pos: Pos, str: [Char], val: Char} =
  lam p.
    lam str: [Char].
      let ret =
        lam c.
          lam s.
            lam n.
              { val = c, pos = advanceCol p n, str = s }
      in
      match
        str
      with
        "\\" ++ xs
      then
        match
          xs
        with
          "\\" ++ xs
        then
          ret '\\' xs 2
        else match
          xs
        with
          "n" ++ xs
        then
          ret '\n' xs 2
        else match
          xs
        with
          "t" ++ xs
        then
          ret '\t' xs 2
        else match
          xs
        with
          "\"" ++ xs
        then
          ret '\"' xs 2
        else match
          xs
        with
          "\'" ++ xs
        then
          ret '\'' xs 2
        else
          posErrorExit (advanceCol p 1) "Unknown escape character."
      else match
        str
      with
        [ x ] ++ xs
      then
        ret x xs 1
      else
        posErrorExit p "Unexpected end of file."
in
type ExprInfixParser_Associativity
in
con ExprInfixParser_RightAssoc: () -> ExprInfixParser_Associativity in
con ExprInfixParser_LeftAssoc: () -> ExprInfixParser_Associativity in
recursive
  let vMExpr_getSymbol: SymLookup_LookupParams -> Map [Char] Name -> Name -> Name =
    lam lkup.
      lam env.
        lam __sem_target.
          match
            __sem_target
          with
            ident
          in
          match
              nameHasSym ident
            with
              true
            then
              ident
            else
              optionGetOrElse
                (lam #var"".
                   match
                     lkup.allowFree
                   with
                     true
                   then
                     ident
                   else
                     vMExpr_symLookupError lkup ident)
                (mapLookup (nameGetStr ident) env)
  let vMExpr_setSymbol: Map [Char] Name -> Name -> (Map [Char] Name, Name) =
    lam env.
      lam __sem_target.
        match
          __sem_target
        with
          ident
        in
        match
            nameHasSym ident
          with
            true
          then
            (env, ident)
          else
            let ident = nameSetNewSym ident in
            (mapInsert (nameGetStr ident) ident env, ident)
  let vMExpr_stripTyAll =
    lam __sem_target.
      match
        __sem_target
      with
        AliasTypeAst_TyAlias t & ty
      then
        let #var"X" = vMExpr_stripTyAll t.content in
        match
          #var"X"
        with
          ("", _)
        then
          ("", ty)
        else match
          #var"X"
        with
          stripped
        in
        stripped
      else match
        __sem_target
      with
        ty
      in
      vMExpr_stripTyAllBase "" ty
  let vMExpr_symbolizePat: all ext. SymEnv -> Map [Char] Name -> Ast_Pat -> (Map [Char] Name, Ast_Pat) =
    lam env.
      lam patEnv.
        lam __sem_target.
          match
            __sem_target
          with
            NamedPat_PatNamed p
          then
            match
              _symbolizePatName patEnv p.ident
            with
              (patEnv, patname)
            in
            (patEnv, NamedPat_PatNamed
                { p with ident = patname })
          else match
            __sem_target
          with
            SeqEdgePat_PatSeqEdge p
          then
            match
              mapAccumL (vMExpr_symbolizePat env) patEnv p.prefix
            with
              (patEnv, prefix)
            in
            match
                _symbolizePatName patEnv p.middle
              with
                (patEnv, middle)
              in
              match
                  mapAccumL (vMExpr_symbolizePat env) patEnv p.postfix
                with
                  (patEnv, postfix)
                in
                (patEnv, SeqEdgePat_PatSeqEdge
                    { p with prefix = prefix, middle = middle, postfix = postfix })
          else match
            __sem_target
          with
            DataPat_PatCon r
          then
            let ident =
              vMExpr_getSymbol
                { info = [ r.info ],
                  kind = "constructor",
                  allowFree = env.allowFree }
                env.currentEnv.conEnv
                r.ident
            in
            match
              vMExpr_symbolizePat env patEnv r.subpat
            with
              (patEnv, subpat)
            in
            (patEnv, DataPat_PatCon
                { r with ident = ident, subpat = subpat })
          else match
            __sem_target
          with
            NotPat_PatNot p
          then
            match
              vMExpr_symbolizePat env patEnv p.subpat
            with
              (_, subpat)
            in
            (patEnv, NotPat_PatNot
                { p with subpat = subpat })
          else match
            __sem_target
          with
            t
          in
          vMExpr_smapAccumL_Pat_Pat (vMExpr_symbolizePat env) patEnv t
  let vMExpr_symbolizeExpr: SymEnv -> Ast_Expr -> Ast_Expr =
    lam env: SymEnv.
      lam __sem_target.
        match
          __sem_target
        with
          VarAst_TmVar t
        then
          let ident =
            vMExpr_getSymbol
              { info = [ t.info ], kind = "variable", allowFree = env.allowFree }
              env.currentEnv.varEnv
              t.ident
          in
          VarAst_TmVar
            { t with ident = ident }
        else match
          __sem_target
        with
          LamAst_TmLam t
        then
          match
            vMExpr_setSymbol env.currentEnv.varEnv t.ident
          with
            (varEnv, ident)
          in
          LamAst_TmLam
              { t
                with
                ident = ident,
                  tyAnnot = vMExpr_symbolizeType env t.tyAnnot,
                  body =
                  vMExpr_symbolizeExpr (symbolizeUpdateVarEnv env varEnv) t.body }
        else match
          __sem_target
        with
          LetAst_TmLet t
        then
          match
            vMExpr_symbolizeTyAnnot env t.tyAnnot
          with
            (tyVarEnv, tyAnnot)
          in
          match
              vMExpr_setSymbol env.currentEnv.varEnv t.ident
            with
              (varEnv, ident)
            in
            LetAst_TmLet
                { t
                  with
                  ident = ident,
                    tyAnnot = tyAnnot,
                    body =
                    vMExpr_symbolizeExpr (symbolizeUpdateTyVarEnv env tyVarEnv) t.body,
                    inexpr =
                    vMExpr_symbolizeExpr (symbolizeUpdateVarEnv env varEnv) t.inexpr }
        else match
          __sem_target
        with
          RecLetsAst_TmRecLets t
        then
          let setSymbolIdent =
            lam env.
              lam b.
                match
                  vMExpr_setSymbol env b.ident
                with
                  (env, ident)
                in
                (env, { b with ident = ident })
          in
          match
            mapAccumL setSymbolIdent env.currentEnv.varEnv t.bindings
          with
            (varEnv, bindings)
          in
          let newEnv = symbolizeUpdateVarEnv env varEnv in
            let bindings =
              map
                (lam b.
                   match
                     vMExpr_symbolizeTyAnnot env b.tyAnnot
                   with
                     (tyVarEnv, tyAnnot)
                   in
                   { b
                       with
                       body =
                         vMExpr_symbolizeExpr (symbolizeUpdateTyVarEnv newEnv tyVarEnv) b.body,
                         tyAnnot = tyAnnot })
                bindings
            in
            RecLetsAst_TmRecLets
              { t
                with
                bindings = bindings,
                  inexpr = vMExpr_symbolizeExpr newEnv t.inexpr }
        else match
          __sem_target
        with
          ExtAst_TmExt t
        then
          let setName =
            match
              env.ignoreExternals
            with
              true
            then
              lam x.
                lam y.
                  (x, y)
            else
              vMExpr_setSymbol
          in
          match
            setName env.currentEnv.varEnv t.ident
          with
            (varEnv, ident)
          in
          ExtAst_TmExt
              { t
                with
                ident = ident,
                  inexpr =
                  vMExpr_symbolizeExpr (symbolizeUpdateVarEnv env varEnv) t.inexpr,
                  tyIdent = vMExpr_symbolizeType env t.tyIdent }
        else match
          __sem_target
        with
          TypeAst_TmType t
        then
          match
            vMExpr_setSymbol env.currentEnv.tyConEnv t.ident
          with
            (tyConEnv, ident)
          in
          match
              mapAccumL vMExpr_setSymbol env.currentEnv.tyVarEnv t.params
            with
              (tyVarEnv, params)
            in
            TypeAst_TmType
                { t
                  with
                  ident = ident,
                    params = params,
                    tyIdent =
                    vMExpr_symbolizeType (symbolizeUpdateTyVarEnv env tyVarEnv) t.tyIdent,
                    inexpr =
                    vMExpr_symbolizeExpr (symbolizeUpdateTyConEnv env tyConEnv) t.inexpr }
        else match
          __sem_target
        with
          DataAst_TmConDef t
        then
          match
            vMExpr_setSymbol env.currentEnv.conEnv t.ident
          with
            (conEnv, ident)
          in
          DataAst_TmConDef
              { t
                with
                ident = ident,
                  tyIdent = vMExpr_symbolizeType env t.tyIdent,
                  inexpr =
                  vMExpr_symbolizeExpr (symbolizeUpdateConEnv env conEnv) t.inexpr }
        else match
          __sem_target
        with
          DataAst_TmConApp t
        then
          let ident =
            vMExpr_getSymbol
              { info = [ t.info ],
                kind = "constructor",
                allowFree = env.allowFree }
              env.currentEnv.conEnv
              t.ident
          in
          DataAst_TmConApp
            { t with ident = ident, body = vMExpr_symbolizeExpr env t.body }
        else match
          __sem_target
        with
          MatchAst_TmMatch t
        then
          match
            vMExpr_symbolizePat env (mapEmpty cmpString) t.pat
          with
            (thnVarEnv, pat)
          in
          let thnPatEnv =
              symbolizeUpdateVarEnv env (mapUnion env.currentEnv.varEnv thnVarEnv)
            in
            MatchAst_TmMatch
              { t
                with
                target = vMExpr_symbolizeExpr env t.target,
                  pat = pat,
                  thn = vMExpr_symbolizeExpr thnPatEnv t.thn,
                  els = vMExpr_symbolizeExpr env t.els }
        else match
          __sem_target
        with
          t
        in
        let t = vMExpr_smap_Expr_Expr (vMExpr_symbolizeExpr env) t in
          let t = vMExpr_smap_Expr_Type (vMExpr_symbolizeType env) t in
          t
  let vMExpr_symbolizeKind: Info -> SymEnv -> Ast_Kind -> Ast_Kind =
    lam info.
      lam env.
        lam __sem_target.
          match
            __sem_target
          with
            DataKindAst_Data t
          then
            let symbolizeCons =
              lam cons.
                setFold
                  (lam ks.
                     lam k.
                       setInsert
                         (vMExpr_getSymbol
                            { info = [ info ],
                              kind = "constructor",
                              allowFree = env.allowFree }
                            env.currentEnv.conEnv
                            k)
                         ks)
                  (setEmpty nameCmp)
                  cons
            in
            let types =
              foldl
                (lam m.
                   lam b.
                     match
                       b
                     with
                       (t, r)
                     in
                     let t =
                         vMExpr_getSymbol
                           { info = [ info ],
                             kind = "type constructor",
                             allowFree = env.allowFree }
                           env.currentEnv.tyConEnv
                           t
                       in
                       mapInsert
                         t
                         { r
                           with
                           lower = symbolizeCons r.lower,
                             upper = optionMap symbolizeCons r.upper }
                         m)
                (mapEmpty nameCmp)
                (mapBindings t.types)
            in
            DataKindAst_Data
              { t with types = types }
          else match
            __sem_target
          with
            t
          in
          vMExpr_smap_Kind_Type (vMExpr_symbolizeType env) t
  let vMExpr_symbolizeType: SymEnv -> Ast_Type -> Ast_Type =
    lam env.
      lam __sem_target.
        match
          __sem_target
        with
          VariantTypeAst_TyVariant t & ty
        then
          match
            eqi (mapLength t.constrs) 0
          with
            true
          then
            ty
          else
            errorSingle
              [ t.info ]
              "Symbolizing non-empty variant types not yet supported"
        else match
          __sem_target
        with
          ConTypeAst_TyCon t
        then
          let ident =
            vMExpr_getSymbol
              { info = [ t.info ],
                kind = "type constructor",
                allowFree = env.allowFree }
              env.currentEnv.tyConEnv
              t.ident
          in
          ConTypeAst_TyCon
            { t with ident = ident, data = vMExpr_symbolizeType env t.data }
        else match
          __sem_target
        with
          DataTypeAst_TyData t
        then
          let cons =
            setFold
              (lam ks.
                 lam k.
                   setInsert
                     (vMExpr_getSymbol
                        { info = [ t.info ],
                          kind = "constructor",
                          allowFree = env.allowFree }
                        env.currentEnv.conEnv
                        k)
                     ks)
              (setEmpty nameCmp)
              t.cons
          in
          DataTypeAst_TyData
            { t with cons = cons }
        else match
          __sem_target
        with
          VarTypeAst_TyVar t
        then
          let ident =
            vMExpr_getSymbol
              { info = [ t.info ],
                kind = "type variable",
                allowFree = env.allowFree }
              env.currentEnv.tyVarEnv
              t.ident
          in
          VarTypeAst_TyVar
            { t with ident = ident }
        else match
          __sem_target
        with
          AllTypeAst_TyAll t
        then
          let kind = vMExpr_symbolizeKind t.info env t.kind in
          match
            vMExpr_setSymbol env.currentEnv.tyVarEnv t.ident
          with
            (tyVarEnv, ident)
          in
          AllTypeAst_TyAll
              { t
                with
                ident = ident,
                  ty =
                  vMExpr_symbolizeType (symbolizeUpdateTyVarEnv env tyVarEnv) t.ty,
                  kind = kind }
        else match
          __sem_target
        with
          t
        in
        vMExpr_smap_Type_Type (vMExpr_symbolizeType env) t
  let vMExpr_smap_Expr_Expr: (Ast_Expr -> Ast_Expr) -> Ast_Expr -> Ast_Expr =
    lam f.
      lam __sem_target.
        match
          __sem_target
        with
          p
        in
        let res =
            vMExpr_smapAccumL_Expr_Expr
              (lam #var"".
                 lam a.
                   ({}, f a))
              {}
              p
          in
          res.1
  let vMExpr_smap_Expr_Type: (Ast_Type -> Ast_Type) -> Ast_Expr -> Ast_Expr =
    lam f.
      lam __sem_target.
        match
          __sem_target
        with
          p
        in
        let res =
            vMExpr_smapAccumL_Expr_Type
              (lam #var"".
                 lam a.
                   ({}, f a))
              {}
              p
          in
          res.1
  let vMExpr_smap_Kind_Type: (Ast_Type -> Ast_Type) -> Ast_Kind -> Ast_Kind =
    lam f: Ast_Type -> Ast_Type.
      lam __sem_target.
        match
          __sem_target
        with
          s
        in
        match
            vMExpr_smapAccumL_Kind_Type
              (lam #var"".
                 lam x.
                   ({}, f x))
              {}
              s
          with
            (_, s)
          in
          s
  let vMExpr_smap_Type_Type: (Ast_Type -> Ast_Type) -> Ast_Type -> Ast_Type =
    lam f.
      lam __sem_target.
        match
          __sem_target
        with
          p
        in
        let res =
            vMExpr_smapAccumL_Type_Type
              (lam #var"".
                 lam a.
                   ({}, f a))
              {}
              p
          in
          res.1
  let vMExpr_stripTyAllBase =
    lam vars: [(Name, Ast_Kind)].
      lam __sem_target.
        match
          __sem_target
        with
          AllTypeAst_TyAll t
        then
          vMExpr_stripTyAllBase (snoc vars (t.ident, t.kind)) t.ty
        else match
          __sem_target
        with
          ty
        in
        vMExpr_rappAccumL_Type_Type vMExpr_stripTyAllBase vars ty
  let vMExpr_symLookupError: all a. SymLookup_LookupParams -> Name -> a =
    lam lkup.
      lam __sem_target.
        match
          __sem_target
        with
          ident
        in
        errorSingle
            lkup.info
            (join
               [ "Unknown ",
                 lkup.kind,
                 " in symbolize: ",
                 nameGetStr ident ])
  let vMExpr_sfold_Expr_Expr: all acc. (acc -> Ast_Expr -> acc) -> acc -> Ast_Expr -> acc =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            p
          in
          let res =
              vMExpr_smapAccumL_Expr_Expr
                (lam acc.
                   lam a.
                     (f acc a, a))
                acc
                p
            in
            res.0
  let vMExpr_symbolizeTyAnnot: SymEnv -> Ast_Type -> (Map [Char] Name, Ast_Type) =
    lam env.
      lam __sem_target.
        match
          __sem_target
        with
          tyAnnot
        in
        let symbolized = vMExpr_symbolizeType env tyAnnot in
          match
            vMExpr_stripTyAll symbolized
          with
            (vars, stripped)
          in
          (foldl
              (lam env.
                 lam nk.
                   mapInsert (nameGetStr nk.0) nk.0 env)
              env.currentEnv.tyVarEnv
              vars, symbolized)
  let vMExpr_smapAccumL_Pat_Pat: all acc. (acc -> Ast_Pat -> (acc, Ast_Pat)) -> acc -> Ast_Pat -> (acc, Ast_Pat) =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            SeqTotPat_PatSeqTot r
          then
            match
              mapAccumL f acc r.pats
            with
              (acc, pats)
            in
            (acc, SeqTotPat_PatSeqTot
                { r with pats = pats })
          else match
            __sem_target
          with
            SeqEdgePat_PatSeqEdge p
          then
            match
              mapAccumL f acc p.prefix
            with
              (acc, prefix)
            in
            match
                mapAccumL f acc p.postfix
              with
                (acc, postfix)
              in
              (acc, SeqEdgePat_PatSeqEdge
                  { p with prefix = prefix, postfix = postfix })
          else match
            __sem_target
          with
            RecordPat_PatRecord p
          then
            match
              mapMapAccum
                (lam acc.
                   lam #var"".
                     lam p.
                       f acc p)
                acc
                p.bindings
            with
              (acc, bindings)
            in
            (acc, RecordPat_PatRecord
                { p with bindings = bindings })
          else match
            __sem_target
          with
            DataPat_PatCon c
          then
            match
              f acc c.subpat
            with
              (acc, subpat)
            in
            (acc, DataPat_PatCon
                { c with subpat = subpat })
          else match
            __sem_target
          with
            AndPat_PatAnd p
          then
            match
              f acc p.lpat
            with
              (acc, lpat)
            in
            match
                f acc p.rpat
              with
                (acc, rpat)
              in
              (acc, AndPat_PatAnd
                  { p with lpat = lpat, rpat = rpat })
          else match
            __sem_target
          with
            OrPat_PatOr p
          then
            match
              f acc p.lpat
            with
              (acc, lpat)
            in
            match
                f acc p.rpat
              with
                (acc, rpat)
              in
              (acc, OrPat_PatOr
                  { p with lpat = lpat, rpat = rpat })
          else match
            __sem_target
          with
            NotPat_PatNot p
          then
            match
              f acc p.subpat
            with
              (acc, subpat)
            in
            (acc, NotPat_PatNot
                { p with subpat = subpat })
          else match
            __sem_target
          with
            p
          in
          (acc, p)
  let vMExpr_symbolizeAllowFree =
    lam __sem_target.
      match
        __sem_target
      with
        expr
      in
      let env = { symEnvDefault with allowFree = true } in
        vMExpr_symbolizeExpr env expr
  let vMExpr_rappAccumL_Type_Type: all acc. (acc -> Ast_Type -> (acc, Ast_Type)) -> acc -> Ast_Type -> (acc, Ast_Type) =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            AliasTypeAst_TyAlias t
          then
            f acc t.content
          else match
            __sem_target
          with
            ty
          in
          (acc, ty)
  let vMExpr_smapAccumL_Expr_Expr: all acc. (acc -> Ast_Expr -> (acc, Ast_Expr)) -> acc -> Ast_Expr -> (acc, Ast_Expr) =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            AppAst_TmApp t
          then
            match
              f acc t.lhs
            with
              (acc, lhs)
            in
            match
                f acc t.rhs
              with
                (acc, rhs)
              in
              (acc, AppAst_TmApp
                  { t with lhs = lhs, rhs = rhs })
          else match
            __sem_target
          with
            LamAst_TmLam t
          then
            match
              f acc t.body
            with
              (acc, body)
            in
            (acc, LamAst_TmLam
                { t with body = body })
          else match
            __sem_target
          with
            LetAst_TmLet t
          then
            match
              f acc t.body
            with
              (acc, body)
            in
            match
                f acc t.inexpr
              with
                (acc, inexpr)
              in
              (acc, LetAst_TmLet
                  { t with body = body, inexpr = inexpr })
          else match
            __sem_target
          with
            RecLetsAst_TmRecLets t
          then
            let bindingFunc =
              lam acc.
                lam b: RecLetsAst_RecLetBinding.
                  match
                    f acc b.body
                  with
                    (acc, body)
                  in
                  (acc, { b with body = body })
            in
            match
              mapAccumL bindingFunc acc t.bindings
            with
              (acc, bindings)
            in
            match
                f acc t.inexpr
              with
                (acc, inexpr)
              in
              (acc, RecLetsAst_TmRecLets
                  { t with bindings = bindings, inexpr = inexpr })
          else match
            __sem_target
          with
            SeqAst_TmSeq t
          then
            match
              mapAccumL f acc t.tms
            with
              (acc, tms)
            in
            (acc, SeqAst_TmSeq
                { t with tms = tms })
          else match
            __sem_target
          with
            RecordAst_TmRecord t
          then
            match
              mapMapAccum
                (lam acc.
                   lam #var"".
                     lam e.
                       f acc e)
                acc
                t.bindings
            with
              (acc, bindings)
            in
            (acc, RecordAst_TmRecord
                { t with bindings = bindings })
          else match
            __sem_target
          with
            RecordAst_TmRecordUpdate t
          then
            match
              f acc t.rec
            with
              (acc, rec)
            in
            match
                f acc t.value
              with
                (acc, value)
              in
              (acc, RecordAst_TmRecordUpdate
                  { t with rec = rec, value = value })
          else match
            __sem_target
          with
            TypeAst_TmType t
          then
            match
              f acc t.inexpr
            with
              (acc, inexpr)
            in
            (acc, TypeAst_TmType
                { t with inexpr = inexpr })
          else match
            __sem_target
          with
            DataAst_TmConDef t
          then
            match
              f acc t.inexpr
            with
              (acc, inexpr)
            in
            (acc, DataAst_TmConDef
                { t with inexpr = inexpr })
          else match
            __sem_target
          with
            DataAst_TmConApp t
          then
            match
              f acc t.body
            with
              (acc, body)
            in
            (acc, DataAst_TmConApp
                { t with body = body })
          else match
            __sem_target
          with
            MatchAst_TmMatch t
          then
            match
              f acc t.target
            with
              (acc, target)
            in
            match
                f acc t.thn
              with
                (acc, thn)
              in
              match
                  f acc t.els
                with
                  (acc, els)
                in
                (acc, MatchAst_TmMatch
                    { t with target = target, thn = thn, els = els })
          else match
            __sem_target
          with
            UtestAst_TmUtest t
          then
            match
              f acc t.test
            with
              (acc, test)
            in
            match
                f acc t.expected
              with
                (acc, expected)
              in
              match
                  f acc t.next
                with
                  (acc, next)
                in
                match
                    optionMapAccum f acc t.tusing
                  with
                    (acc, tusing)
                  in
                  match
                      optionMapAccum f acc t.tonfail
                    with
                      (acc, tonfail)
                    in
                    (acc, UtestAst_TmUtest
                        { t
                          with
                          test = test,
                            expected = expected,
                            next = next,
                            tusing = tusing,
                            tonfail = tonfail })
          else match
            __sem_target
          with
            ExtAst_TmExt t
          then
            match
              f acc t.inexpr
            with
              (acc, inexpr)
            in
            (acc, ExtAst_TmExt
                { t with inexpr = inexpr })
          else match
            __sem_target
          with
            p
          in
          (acc, p)
  let vMExpr_smapAccumL_Expr_Type: all acc. (acc -> Ast_Type -> (acc, Ast_Type)) -> acc -> Ast_Expr -> (acc, Ast_Expr) =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            LamAst_TmLam t
          then
            match
              f acc t.tyAnnot
            with
              (acc, tyAnnot)
            in
            (acc, LamAst_TmLam
                { t with tyAnnot = tyAnnot })
          else match
            __sem_target
          with
            LetAst_TmLet t
          then
            match
              f acc t.tyAnnot
            with
              (acc, tyAnnot)
            in
            (acc, LetAst_TmLet
                { t with tyAnnot = tyAnnot })
          else match
            __sem_target
          with
            RecLetsAst_TmRecLets t
          then
            let bindingFunc =
              lam acc.
                lam b: RecLetsAst_RecLetBinding.
                  match
                    f acc b.tyAnnot
                  with
                    (acc, tyAnnot)
                  in
                  (acc, { b with tyAnnot = tyAnnot })
            in
            match
              mapAccumL bindingFunc acc t.bindings
            with
              (acc, bindings)
            in
            (acc, RecLetsAst_TmRecLets
                { t with bindings = bindings })
          else match
            __sem_target
          with
            TypeAst_TmType t
          then
            match
              f acc t.tyIdent
            with
              (acc, tyIdent)
            in
            (acc, TypeAst_TmType
                { t with tyIdent = tyIdent })
          else match
            __sem_target
          with
            DataAst_TmConDef t
          then
            match
              f acc t.tyIdent
            with
              (acc, tyIdent)
            in
            (acc, DataAst_TmConDef
                { t with tyIdent = tyIdent })
          else match
            __sem_target
          with
            ExtAst_TmExt t
          then
            match
              f acc t.tyIdent
            with
              (acc, tyIdent)
            in
            (acc, ExtAst_TmExt
                { t with tyIdent = tyIdent })
          else match
            __sem_target
          with
            p
          in
          (acc, p)
  let vMExpr_smapAccumL_Kind_Type: all acc. (acc -> Ast_Type -> (acc, Ast_Type)) -> acc -> Ast_Kind -> (acc, Ast_Kind) =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            RecordKindAst_Record r
          then
            match
              mapMapAccum
                (lam acc.
                   lam #var"".
                     lam e.
                       f acc e)
                acc
                r.fields
            with
              (acc, flds)
            in
            (acc, RecordKindAst_Record
                { r with fields = flds })
          else match
            __sem_target
          with
            s
          in
          (acc, s)
  let vMExpr_smapAccumL_Type_Type: all acc. (acc -> Ast_Type -> (acc, Ast_Type)) -> acc -> Ast_Type -> (acc, Ast_Type) =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            FunTypeAst_TyArrow t
          then
            match
              f acc t.from
            with
              (acc, from)
            in
            match
                f acc t.to
              with
                (acc, to)
              in
              (acc, FunTypeAst_TyArrow
                  { t with from = from, to = to })
          else match
            __sem_target
          with
            SeqTypeAst_TySeq t
          then
            match
              f acc t.ty
            with
              (acc, ty)
            in
            (acc, SeqTypeAst_TySeq
                { t with ty = ty })
          else match
            __sem_target
          with
            TensorTypeAst_TyTensor t
          then
            match
              f acc t.ty
            with
              (acc, ty)
            in
            (acc, TensorTypeAst_TyTensor
                { t with ty = ty })
          else match
            __sem_target
          with
            RecordTypeAst_TyRecord t
          then
            match
              mapMapAccum
                (lam acc.
                   lam #var"".
                     lam e.
                       f acc e)
                acc
                t.fields
            with
              (acc, fields)
            in
            (acc, RecordTypeAst_TyRecord
                { t with fields = fields })
          else match
            __sem_target
          with
            VariantTypeAst_TyVariant t
          then
            match
              mapMapAccum
                (lam acc.
                   lam #var"".
                     lam e.
                       f acc e)
                acc
                t.constrs
            with
              (acc, constrs)
            in
            (acc, VariantTypeAst_TyVariant
                { t with constrs = constrs })
          else match
            __sem_target
          with
            ConTypeAst_TyCon t
          then
            match
              f acc t.data
            with
              (acc, data)
            in
            (acc, ConTypeAst_TyCon
                { t with data = data })
          else match
            __sem_target
          with
            AllTypeAst_TyAll t
          then
            match
              vMExpr_smapAccumL_Kind_Type f acc t.kind
            with
              (acc, kind)
            in
            match
                f acc t.ty
              with
                (acc, ty)
              in
              (acc, AllTypeAst_TyAll
                  { t with kind = kind, ty = ty })
          else match
            __sem_target
          with
            AppTypeAst_TyApp t
          then
            match
              f acc t.lhs
            with
              (acc, lhs)
            in
            match
                f acc t.rhs
              with
                (acc, rhs)
              in
              (acc, AppTypeAst_TyApp
                  { t with lhs = lhs, rhs = rhs })
          else match
            __sem_target
          with
            AliasTypeAst_TyAlias t
          then
            match
              f acc t.content
            with
              (acc, content)
            in
            match
                f acc t.display
              with
                (acc, display)
              in
              (acc, AliasTypeAst_TyAlias
                  { t with content = content, display = display })
          else match
            __sem_target
          with
            p
          in
          (acc, p)
in
type OCamlTopBinding =
  {body: Ast_Expr, ident: Name, tyBody: Ast_Type}
in
type OCamlTopAst_Top
in
con OCamlTopAst_OTopVariantTypeDecl: {ident: Name, constrs: Map Name Ast_Type} -> OCamlTopAst_Top in
con OCamlTopAst_OTopCExternalDecl: {ty: Ast_Type, ident: Name, nativeIdent: Name, bytecodeIdent: Name} -> OCamlTopAst_Top in
con OCamlTopAst_OTopTypeDecl: {ty: Ast_Type, ident: Name} -> OCamlTopAst_Top in
con OCamlTopAst_OTopTryWith: {try: Ast_Expr, arms: [(Ast_Pat, Ast_Expr)]} -> OCamlTopAst_Top in
con OCamlTopAst_OTopRecLets: {bindings: [OCamlTopBinding]} -> OCamlTopAst_Top in
con OCamlTopAst_OTopExpr: {expr: Ast_Expr} -> OCamlTopAst_Top in
con OCamlTopAst_OTopLet: {body: Ast_Expr, ident: Name, tyBody: Ast_Type} -> OCamlTopAst_Top in
con OCamlRecord_OTmProject: {tm: Ast_Expr, field: [Char]} -> Ast_Expr in
con OCamlRecord_OTmRecord: {tyident: Ast_Type, bindings: [([Char], Ast_Expr)]} -> Ast_Expr in
con OCamlRecord_OPatRecord: {bindings: Map SID Ast_Pat} -> Ast_Pat in
con OCamlRecordUpdate_OTmRecordUpdate: {rec: Ast_Expr, updates: [(SID, Ast_Expr)]} -> Ast_Expr in
con OCamlMatch_OTmMatch: {arms: [(Ast_Pat, Ast_Expr)], target: Ast_Expr} -> Ast_Expr in
con OCamlArray_OTmArray: {tms: [Ast_Expr]} -> Ast_Expr in
con OCamlTuple_OTmTuple: {values: [Ast_Expr]} -> Ast_Expr in
con OCamlTuple_OPatTuple: {pats: [Ast_Pat]} -> Ast_Pat in
con OCamlData_OTmConApp: {args: [Ast_Expr], ident: Name} -> Ast_Expr in
con OCamlData_OPatCon: {args: [Ast_Pat], ident: Name} -> Ast_Pat in
con OCamlString_OTmString: {text: [Char]} -> Ast_Expr in
con OCamlExternal_OTmConAppExt: {args: [Ast_Expr], ident: [Char]} -> Ast_Expr in
con OCamlExternal_OTmExprExt: {expr: [Char]} -> Ast_Expr in
con OCamlExternal_OTmVarExt: {ident: [Char]} -> Ast_Expr in
con OCamlExternal_OPatConExt: {args: [Ast_Pat], ident: [Char]} -> Ast_Pat in
con OCamlLabel_OTmLabel: {arg: Ast_Expr, label: [Char]} -> Ast_Expr in
con OCamlLam_OTmLam: {body: Ast_Expr, ident: Name, label: Option [Char]} -> Ast_Expr in
con OCamlTypeAst_OTyBigarrayFloat64Elt: {info: Info} -> Ast_Type in
con OCamlTypeAst_OTyBigarrayGenarray: {ty: Ast_Type, elty: Ast_Type, info: Info, layout: Ast_Type} -> Ast_Type in
con OCamlTypeAst_OTyBigarrayClayout: {info: Info} -> Ast_Type in
con OCamlTypeAst_OTyBigarrayIntElt: {info: Info} -> Ast_Type in
con OCamlTypeAst_OTyInlinedRecord: {info: Info} -> Ast_Type in
con OCamlTypeAst_OTyBigarrayArray: {ty: Ast_Type, elty: Ast_Type, info: Info, rank: Int, layout: Ast_Type} -> Ast_Type in
con OCamlTypeAst_OTyRecordExt: {info: Info, fields: [{ty: Ast_Type, label: [Char], asLabel: [Char]}], tyident: Ast_Type} -> Ast_Type in
con OCamlTypeAst_OTyVarExt: {args: [Ast_Type], info: Info, ident: [Char]} -> Ast_Type in
con OCamlTypeAst_OTyString: {info: Info} -> Ast_Type in
con OCamlTypeAst_OTyRecord: {info: Info, fields: [([Char], Ast_Type)], tyident: Ast_Type} -> Ast_Type in
con OCamlTypeAst_OTyTuple: {tys: [Ast_Type], info: Info} -> Ast_Type in
con OCamlTypeAst_OTyParam: {info: Info, ident: [Char]} -> Ast_Type in
con OCamlTypeAst_OTyLabel: {ty: Ast_Type, info: Info, label: [Char]} -> Ast_Type in
con OCamlTypeAst_OTyArray: {ty: Ast_Type, info: Info} -> Ast_Type in
con OCamlTypeAst_OTyList: {ty: Ast_Type, info: Info} -> Ast_Type in
con OCamlTypeAst_OTyVar: {info: Info, ident: Name} -> Ast_Type in
let otylist_ =
  lam ty.
    OCamlTypeAst_OTyList
      { ty = ty, info = NoInfo
            {} }
in
let otyarray_ =
  lam ty.
    OCamlTypeAst_OTyArray
      { ty = ty, info = NoInfo
            {} }
in
let otygenarray_ =
  lam ty.
    lam elty.
      lam layout.
        OCamlTypeAst_OTyBigarrayGenarray
          { ty = ty,
            info = NoInfo
                {},
            elty = elty,
            layout = layout }
in
let otybaarray_ =
  lam rank.
    lam ty.
      lam elty.
        lam layout.
          OCamlTypeAst_OTyBigarrayArray
            { ty = ty,
              info = NoInfo
                  {},
              elty = elty,
              layout = layout,
              rank = rank }
in
let oclayout_ =
  OCamlTypeAst_OTyBigarrayClayout
    { info = NoInfo
          {} }
in
let otygenarrayclayoutint_ =
  otygenarray_
    tyint_
    (OCamlTypeAst_OTyBigarrayIntElt
       { info = NoInfo
             {} })
    oclayout_
in
let otygenarrayclayoutfloat_ =
  otygenarray_
    tyfloat_
    (OCamlTypeAst_OTyBigarrayFloat64Elt
       { info = NoInfo
             {} })
    oclayout_
in
let otybaarrayclayoutint_ =
  lam rank.
    otybaarray_
      rank
      tyint_
      (OCamlTypeAst_OTyBigarrayIntElt
         { info = NoInfo
               {} })
      oclayout_
in
let otybaarrayclayoutfloat_ =
  lam rank.
    otybaarray_
      rank
      tyfloat_
      (OCamlTypeAst_OTyBigarrayFloat64Elt
         { info = NoInfo
               {} })
      oclayout_
in
let otytuple_ =
  lam tys.
    OCamlTypeAst_OTyTuple
      { info = NoInfo
            {}, tys = tys }
in
let otyunit_ = otytuple_ "" in
let otyvarext_ =
  lam ident.
    lam args.
      OCamlTypeAst_OTyVarExt
        { info = NoInfo
              {}, ident = ident, args = args }
in
let otyparam_ =
  lam ident.
    OCamlTypeAst_OTyParam
      { info = NoInfo
            {}, ident = ident }
in
let otylabel_ =
  lam label.
    lam ty.
      OCamlTypeAst_OTyLabel
        { ty = ty, info = NoInfo
              {}, label = label }
in
let otyrecordext_ =
  lam tyident.
    lam fields.
      OCamlTypeAst_OTyRecordExt
        { info = NoInfo
              {},
          fields = fields,
          tyident = tyident }
in
let otystring_ = OCamlTypeAst_OTyString
    { info = NoInfo
          {} }
in
let otyopaque_ = otyvarext_ "opaque" "" in
type WriteChannel
in
type ReadChannel
in
external externalWriteString! : WriteChannel -> [Char] -> ()
in
let fileWriteString: WriteChannel -> [Char] -> () = lam c.
    lam s.
      externalWriteString c s
in
external externalReadLine! : ReadChannel -> ([Char], Bool)
in
let fileReadLine: ReadChannel -> Option [Char] =
  lam rc.
    match
      externalReadLine rc
    with
      (s, false)
    then
      Some
        s
    else
      None
        {}
in
external externalReadBytes! : ReadChannel -> Int -> ([Int], Bool, Bool)
in
let fileReadBytes: ReadChannel -> Int -> Option [Int] =
  lam rc.
    lam len.
      let #var"X" = externalReadBytes rc len in
      match
        #var"X"
      with
        ("", true, _)
      then
        None
          {}
      else match
        #var"X"
      with
        (s, _, false)
      then
        Some
          s
      else match
        #var"X"
      with
        _
      in
      None
          {}
in
external externalStdin! : ReadChannel
in
let fileStdin = externalStdin in
external externalStdout! : WriteChannel
in
let fileStdout = externalStdout in
external externalStderr! : WriteChannel
in
let fileStderr = externalStderr in
type Dyn
in
let asDyn: all a. a -> Dyn = unsafeCoerce in
let fromDyn: all a. Dyn -> a = unsafeCoerce in
type SpecSymbol tok repr state prodLabel
in
type ParsedSymbol tok
in
type Action tok state =
  state -> [ParsedSymbol tok] -> Dyn
in
con TokSpec: all tok. all repr. all state. all prodLabel. repr -> SpecSymbol tok repr state prodLabel in
con LitSpec: all tok. all repr. all state. all prodLabel. {lit: [Char]} -> SpecSymbol tok repr state prodLabel in
con NtSpec: all tok. all repr. all state. all prodLabel. Name -> SpecSymbol tok repr state prodLabel in
con NtSym: all repr. all tok. all state. all prodLabel. {nt: Name, table: Ref (Map (SpecSymbol tok repr state prodLabel) {syms: [SpecSymbol tok repr state prodLabel], label: prodLabel, action: Action tok state})} -> SpecSymbol tok repr state prodLabel in
con UserSym: all tok. Dyn -> ParsedSymbol tok in
con TokParsed: all tok. tok -> ParsedSymbol tok in
con LitParsed: all tok. {lit: [Char], info: Info} -> ParsedSymbol tok in
type StackItem tok repr state prodLabel =
  {rest: [SpecSymbol tok repr state prodLabel], seen: [ParsedSymbol tok], label: prodLabel}
in
type ParseError tok repr state prodLabel
in
con UnexpectedFirst: all tok. all repr. all state. all prodLabel. {nt: Name, found: ParsedSymbol tok, stack: [StackItem tok repr state prodLabel], expected: [SpecSymbol tok repr state prodLabel]} -> ParseError tok repr state prodLabel in
con UnexpectedToken: all tok. all repr. all state. all prodLabel. {found: ParsedSymbol tok, stack: [StackItem tok repr state prodLabel], expected: SpecSymbol tok repr state prodLabel} -> ParseError tok repr state prodLabel in
type ParserBase_SymSet state prodLabel =
  {eps: Bool, syms: Map (SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel) ()}
in
type ParserConcrete_TableProd prodLabel state =
  {syms: [SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel], label: prodLabel, action: Action TokenParser_Token state}
in
type ParserConcrete_Table prodLabel state =
  {lits: Map [Char] (), start: {nt: Name, table: Ref (Map (SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel) (ParserConcrete_TableProd prodLabel state))}, firstOfRhs: [SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel] -> ParserBase_SymSet state prodLabel}
in
type ParserConcrete_Production prodLabel state =
  {nt: Name, rhs: [SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel], label: prodLabel, action: Action TokenParser_Token state}
in
type ParserConcrete_Grammar prodLabel state =
  {start: Name, productions: [ParserConcrete_Production prodLabel state]}
in
type ParserConcrete_FullStackItem prodLabel state =
  {rest: [SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel], seen: [ParsedSymbol TokenParser_Token], label: prodLabel, action: Action TokenParser_Token state}
in
let _iterateUntilFixpoint: all a. (a -> a -> Bool) -> (a -> a) -> a -> a =
  lam eq.
    lam f.
      recursive
        let work =
          lam a.
            let next = f a in
            match
              eq a next
            with
              true
            then
              a
            else
              work next
      in
      work
in
type GenError prodLabel state =
  Map Name (Map (SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel) [prodLabel])
in
type AllowedDirection
in
con GNeither: () -> AllowedDirection in
con GLeft: () -> AllowedDirection in
con GRight: () -> AllowedDirection in
con GEither: () -> AllowedDirection in
let _includesLeft: AllowedDirection -> Bool =
  lam dir.
    match
      dir
    with
      GEither _ | GLeft _
    then
      true
    else
      false
in
let _includesRight: AllowedDirection -> Bool =
  lam dir.
    match
      dir
    with
      GEither _ | GRight _
    then
      true
    else
      false
in
type PermanentId =
  Symbol
in
type LOpen
in
type LClosed
in
type ROpen
in
type RClosed
in
type WrappedSelf
in
con WAtom: all self. self LClosed RClosed -> WrappedSelf in
con WInfix: all self. self LOpen ROpen -> WrappedSelf in
con WPrefix: all self. self LClosed ROpen -> WrappedSelf in
con WPostfix: all self. self LOpen RClosed -> WrappedSelf in
type LOpenSelf self rstyle
in
con LInfix: all self. self LOpen ROpen -> LOpenSelf self ROpen in
con LPostfix: all self. self LOpen RClosed -> LOpenSelf self RClosed in
type PermanentNode self
in
con AtomP: all self. {id: PermanentId, idx: Int, self: self LClosed RClosed} -> PermanentNode self in
con InfixP: all self. {id: PermanentId, idx: Int, self: self LOpen ROpen, leftChildAlts: [PermanentNode self], rightChildAlts: [PermanentNode self]} -> PermanentNode self in
con PrefixP: all self. {id: PermanentId, idx: Int, self: self LClosed ROpen, rightChildAlts: [PermanentNode self]} -> PermanentNode self in
con PostfixP: all self. {id: PermanentId, idx: Int, self: self LOpen RClosed, leftChildAlts: [PermanentNode self]} -> PermanentNode self in
type TentativeData self
in
con InfixT: all self. {idx: Int, self: self LOpen ROpen, leftChildAlts: [PermanentNode self]} -> TentativeData self in
con PrefixT: all self. {idx: Int, self: self LClosed ROpen} -> TentativeData self in
type GroupingsAllowedFunc self =
  all lstyle. all rstyle. (self lstyle ROpen, self LOpen rstyle) -> AllowedDirection
in
type TopAllowedFunc self =
  all lstyle. all rstyle. self lstyle rstyle -> Bool
in
type LeftAllowedFunc self =
  all lstyle. all rstyle1. all rstyle2. {child: self lstyle rstyle2, parent: self LOpen rstyle1} -> Bool
in
type RightAllowedFunc self =
  all lstyle1. all lstyle2. all rstyle. {child: self lstyle2 rstyle, parent: self lstyle1 ROpen} -> Bool
in
type ParenAllowedFunc self =
  all lstyle. all rstyle. self lstyle rstyle -> AllowedDirection
in
type Config self =
  {topAllowed: TopAllowedFunc self, leftAllowed: LeftAllowedFunc self, parenAllowed: ParenAllowedFunc self, rightAllowed: RightAllowedFunc self, groupingsAllowed: GroupingsAllowedFunc self}
in
type TimeStep =
  Int
in
type TentativeNode self rstyle
in
con TentativeLeaf: all self. {node: PermanentNode self, parents: [TentativeNode self ROpen]} -> TentativeNode self RClosed in
con TentativeMid: all self. {parents: [TentativeNode self ROpen], tentativeData: TentativeData self, maxDistanceFromRoot: Int, addedNodesLeftChildren: Ref (TimeStep, Ref [PermanentNode self]), addedNodesRightChildren: Ref (TimeStep, [PermanentNode self])} -> TentativeNode self ROpen in
con TentativeRoot: all self. {addedNodesLeftChildren: Ref (TimeStep, Ref [PermanentNode self]), addedNodesRightChildren: Ref (TimeStep, [PermanentNode self])} -> TentativeNode self ROpen in
type State self rstyle =
  {nextIdx: Ref Int, frontier: [TentativeNode self rstyle], timestep: Ref TimeStep}
in
let _firstTimeStep: TimeStep = 0 in
let _isBefore: TimeStep -> TimeStep -> Bool = lti in
let _uniqueID: () -> PermanentId = gensym in
let _getParents: all self. all rstyle. TentativeNode self rstyle -> Option [TentativeNode self ROpen] =
  lam n.
    let unlink: all r. TentativeNode self rstyle -> TentativeNode self r = unsafeCoerce
    in
    match
      unlink n
    with
      TentativeLeaf {parents = ps}
    then
      Some
        ps
    else match
      unlink n
    with
      TentativeMid {parents = ps}
    then
      Some
        ps
    else match
      unlink n
    with
      TentativeRoot _
    in
    None
        {}
in
let _opIdxP: all self. PermanentNode self -> Int =
  lam node.
    match
      node
    with
      AtomP {idx = idx}
    then
      idx
    else match
      node
    with
      InfixP {idx = idx}
    then
      idx
    else match
      node
    with
      PrefixP {idx = idx}
    then
      idx
    else match
      node
    with
      PostfixP {idx = idx}
    in
    idx
in
let _addedNodesLeftChildren: all self. TentativeNode self ROpen -> Ref (TimeStep, Ref [PermanentNode self]) =
  lam node.
    match
      node
    with
      TentativeRoot {addedNodesLeftChildren = x} | TentativeMid {addedNodesLeftChildren = x}
    in
    x
in
let _addedNodesRightChildren: all self. TentativeNode self ROpen -> Ref (TimeStep, [PermanentNode self]) =
  lam node.
    match
      node
    with
      TentativeRoot {addedNodesRightChildren = x} | TentativeMid {addedNodesRightChildren = x}
    in
    x
in
let _callWithSelfP: all self. all x. (all lstyle. all rstyle. self lstyle rstyle -> x) -> PermanentNode self -> x =
  lam f.
    lam p.
      let #var"X" = p in
      match
        #var"X"
      with
        AtomP p
      then
        f p.self
      else match
        #var"X"
      with
        InfixP p
      then
        f p.self
      else match
        #var"X"
      with
        PrefixP p
      then
        f p.self
      else match
        #var"X"
      with
        PostfixP p
      in
      f p.self
in
let _isBrokenEdge: all self. TopAllowedFunc self -> Bool -> PermanentNode self -> Bool =
  lam isTopAllowed.
    lam parenAllowed.
      lam node.
        or
          (not parenAllowed)
          (not (_callWithSelfP #frozen"isTopAllowed" node))
in
let _leftChildrenP: all self. PermanentNode self -> Option [PermanentNode self] =
  lam p.
    let #var"X" = p in
    match
      #var"X"
    with
      InfixP r
    then
      Some
        r.leftChildAlts
    else match
      #var"X"
    with
      PostfixP r
    then
      Some
        r.leftChildAlts
    else match
      #var"X"
    with
      _
    in
    None
        {}
in
let _rightChildrenP: all self. PermanentNode self -> Option [PermanentNode self] =
  lam p.
    let #var"X" = p in
    match
      #var"X"
    with
      InfixP r
    then
      Some
        r.rightChildAlts
    else match
      #var"X"
    with
      PrefixP r
    then
      Some
        r.rightChildAlts
    else match
      #var"X"
    with
      _
    in
    None
        {}
in
let _brokenIdxesP: all self. TopAllowedFunc self -> ParenAllowedFunc self -> PermanentNode self -> [Int] =
  lam isTopAllowed.
    lam parenAllowedDirs.
      recursive
        let work =
          lam parenAllowed.
            lam p.
              match
                _isBrokenEdge #frozen"isTopAllowed" #frozen"parenAllowed" p
              with
                true
              then
                let parAllowed = _callWithSelfP #frozen"parenAllowedDirs" p in
                let l =
                  match
                    _leftChildrenP p
                  with
                    Some children
                  then
                    join (map (work (_includesLeft parAllowed)) children)
                  else
                    ""
                in
                let r =
                  match
                    _rightChildrenP p
                  with
                    Some children
                  then
                    join (map (work (_includesRight parAllowed)) children)
                  else
                    ""
                in
                join
                  [ l,
                    [ _opIdxP p ],
                    r ]
              else
                ""
      in
      work false
in
let _brokenChildrenP: all self. TopAllowedFunc self -> ParenAllowedFunc self -> PermanentNode self -> [PermanentNode self] =
  lam isTopAllowed.
    lam parenAllowedDirs.
      recursive
        let work =
          lam parenAllowed.
            lam p.
              match
                _isBrokenEdge #frozen"isTopAllowed" parenAllowed p
              with
                true
              then
                let parAllowed = _callWithSelfP #frozen"parenAllowedDirs" p in
                let l =
                  match
                    _leftChildrenP p
                  with
                    Some children
                  then
                    join (map (work (_includesLeft parAllowed)) children)
                  else
                    ""
                in
                let r =
                  match
                    _rightChildrenP p
                  with
                    Some children
                  then
                    join (map (work (_includesRight parAllowed)) children)
                  else
                    ""
                in
                concat l r
              else
                [ p ]
      in
      work false
in
let breakableInitState: all self. () -> State self ROpen =
  lam #var"".
    let timestep = ref _firstTimeStep in
    let nextIdx = ref 0 in
    let addedLeft = ref (_firstTimeStep, ref "") in
    let addedRight = ref (_firstTimeStep, "") in
    { nextIdx = nextIdx,
      frontier =
        [ TentativeRoot
            { addedNodesLeftChildren = addedLeft,
              addedNodesRightChildren = addedRight } ],
      timestep = timestep }
in
recursive
  let _maxDistanceFromRoot: all self. all rstyle. TentativeNode self rstyle -> Int =
    lam n.
      let unlink: all r. TentativeNode self rstyle -> TentativeNode self r = unsafeCoerce
      in
      match
        unlink n
      with
        TentativeMid {maxDistanceFromRoot = r}
      then
        r
      else match
        unlink n
      with
        TentativeRoot _
      then
        0
      else match
        unlink n
      with
        TentativeLeaf {parents = ps}
      in
      maxOrElse
          (lam #var"".
             0)
          subi
          (map _maxDistanceFromRoot ps)
in
let _shallowAllowedLeft: all self. all rstyle. LeftAllowedFunc self -> LOpenSelf self rstyle -> TentativeNode self RClosed -> Option (PermanentNode self) =
  lam allowedLeft.
    lam parent.
      lam child.
        match
          child
        with
          TentativeLeaf {node = node}
        in
        let helper =
            lam self.
              let unlink: all r. LOpenSelf self rstyle -> LOpenSelf self r = unsafeCoerce
              in
              match
                unlink parent
              with
                LInfix parent
              then
                allowedLeft { child = self, parent = parent }
              else match
                unlink parent
              with
                LPostfix parent
              in
              allowedLeft { child = self, parent = parent }
          in
          match
            _callWithSelfP #frozen"helper" node
          with
            true
          then
            Some
              node
          else
            None
              {}
in
let _shallowAllowedRight: all self. TopAllowedFunc self -> RightAllowedFunc self -> TentativeNode self ROpen -> TentativeNode self RClosed -> Option (PermanentNode self) =
  lam topAllowed.
    lam rightAllowed.
      lam parent.
        lam child.
          match
            child
          with
            TentativeLeaf {node = node}
          in
          let #var"X" = parent in
            match
              #var"X"
            with
              TentativeMid {tentativeData = InfixT {self = parent}}
            then
              let f =
                lam child.
                  rightAllowed { child = child, parent = parent }
              in
              match
                _callWithSelfP #frozen"f" node
              with
                true
              then
                Some
                  node
              else
                None
                  {}
            else match
              #var"X"
            with
              TentativeMid {tentativeData = PrefixT {self = parent}}
            then
              let f =
                lam child.
                  rightAllowed { child = child, parent = parent }
              in
              match
                _callWithSelfP #frozen"f" node
              with
                true
              then
                Some
                  node
              else
                None
                  {}
            else match
              #var"X"
            with
              TentativeRoot _
            in
            match
                _callWithSelfP #frozen"topAllowed" node
              with
                true
              then
                Some
                  node
              else
                None
                  {}
in
let _addRightChildren: all self. all rstyle. State self rstyle -> TentativeNode self ROpen -> [PermanentNode self] -> TentativeNode self RClosed =
  lam st.
    lam parent.
      lam children.
        match
          parent
        with
          TentativeMid {parents = parents, tentativeData = data}
        then
          let id = _uniqueID {} in
          let node =
            match
              data
            with
              InfixT {idx = idx, self = self, leftChildAlts = l}
            then
              InfixP
                { id = id,
                  idx = idx,
                  self = self,
                  leftChildAlts = l,
                  rightChildAlts = children }
            else match
              data
            with
              PrefixT {idx = idx, self = self}
            in
            PrefixP
                { id = id, idx = idx, self = self, rightChildAlts = children }
          in
          TentativeLeaf
            { node = node, parents = parents }
        else match
          parent
        with
          TentativeRoot _
        in
        error "Unexpectedly tried to add right children to the root"
in
let _addLeftChildren: all self. all rstyle. all rstyle2. State self rstyle2 -> LOpenSelf self rstyle -> [PermanentNode self] -> [TentativeNode self ROpen] -> TentativeNode self rstyle =
  lam st.
    lam lself.
      lam leftChildren.
        lam parents.
          let idx = deref st.nextIdx in
          let unlink: all r. LOpenSelf self rstyle -> LOpenSelf self r = unsafeCoerce
          in
          match
            unlink lself
          with
            LInfix self
          then
            let return: TentativeNode self ROpen -> TentativeNode self rstyle = unsafeCoerce
            in
            let time = deref st.timestep in
            let addedLeft = ref (_firstTimeStep, ref "") in
            let addedRight = ref (_firstTimeStep, "") in
            let res =
              TentativeMid
                { parents = parents,
                  tentativeData =
                    InfixT
                      { idx = idx, self = self, leftChildAlts = leftChildren },
                  maxDistanceFromRoot =
                    addi
                      1
                      (maxOrElse
                         (lam #var"".
                            0)
                         subi
                         (map _maxDistanceFromRoot parents)),
                  addedNodesLeftChildren = addedLeft,
                  addedNodesRightChildren = addedRight }
            in
            return res
          else match
            unlink lself
          with
            LPostfix self
          in
          let return: TentativeNode self RClosed -> TentativeNode self rstyle = unsafeCoerce
            in
            let id = _uniqueID {} in
            let res =
              TentativeLeaf
                { node =
                    PostfixP
                      { id = id, idx = idx, self = self, leftChildAlts = leftChildren },
                  parents = parents }
            in
            return res
in
let _addRightChildToParent: all self. TimeStep -> PermanentNode self -> TentativeNode self ROpen -> Option (TentativeNode self ROpen) =
  lam time.
    lam child.
      lam parent.
        let target = _addedNodesRightChildren parent in
        match
          deref target
        with
          (lastUpdate, prev)
        in
        match
            _isBefore lastUpdate time
          with
            true
          then
            (modref target (time, [ child ]))
            ; Some
              parent
          else
            (modref target (time, cons child prev))
            ; None
              {}
in
let _addLeftChildToParent: all self. TimeStep -> PermanentNode self -> [TentativeNode self ROpen] -> Option [TentativeNode self ROpen] =
  lam time.
    lam child.
      lam parents.
        match
          parents
        with
          [ p ] ++ _
        in
        let target = _addedNodesLeftChildren p in
          match
            deref target
          with
            (lastUpdate, prev)
          in
          match
              _isBefore lastUpdate time
            with
              true
            then
              let leftChildrenHere = ref [ child ] in
              (for_
                   parents
                   (lam p.
                      modref (_addedNodesLeftChildren p) (time, leftChildrenHere)))
              ; Some
                parents
            else
              (modref prev (cons child (deref prev)))
              ; None
                {}
in
let _getAllowedGroupings: all self. all rstyle. GroupingsAllowedFunc self -> TentativeNode self ROpen -> LOpenSelf self rstyle -> (Bool, Bool) =
  lam groupings.
    lam l.
      lam r.
        let #var"X" = l in
        match
          #var"X"
        with
          TentativeRoot _
        then
          (false, true)
        else match
          #var"X"
        with
          TentativeMid l
        in
        let dirs =
            let unlink: all r. LOpenSelf self rstyle -> LOpenSelf self r = unsafeCoerce
            in
            match
              (l.tentativeData, unlink r)
            with
              (InfixT l, LInfix r)
            then
              groupings (l.self, r)
            else match
              (l.tentativeData, unlink r)
            with
              (PrefixT l, LInfix r)
            then
              groupings (l.self, r)
            else match
              (l.tentativeData, unlink r)
            with
              (InfixT l, LPostfix r)
            then
              groupings (l.self, r)
            else match
              (l.tentativeData, unlink r)
            with
              (PrefixT l, LPostfix r)
            in
            groupings (l.self, r)
          in
          (_includesLeft dirs, _includesRight dirs)
in
type BreakableQueue self =
  Ref (Map Int [TentativeNode self ROpen])
in
let _newQueue: all self. () -> BreakableQueue self =
  lam #var"".
    ref
      (mapEmpty (lam x.
            lam y.
              subi y x))
in
let _addToQueue: all self. TentativeNode self ROpen -> BreakableQueue self -> () =
  lam node.
    lam queue.
      modref
        queue
        (mapInsertWith concat (_maxDistanceFromRoot node) [ node ] (deref queue))
in
let _popFromQueue: all self. BreakableQueue self -> Option (TentativeNode self ROpen) =
  lam queue.
    match
      mapGetMin (deref queue)
    with
      Some (k, [ v ] ++ vs)
    then
      let newQueue =
        match
          vs
        with
          ""
        then
          mapRemove k (deref queue)
        else
          mapInsert k vs (deref queue)
      in
      (modref queue newQueue)
      ; Some
        v
    else
      None
        {}
in
let _addLOpen: all self. all rstyle. Config self -> LOpenSelf self rstyle -> State self RClosed -> Option (State self rstyle) =
  lam config.
    lam lself.
      lam st.
        let nodesToProcess = _newQueue {} in
        let time = addi 1 (deref st.timestep) in
        (modref st.timestep time)
        ; let makeNewParents: [TentativeNode self ROpen] -> TentativeNode self rstyle =
          lam parents.
            match
              parents
            with
              [ p ] ++ _
            in
            let snd: all a. all b. (a, b) -> b = lam x.
                  x.1
              in
              let cs = deref (snd (deref (_addedNodesLeftChildren p))) in
              match
                cs
              with
                [ _ ] ++ _
              then
                _addLeftChildren st lself cs parents
              else
                error
                  "Somehow thought that a node would be a new parent, but it had no added children"
        in
        let handleLeaf: BreakableQueue self -> TentativeNode self RClosed -> Option [TentativeNode self ROpen] =
          lam queue.
            lam child.
              match
                _getParents child
              with
                Some parents
              in
              let lallowed: LeftAllowedFunc self = config.leftAllowed in
                let rallowed: RightAllowedFunc self = config.rightAllowed in
                let tallowed: TopAllowedFunc self = config.topAllowed in
                let gallowed: GroupingsAllowedFunc self = config.groupingsAllowed
                in
                let shallowRight = _shallowAllowedLeft #frozen"lallowed" lself child
                in
                let f =
                  lam parent.
                    let shallowLeft =
                      _shallowAllowedRight #frozen"tallowed" #frozen"rallowed" parent child
                    in
                    match
                      _getAllowedGroupings #frozen"gallowed" parent lself
                    with
                      (precLeft, precRight)
                    in
                    let config = (shallowLeft, shallowRight, precLeft, precRight)
                      in
                      (match
                           (parent, config)
                         with
                           (!TentativeRoot _, _) & ((_, (Some child, None _, _, _)) | (_, (Some child, _, true, _)))
                         then
                           match
                             _addRightChildToParent time child parent
                           with
                             Some parent
                           then
                             _addToQueue parent queue
                           else
                             {}
                         else
                           {})
                      ; match
                        config
                      with
                        (None _, Some child, _, _) | (_, Some child, _, true)
                      then
                        true
                      else
                        false
                in
                let parentsThatAllowRight = filter f parents in
                match
                  (shallowRight, parentsThatAllowRight)
                with
                  (Some child, parents & ([ _ ] ++ _))
                then
                  _addLeftChildToParent time child parents
                else
                  None
                    {}
        in
        recursive
          let work: BreakableQueue self -> [[TentativeNode self ROpen]] -> [[TentativeNode self ROpen]] =
            lam queue.
              lam acc.
                match
                  _popFromQueue queue
                with
                  Some (parent & TentativeMid {addedNodesRightChildren = addedRight})
                then
                  match
                    deref addedRight
                  with
                    (_, children & ([ _ ] ++ _))
                  then
                    let acc =
                      match
                        handleLeaf queue (_addRightChildren st parent children)
                      with
                        Some n
                      then
                        cons n acc
                      else
                        acc
                    in
                    work queue acc
                  else
                    error
                      "Somehow reached a parent without right children that was still added to the queue"
                else
                  acc
        in
        let frontier = st.frontier in
        let newParents = mapOption (handleLeaf nodesToProcess) frontier
        in
        let newParents = work nodesToProcess newParents in
        match
          map makeNewParents newParents
        with
          frontier & ([ _ ] ++ _)
        then
          Some
            { nextIdx = st.nextIdx,
              frontier = frontier,
              timestep = st.timestep }
        else
          None
            {}
in
let breakableAddPrefix: all self. Config self -> self LClosed ROpen -> State self ROpen -> State self ROpen =
  lam config.
    lam self.
      lam st.
        let frontier = st.frontier in
        let time = deref st.timestep in
        let idx = deref st.nextIdx in
        (modref st.nextIdx (addi 1 idx))
        ; let addedLeft = ref (_firstTimeStep, ref "") in
        let addedRight = ref (_firstTimeStep, "") in
        { st
          with
          frontier =
            [ TentativeMid
                { parents = frontier,
                  tentativeData =
                    PrefixT
                      { idx = idx, self = self },
                  maxDistanceFromRoot =
                    addi
                      1
                      (maxOrElse
                         (lam #var"".
                            0)
                         subi
                         (map _maxDistanceFromRoot frontier)),
                  addedNodesLeftChildren = addedLeft,
                  addedNodesRightChildren = addedRight } ] }
in
let breakableAddInfix: all self. Config self -> self LOpen ROpen -> State self RClosed -> Option (State self ROpen) =
  lam config.
    lam self.
      lam st.
        let res = _addLOpen config (LInfix
               self) st in
        (modref st.nextIdx (addi 1 (deref st.nextIdx)))
        ; res
in
let breakableAddPostfix: all self. Config self -> self LOpen RClosed -> State self RClosed -> Option (State self RClosed) =
  lam config.
    lam self.
      lam st.
        let res = _addLOpen config (LPostfix
               self) st in
        (modref st.nextIdx (addi 1 (deref st.nextIdx)))
        ; res
in
let breakableAddAtom: all self. Config self -> self LClosed RClosed -> State self ROpen -> State self RClosed =
  lam config.
    lam self.
      lam st.
        let idx = deref st.nextIdx in
        (modref st.nextIdx (addi 1 idx))
        ; let id = _uniqueID {} in
        { nextIdx = st.nextIdx,
          frontier =
            [ TentativeLeaf
                { node =
                    AtomP
                      { id = id, idx = idx, self = self },
                  parents = st.frontier } ],
          timestep = st.timestep }
in
let breakableFinalizeParse: all self. Config self -> State self RClosed -> Option [PermanentNode self] =
  lam config.
    lam st.
      let nodesToProcess = _newQueue {} in
      let time = addi 1 (deref st.timestep) in
      (modref st.timestep time)
      ; let rallowed: RightAllowedFunc self = config.rightAllowed in
      let tallowed: TopAllowedFunc self = config.topAllowed in
      let handleLeaf: BreakableQueue self -> TentativeNode self RClosed -> () =
        lam queue.
          lam child.
            match
              _getParents child
            with
              Some parents
            in
            for_
                parents
                (lam parent.
                   match
                     _shallowAllowedRight #frozen"tallowed" #frozen"rallowed" parent child
                   with
                     Some child
                   then
                     match
                       _addRightChildToParent time child parent
                     with
                       Some parent
                     then
                       _addToQueue parent queue
                     else
                       {}
                   else
                     {})
      in
      recursive
        let work: BreakableQueue self -> [PermanentNode self] =
          lam queue.
            match
              _popFromQueue queue
            with
              Some p
            then
              let snd: all a. all b. (a, b) -> b = lam x.
                  x.1
              in
              let children = snd (deref (_addedNodesRightChildren p)) in
              match
                p
              with
                TentativeRoot _
              then
                children
              else match
                (p, children)
              with
                (TentativeMid _, [ _ ] ++ _)
              then
                (handleLeaf queue (_addRightChildren st p children))
                ; work queue
              else match
                p
              with
                TentativeMid _
              in
              error
                  "Somehow reached a TentativeMid without right children, that was still added to the queue"
            else
              ""
      in
      let frontier = st.frontier in
      (iter (handleLeaf nodesToProcess) frontier)
      ; match
        work nodesToProcess
      with
        res & ([ _ ] ++ _)
      then
        Some
          res
      else
        None
          {}
in
type Ambiguity pos tokish =
  {range: {last: pos, first: pos}, partialResolutions: [[tokish]]}
in
type Important
in
con Important: () -> Important in
con Unimportant: () -> Important in
let breakableReportAmbiguities: all self. all pos. all tokish. {lpar: tokish, rpar: tokish, toTok: all lstyle. all rstyle. Important -> self lstyle rstyle -> [tokish], leftPos: all rstyle. self LClosed rstyle -> pos, rightPos: all lstyle. self lstyle RClosed -> pos, topAllowed: TopAllowedFunc self, parenAllowed: ParenAllowedFunc self} -> [PermanentNode self] -> [Ambiguity pos tokish] =
  lam info.
    lam nodes.
      let pallowed: ParenAllowedFunc self = info.parenAllowed in
      let tallowed: TopAllowedFunc self = info.topAllowed in
      recursive
        let range: PermanentNode self -> {last: pos, first: pos} =
          lam node.
            let #var"X" = node in
            match
              #var"X"
            with
              AtomP {self = self}
            then
              { last = info.rightPos self, first = info.leftPos self }
            else match
              #var"X"
            with
              InfixP {leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _}
            then
              { last = (range r).last, first = (range l).first }
            else match
              #var"X"
            with
              PrefixP {self = self, rightChildAlts = [ r ] ++ _}
            then
              { last = (range r).last, first = info.leftPos self }
            else match
              #var"X"
            with
              PostfixP {self = self, leftChildAlts = [ l ] ++ _}
            in
            { last = info.rightPos self, first = (range l).first }
      in
      type IdxSet =
        [Int]
      in
      let leftOf: Int -> IdxSet -> IdxSet = lam idx.
          lam set.
            filter (gti idx) set
      in
      let rightOf: Int -> IdxSet -> IdxSet = lam idx.
          lam set.
            filter (lti idx) set
      in
      let inIdxSet: Int -> IdxSet -> Bool = lam idx.
          lam set.
            any (eqi idx) set
      in
      let idxSetEmpty: IdxSet -> Bool = lam set.
          null set
      in
      let isImportant: PermanentNode self -> IdxSet -> Important =
        lam p.
          lam set.
            match
              inIdxSet (_opIdxP p) set
            with
              true
            then
              Important
                {}
            else
              Unimportant
                {}
      in
      let idxAndImportant: PermanentNode self -> IdxSet -> (IdxSet, Important, IdxSet) =
        lam p.
          lam set.
            let idx = _opIdxP p in
            (leftOf idx set, match
              inIdxSet idx set
            with
              true
            then
              Important
                {}
            else
              Unimportant
                {}, rightOf idx set)
      in
      recursive
        let flattenOne: IdxSet -> PermanentNode self -> [tokish] =
          lam important.
            lam node.
              let isImportant = isImportant node important in
              let #var"X" = node in
              match
                #var"X"
              with
                AtomP {self = self}
              then
                info.toTok isImportant self
              else match
                #var"X"
              with
                InfixP p
              then
                join
                  [ flattenMany important p.leftChildAlts,
                    info.toTok isImportant p.self,
                    flattenMany important p.rightChildAlts ]
              else match
                #var"X"
              with
                PrefixP p
              then
                concat
                  (info.toTok isImportant p.self)
                  (flattenMany important p.rightChildAlts)
              else match
                #var"X"
              with
                PostfixP p
              in
              concat
                  (flattenMany important p.leftChildAlts)
                  (info.toTok isImportant p.self)
        let flattenMany: IdxSet -> [PermanentNode self] -> [tokish] =
          lam important.
            lam nodes.
              match
                nodes
              with
                [ n ] ++ _
              in
              flattenOne important n
      in
      recursive
        let resolveTopOne: IdxSet -> PermanentNode self -> [[tokish]] =
          lam topIdxs.
            lam p.
              match
                idxAndImportant p topIdxs
              with
                (lIdxs, selfImportant, rIdxs)
              in
              let parAllowed = _callWithSelfP #frozen"pallowed" p in
                let l =
                  match
                    _leftChildrenP p
                  with
                    Some children
                  then
                    resolveTopMany lIdxs (_includesLeft parAllowed) children
                  else
                    [ "" ]
                in
                let r =
                  match
                    _rightChildrenP p
                  with
                    Some children
                  then
                    resolveTopMany rIdxs (_includesRight parAllowed) children
                  else
                    [ "" ]
                in
                let f = lam x.
                    info.toTok selfImportant x
                in
                let here = _callWithSelfP #frozen"f" p in
                seqLiftA2
                  (lam l.
                     lam r.
                       join
                         [ l,
                           here,
                           r ])
                  l
                  r
        let resolveTopMany: [Int] -> Bool -> [PermanentNode self] -> [[tokish]] =
          lam topIdxs.
            lam parenAllowed.
              lam ps.
                match
                  partition (_isBrokenEdge #frozen"tallowed" parenAllowed) ps
                with
                  (broken, whole)
                in
                let broken = join (map (resolveTopOne topIdxs) broken) in
                  let whole =
                    match
                      null whole
                    with
                      true
                    then
                      ""
                    else
                      let flattened = flattenMany topIdxs whole in
                      match
                        idxSetEmpty topIdxs
                      with
                        true
                      then
                        [ flattened ]
                      else
                        [ snoc (cons info.lpar flattened) info.rpar ]
                  in
                  concat broken whole
      in
      let ambiguities: Ref [Ambiguity pos tokish] = ref "" in
      recursive
        let workMany: Option (PermanentNode self) -> Bool -> [PermanentNode self] -> () =
          lam brokenParent.
            lam parenAllowed.
              lam tops.
                match
                  tops
                with
                  [ n ]
                then
                  workOne
                    (match
                       _isBrokenEdge #frozen"tallowed" parenAllowed n
                     with
                       true
                     then
                       brokenParent
                     else
                       None
                         {})
                    n
                else match
                  tops
                with
                  [ n ] ++ _
                then
                  let x =
                    match
                      (any (_isBrokenEdge #frozen"tallowed" parenAllowed) tops, brokenParent)
                    with
                      (true, Some parent)
                    then
                      ([ parent ], range parent)
                    else
                      (tops, range n)
                  in
                  let tops = x.0 in
                  let range = x.1 in
                  let topIdxs =
                    setOfSeq
                      subi
                      (join
                         (map (_brokenIdxesP #frozen"tallowed" #frozen"pallowed") tops))
                  in
                  recursive
                    let addChildMaybe =
                      lam acc.
                        lam c.
                          let idxs = _brokenIdxesP #frozen"tallowed" #frozen"pallowed" c
                          in
                          let acc =
                            match
                              any
                                (lam x.
                                   setMem x topIdxs)
                                idxs
                            with
                              true
                            then
                              foldl
                                (lam acc.
                                   lam x.
                                     setInsert x acc)
                                acc
                                idxs
                            else
                              acc
                          in
                          foldl
                            addChildMaybe
                            acc
                            (_brokenChildrenP #frozen"tallowed" #frozen"pallowed" c)
                  in
                  let addChildrenMaybe =
                    lam acc.
                      lam top.
                        foldl
                          addChildMaybe
                          acc
                          (_brokenChildrenP #frozen"tallowed" #frozen"pallowed" top)
                  in
                  let mergeRootIdxs = foldl addChildrenMaybe (setEmpty subi) tops
                  in
                  let idxesToCover = setToSeq (setUnion mergeRootIdxs topIdxs) in
                  let resolutions: [[tokish]] = resolveTopMany idxesToCover false tops
                  in
                  let err = { range = range, partialResolutions = resolutions }
                  in
                  (modref ambiguities (cons err (deref ambiguities)))
                  ; {}
                else
                  (dprintLn tops)
                  ; never
        let workOne: Option (PermanentNode self) -> PermanentNode self -> () =
          lam brokenParent.
            lam node.
              let parAllowed = _callWithSelfP #frozen"pallowed" node in
              (match
                   _leftChildrenP node
                 with
                   Some children
                 then
                   workMany
                     (optionOr brokenParent (Some
                           node))
                     (_includesLeft parAllowed)
                     children
                 else
                   {})
              ; (match
                   _rightChildrenP node
                 with
                   Some children
                 then
                   workMany
                     (optionOr brokenParent (Some
                           node))
                     (_includesRight parAllowed)
                     children
                 else
                   {})
              ; {}
      in
      (workMany (None
              {}) false nodes)
      ; deref ambiguities
in
type BreakableErrorHighlightConfig self =
  {lpar: [Char], rpar: [Char], getInfo: all lstyle. all rstyle. self lstyle rstyle -> Info, topAllowed: TopAllowedFunc self, parenAllowed: ParenAllowedFunc self, terminalInfos: all lstyle. all rstyle. self lstyle rstyle -> [Info]}
in
let breakableToErrorHighlightSpec: all self. BreakableErrorHighlightConfig self -> [PermanentNode self] -> [{info: Info, partialResolutions: [[Highlight]]}] =
  lam config.
    lam tops.
      let pallowed: ParenAllowedFunc self = config.parenAllowed in
      let tallowed: TopAllowedFunc self = config.topAllowed in
      let linfo: all rstyle. self LClosed rstyle -> Info = config.getInfo
      in
      let rinfo: all lstyle. self lstyle RClosed -> Info = config.getInfo
      in
      let totok: all lstyle. all rstyle. Important -> self lstyle rstyle -> [Highlight] =
        lam imp.
          lam self.
            match
              imp
            with
              Important _
            then
              map
                (lam info.
                   Relevant
                     info)
                (config.terminalInfos self)
            else
              [ Irrelevant
                  (config.getInfo self) ]
      in
      let reportConfig =
        { topAllowed = #frozen"tallowed",
          parenAllowed = #frozen"pallowed",
          lpar =
            Added
              { content = config.lpar, ensureSurroundedBySpaces = true },
          rpar =
            Added
              { content = config.rpar, ensureSurroundedBySpaces = true },
          toTok = #frozen"totok",
          leftPos = #frozen"linfo",
          rightPos = #frozen"rinfo" }
      in
      let res = breakableReportAmbiguities reportConfig tops in
      let fixup =
        lam amb: Ambiguity Info Highlight.
          let amb: Ambiguity Info Highlight = amb in
          { info = mergeInfo amb.range.first amb.range.last,
            partialResolutions = amb.partialResolutions }
      in
      map fixup res
in
let breakableDefaultHighlight: all self. BreakableErrorHighlightConfig self -> [Char] -> [PermanentNode self] -> [(Info, [Char])] =
  lam config.
    lam content.
      lam tops.
        let highlightOne =
          lam amb: {info: Info, partialResolutions: [[Highlight]]}.
            let alternatives =
              map
                (formatHighlights terminalHighlightAddedConfig content)
                amb.partialResolutions
            in
            let body =
              match
                match
                  amb.info
                with
                  Info x
                then
                  eqi x.row1 x.row2
                else
                  false
              with
                true
              then
                strJoin "\n" (map (concat "  ") alternatives)
              else
                strJoin
                  "\n\n"
                  (mapi
                     (lam i.
                        lam str.
                          join
                            [ "Alternative ",
                              int2string (addi i 1),
                              ":\n",
                              str ])
                     alternatives)
            in
            (amb.info, join
              [ "Ambiguity error\n",
                body,
                "\n" ])
        in
        map highlightOne (breakableToErrorHighlightSpec config tops)
in
type CalcBaseAst_DSLStatement
in
type CalcBaseAst_DSLExpr
in
type CalcBaseAst_File
in
type FileAst_FileRecord =
  {s: [CalcBaseAst_DSLStatement], info: Info}
in
con FileAst_File1: FileAst_FileRecord -> CalcBaseAst_File in
type AssignmentDSLStatementAst_AssignmentDSLStatementRecord =
  {val: CalcBaseAst_DSLExpr, info: Info, ident: {i: Info, v: [Char]}}
in
con AssignmentDSLStatementAst_AssignmentDSLStatement: AssignmentDSLStatementAst_AssignmentDSLStatementRecord -> CalcBaseAst_DSLStatement in
type NumDSLExprAst_NumDSLExprRecord =
  {val: {i: Info, v: Float}, info: Info}
in
con NumDSLExprAst_NumDSLExpr: NumDSLExprAst_NumDSLExprRecord -> CalcBaseAst_DSLExpr in
type VariableDSLExprAst_VariableDSLExprRecord =
  {info: Info, ident: {i: Info, v: [Char]}}
in
con VariableDSLExprAst_VariableDSLExpr: VariableDSLExprAst_VariableDSLExprRecord -> CalcBaseAst_DSLExpr in
type AddDSLExprAst_AddDSLExprRecord =
  {info: Info, left: CalcBaseAst_DSLExpr, right: CalcBaseAst_DSLExpr}
in
con AddDSLExprAst_AddDSLExpr: AddDSLExprAst_AddDSLExprRecord -> CalcBaseAst_DSLExpr in
type SubDSLExprAst_SubDSLExprRecord =
  {info: Info, left: CalcBaseAst_DSLExpr, right: CalcBaseAst_DSLExpr}
in
con SubDSLExprAst_SubDSLExpr: SubDSLExprAst_SubDSLExprRecord -> CalcBaseAst_DSLExpr in
type MulDSLExprAst_MulDSLExprRecord =
  {info: Info, left: CalcBaseAst_DSLExpr, right: CalcBaseAst_DSLExpr}
in
con MulDSLExprAst_MulDSLExpr: MulDSLExprAst_MulDSLExprRecord -> CalcBaseAst_DSLExpr in
type DivDSLExprAst_DivDSLExprRecord =
  {info: Info, left: CalcBaseAst_DSLExpr, right: CalcBaseAst_DSLExpr}
in
con DivDSLExprAst_DivDSLExpr: DivDSLExprAst_DivDSLExprRecord -> CalcBaseAst_DSLExpr in
type BadFileAst_BadFileRecord =
  {info: Info}
in
con BadFileAst_BadFile: BadFileAst_BadFileRecord -> CalcBaseAst_File in
type BadDSLStatementAst_BadDSLStatementRecord =
  {info: Info}
in
con BadDSLStatementAst_BadDSLStatement: BadDSLStatementAst_BadDSLStatementRecord -> CalcBaseAst_DSLStatement in
type BadDSLExprAst_BadDSLExprRecord =
  {info: Info}
in
con BadDSLExprAst_BadDSLExpr: BadDSLExprAst_BadDSLExprRecord -> CalcBaseAst_DSLExpr in
type FileOpBase_FileOp a a
in
type DSLStatementOpBase_DSLStatementOp a a
in
type DSLExprOpBase_DSLExprOp a a
in
con FileOp1_FileOp1: all lstyle. all rstyle. {s: [CalcBaseAst_DSLStatement], __br_info: Info, __br_terms: [Info]} -> FileOpBase_FileOp lstyle rstyle in
con AssignmentDSLStatementOp_AssignmentDSLStatementOp: all lstyle. all rstyle. {val: [CalcBaseAst_DSLExpr], ident: [{i: Info, v: [Char]}], __br_info: Info, __br_terms: [Info]} -> DSLStatementOpBase_DSLStatementOp lstyle rstyle in
con NumDSLExprOp_NumDSLExprOp: all lstyle. all rstyle. {val: [{i: Info, v: Float}], __br_info: Info, __br_terms: [Info]} -> DSLExprOpBase_DSLExprOp lstyle rstyle in
con VariableDSLExprOp_VariableDSLExprOp: all lstyle. all rstyle. {ident: [{i: Info, v: [Char]}], __br_info: Info, __br_terms: [Info]} -> DSLExprOpBase_DSLExprOp lstyle rstyle in
con AddDSLExprOp_AddDSLExprOp: all lstyle. all rstyle. {__br_info: Info, __br_terms: [Info]} -> DSLExprOpBase_DSLExprOp lstyle rstyle in
con SubDSLExprOp_SubDSLExprOp: all lstyle. all rstyle. {__br_info: Info, __br_terms: [Info]} -> DSLExprOpBase_DSLExprOp lstyle rstyle in
con MulDSLExprOp_MulDSLExprOp: all lstyle. all rstyle. {__br_info: Info, __br_terms: [Info]} -> DSLExprOpBase_DSLExprOp lstyle rstyle in
con DivDSLExprOp_DivDSLExprOp: all lstyle. all rstyle. {__br_info: Info, __br_terms: [Info]} -> DSLExprOpBase_DSLExprOp lstyle rstyle in
con DSLExprGrouping_DSLExprGrouping: all lstyle. all rstyle. {inner: CalcBaseAst_DSLExpr, __br_info: Info, __br_terms: [Info]} -> DSLExprOpBase_DSLExprOp lstyle rstyle in
recursive
  let vParseCalc_ntSym =
    lam __sem_target.
      match
        __sem_target
      with
        nt
      in
      NtSpec
          nt
  let vParseCalc_litSym =
    lam __sem_target.
      match
        __sem_target
      with
        str
      in
      let res: TokenParser_NextTokenResult =
          vParseCalc_nextToken { pos = posVal (concat "lit: " str) 1 1, str = str }
        in
        match
          (res.stream.str, res.lit)
        with
          ("", !"")
        then
          LitSpec
            { lit = str }
        else
          error
            (join
               [ "A literal token does not lex as a single token: \"",
                 str,
                 "\" leaves \"",
                 res.stream.str,
                 "\"" ])
  let vParseCalc_tokSym =
    lam __sem_target.
      match
        __sem_target
      with
        repr
      in
      TokSpec
          repr
  let vParseCalc_eatWSAC =
    lam p: Pos.
      lam __sem_target.
        match
          __sem_target
        with
          " " ++ xs
        then
          vParseCalc_eatWSAC (advanceCol p 1) xs
        else match
          __sem_target
        with
          "\t" ++ xs
        then
          vParseCalc_eatWSAC (advanceCol p tabSpace) xs
        else match
          __sem_target
        with
          "\n" ++ xs
        then
          vParseCalc_eatWSAC (advanceRow p 1) xs
        else match
          __sem_target
        with
          "\r" ++ xs
        then
          vParseCalc_eatWSAC p xs
        else match
          __sem_target
        with
          "--" ++ xs
        then
          recursive
            let remove =
              lam p.
                lam str.
                  match
                    str
                  with
                    "\n" ++ xs
                  then
                    vParseCalc_eatWSAC (advanceRow p 1) xs
                  else match
                    str
                  with
                    [ x ] ++ xs
                  then
                    remove (advanceCol p 1) xs
                  else
                    vParseCalc_eatWSAC p str
          in
          remove p xs
        else match
          __sem_target
        with
          "/-" ++ xs
        then
          recursive
            let remove =
              lam p.
                lam str.
                  lam d.
                    match
                      str
                    with
                      "/-" ++ xs
                    then
                      remove (advanceCol p 2) xs (addi d 1)
                    else match
                      str
                    with
                      "\n" ++ xs
                    then
                      remove (advanceRow p 1) xs d
                    else match
                      str
                    with
                      "-/" ++ xs
                    then
                      match
                        eqi d 1
                      with
                        true
                      then
                        vParseCalc_eatWSAC (advanceCol p 2) xs
                      else
                        remove (advanceCol p 2) xs (subi d 1)
                    else match
                      str
                    with
                      [ _ ] ++ xs
                    then
                      remove (advanceCol p 1) xs d
                    else match
                      eqi d 0
                    with
                      true
                    then
                      vParseCalc_eatWSAC p str
                    else
                      posErrorExit p "Unmatched multiline comments."
          in
          remove (advanceCol p 2) xs 1
        else match
          __sem_target
        with
          x
        in
        { pos = p, str = x }
  let vParseCalc_nextToken =
    lam __sem_target.
      match
        __sem_target
      with
        stream
      in
      let stream: TokenParser_Stream = stream in
        let stream: TokenParser_Stream = vParseCalc_eatWSAC stream.pos stream.str
        in
        vParseCalc_parseToken stream.pos stream.str
  let vParseCalc_parseToken =
    lam pos: Pos.
      lam __sem_target.
        match
          __sem_target
        with
          ""
        then
          let info = makeInfo pos pos in
          { info = info,
            lit = "",
            token = EOFTokenParser_EOFTok
                { info = info },
            stream = { pos = pos, str = "" } }
        else match
          __sem_target
        with
          [ ('_' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z') & c ] ++ str
        then
          match
            parseIdentCont (advanceCol pos 1) str
          with
            {val = val, pos = pos2, str = str}
          in
          let val = cons c val in
            let info = makeInfo pos pos2 in
            { info = info,
              lit = val,
              token =
                LIdentTokenParser_LIdentTok
                  { info = info, val = val },
              stream = { pos = pos2, str = str } }
        else match
          __sem_target
        with
          [ ('A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z') & c ] ++ str
        then
          match
            parseIdentCont (advanceCol pos 1) str
          with
            {val = val, pos = pos2, str = str}
          in
          let val = cons c val in
            let info = makeInfo pos pos2 in
            { info = info,
              lit = val,
              token =
                UIdentTokenParser_UIdentTok
                  { info = info, val = val },
              stream = { pos = pos2, str = str } }
        else match
          __sem_target
        with
          ([ '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ] ++ _) & str
        then
          match
            parseUInt pos str
          with
            {val = val, pos = pos2, str = str}
          in
          vParseCalc_parseIntCont val pos pos2 str
        else match
          __sem_target
        with
          [ ('%' | '<' | '>' | '!' | '?' | '~' | ':' | '.' | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '@' | '^' | '|') & c ] ++ str
        then
          match
            parseOperatorCont (advanceCol pos 1) str
          with
            {val = val, stream = stream}
          in
          let val = cons c val in
            let info = makeInfo pos stream.pos in
            { info = info,
              lit = val,
              token =
                OperatorTokenParser_OperatorTok
                  { info = info, val = val },
              stream = stream }
        else match
          __sem_target
        with
          "(" ++ str
        then
          let pos2 = advanceCol pos 1 in
          let info = makeInfo pos pos2 in
          { info = info,
            lit = "(",
            token =
              BracketTokenParser_LParenTok
                { info = info },
            stream = { pos = pos2, str = str } }
        else match
          __sem_target
        with
          ")" ++ str
        then
          let pos2 = advanceCol pos 1 in
          let info = makeInfo pos pos2 in
          { info = info,
            lit = ")",
            token =
              BracketTokenParser_RParenTok
                { info = info },
            stream = { pos = pos2, str = str } }
        else match
          __sem_target
        with
          "[" ++ str
        then
          let pos2 = advanceCol pos 1 in
          let info = makeInfo pos pos2 in
          { info = info,
            lit = "[",
            token =
              BracketTokenParser_LBracketTok
                { info = info },
            stream = { pos = pos2, str = str } }
        else match
          __sem_target
        with
          "]" ++ str
        then
          let pos2 = advanceCol pos 1 in
          let info = makeInfo pos pos2 in
          { info = info,
            lit = "]",
            token =
              BracketTokenParser_RBracketTok
                { info = info },
            stream = { pos = pos2, str = str } }
        else match
          __sem_target
        with
          "{" ++ str
        then
          let pos2 = advanceCol pos 1 in
          let info = makeInfo pos pos2 in
          { info = info,
            lit = "{",
            token =
              BracketTokenParser_LBraceTok
                { info = info },
            stream = { pos = pos2, str = str } }
        else match
          __sem_target
        with
          "}" ++ str
        then
          let pos2 = advanceCol pos 1 in
          let info = makeInfo pos pos2 in
          { info = info,
            lit = "}",
            token =
              BracketTokenParser_RBraceTok
                { info = info },
            stream = { pos = pos2, str = str } }
        else match
          __sem_target
        with
          ";" ++ str
        then
          let pos2 = advanceCol pos 1 in
          let info = makeInfo pos pos2 in
          { info = info,
            lit = ";",
            token =
              SemiTokenParser_SemiTok
                { info = info },
            stream = { pos = pos2, str = str } }
        else match
          __sem_target
        with
          "," ++ str
        then
          let pos2 = advanceCol pos 1 in
          let info = makeInfo pos pos2 in
          { info = info,
            lit = ",",
            token =
              CommaTokenParser_CommaTok
                { info = info },
            stream = { pos = pos2, str = str } }
        else match
          __sem_target
        with
          "\"" ++ str
        in
        recursive
            let work =
              lam acc.
                lam p2.
                  lam str.
                    match
                      str
                    with
                      "\"" ++ str
                    then
                      { val = acc, pos = advanceCol p2 1, str = str }
                    else match
                      matchChar p2 str
                    with
                      {val = charval, pos = p2, str = str}
                    in
                    work (snoc acc charval) p2 str
          in
          match
            work "" (advanceCol pos 1) str
          with
            {val = val, pos = pos2, str = str}
          in
          let info = makeInfo pos pos2 in
            { info = info,
              lit = "",
              token =
                StringTokenParser_StringTok
                  { info = info, val = val },
              stream = { pos = pos2, str = str } }
  let vParseCalc_tokReprCmp =
    lam l.
      lam __sem_target.
        match
          __sem_target
        with
          r
        in
        vParseCalc_tokReprCmp2 (l, r)
  let vParseCalc_tokReprCmp2: (TokenReprBase_TokenRepr, TokenReprBase_TokenRepr) -> Int =
    lam __sem_target.
      match
        __sem_target
      with
        (l, r)
      in
      subi (constructorTag l) (constructorTag r)
  let vParseCalc_eqSpecSymbol: all state. all prodLabel. SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel -> SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel -> Bool =
    lam l.
      lam __sem_target.
        match
          __sem_target
        with
          r
        in
        eqi 0 (vParseCalc__compareSpecSymbol (l, r))
  let vParseCalc_parseIntCont =
    lam acc: [Char].
      lam pos1: Pos.
        lam pos2: Pos.
          lam __sem_target.
            match
              __sem_target
            with
              ([ '.',
                 '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ] ++ _) & str
            then
              match
                parseFloatExponent (advanceCol pos2 1) (tail str)
              with
                {val = val, pos = pos3, str = str}
              in
              let acc =
                  join
                    [ acc,
                      ".",
                      val ]
                in
                vParseCalc_parseFloatCont acc pos1 pos3 str
            else match
              __sem_target
            with
              "." ++ str
            then
              vParseCalc_parseFloatCont acc pos1 (advanceCol pos2 1) str
            else match
              __sem_target
            with
              ([ 'e' | 'E' ] ++ _) & ([ _,
                 '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ] ++ _ | [ _,
                 '+' | '-',
                 '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ] ++ _) & str
            then
              vParseCalc_parseFloatCont acc pos1 pos2 str
            else match
              __sem_target
            with
              str
            in
            let info = makeInfo pos1 pos2 in
              { info = info,
                lit = "",
                token =
                  UIntTokenParser_IntTok
                    { info = info, val = string2int acc },
                stream = { pos = pos2, str = str } }
  let vParseCalc_getInfo_FileOp: all lstyle. all rstyle. FileOpBase_FileOp lstyle rstyle -> Info =
    lam __sem_target.
      match
        __sem_target
      with
        FileOp1_FileOp1 x
      in
      x.__br_info
  let vParseCalc_parseFloatCont =
    lam acc: [Char].
      lam pos1: Pos.
        lam pos2: Pos.
          lam __sem_target.
            match
              __sem_target
            with
              ([ ('e' | 'E') & e ] ++ _) & ([ _,
                 '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ] ++ _ | [ _,
                 '+' | '-',
                 '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ] ++ _) & str
            then
              match
                parseFloatExponent (advanceCol pos2 1) (tail str)
              with
                {val = val, pos = pos2, str = str}
              in
              let info = makeInfo pos1 pos2 in
                { info = info,
                  lit = "",
                  token =
                    UFloatTokenParser_FloatTok
                      { info = info,
                        val =
                          string2float
                            (join
                               [ acc,
                                 "e",
                                 val ]) },
                  stream = { pos = pos2, str = str } }
            else match
              __sem_target
            with
              str
            in
            let info = makeInfo pos1 pos2 in
              { info = info,
                lit = "",
                token =
                  UFloatTokenParser_FloatTok
                    { info = info, val = string2float acc },
                stream = { pos = pos2, str = str } }
  let vParseCalc_unsplit_FileOp: PermanentNode FileOpBase_FileOp -> (Info, CalcBaseAst_File) =
    lam __sem_target.
      match
        __sem_target
      with
        AtomP {self = FileOp1_FileOp1 x}
      in
      (x.__br_info, FileAst_File1
          { info = x.__br_info, s = x.s })
  let vParseCalc_genParsingTable: all prodLabel. all state. ParserConcrete_Grammar prodLabel state -> Either (GenError prodLabel state) (ParserConcrete_Table prodLabel state) =
    lam __sem_target.
      match
        __sem_target
      with
        grammar
      in
      match
          grammar
        with
          {productions = productions, start = startNt}
        in
        let emptySymSet =
            { syms = mapEmpty vParseCalc_compareSpecSymbol, eps = false }
          in
          let eqSymSet =
            lam s1: ParserBase_SymSet state prodLabel.
              lam s2: ParserBase_SymSet state prodLabel.
                match
                  (s1, s2)
                with
                  ({syms = s1, eps = e1}, {syms = s2, eps = e2})
                then
                  and
                    (eqBool e1 e2)
                    (eqSeq
                       (lam a.
                          lam b.
                            vParseCalc_eqSpecSymbol a.0 b.0)
                       (mapBindings s1)
                       (mapBindings s2))
                else
                  (dprintLn (s1, s2))
                  ; never
          in
          let eqFirstSet =
            lam s1.
              lam s2.
                eqSeq
                  (lam a.
                     lam b.
                       match
                         nameEq a.0 b.0
                       with
                         true
                       then
                         eqSymSet a.1 b.1
                       else
                         false)
                  (mapBindings s1)
                  (mapBindings s2)
          in
          let eqFollowSymSet =
            lam s1.
              lam s2.
                eqSeq
                  (lam a.
                     lam b.
                       vParseCalc_eqSpecSymbol a.0 b.0)
                  (mapBindings s1)
                  (mapBindings s2)
          in
          let eqFollowSet =
            lam s1.
              lam s2.
                eqSeq
                  (lam a.
                     lam b.
                       match
                         nameEq a.0 b.0
                       with
                         true
                       then
                         eqFollowSymSet a.1 b.1
                       else
                         false)
                  (mapBindings s1)
                  (mapBindings s2)
          in
          let addProdToFirst: Map Name (ParserBase_SymSet state prodLabel) -> ParserConcrete_Production prodLabel state -> ParserBase_SymSet state prodLabel -> ParserBase_SymSet state prodLabel =
            lam prev.
              lam prod.
                lam symset.
                  recursive
                    let work =
                      lam symset: ParserBase_SymSet state prodLabel.
                        lam rhs.
                          match
                            rhs
                          with
                            ""
                          then
                            { symset with eps = true }
                          else match
                            rhs
                          with
                            [ (TokSpec _ | LitSpec _) & t ] ++ _
                          then
                            { symset with syms = mapInsert t {} symset.syms }
                          else match
                            rhs
                          with
                            [ NtSpec nt ] ++ rhs
                          in
                          let otherSymset =
                              match
                                mapLookup nt prev
                              with
                                Some s
                              then
                                s
                              else
                                emptySymSet
                            in
                            let symset = { symset with syms = mapUnion symset.syms otherSymset.syms }
                            in
                            match
                              otherSymset.eps
                            with
                              true
                            then
                              work symset rhs
                            else
                              symset
                  in
                  work symset prod.rhs
          in
          let groupedProds: Map Name [ParserConcrete_Production prodLabel state] =
            foldl
              (lam acc.
                 lam prod: ParserConcrete_Production prodLabel state.
                   mapInsert
                     prod.nt
                     (snoc (optionGetOr "" (mapLookup prod.nt acc)) prod)
                     acc)
              (mapEmpty nameCmp)
              productions
          in
          let addNtToFirstSet =
            lam prev.
              lam nt.
                lam symset.
                  let prods = mapFindExn nt groupedProds in
                  foldl
                    (lam symset.
                       lam prod.
                         addProdToFirst prev prod symset)
                    symset
                    prods
          in
          let firstSet: Map Name (ParserBase_SymSet state prodLabel) =
            _iterateUntilFixpoint
              eqFirstSet
              (lam prev.
                 mapMapWithKey (addNtToFirstSet prev) prev)
              (mapMap (lam #var"".
                    emptySymSet) groupedProds)
          in
          let firstOfRhs: [SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel] -> ParserBase_SymSet state prodLabel =
            recursive
              let work =
                lam symset: ParserBase_SymSet state prodLabel.
                  lam rhs.
                    match
                      rhs
                    with
                      ""
                    then
                      { symset with eps = true }
                    else match
                      rhs
                    with
                      [ (TokSpec _ | LitSpec _) & t ] ++ _
                    then
                      { symset with syms = mapInsert t {} symset.syms }
                    else match
                      rhs
                    with
                      [ NtSpec nt | NtSym {nt = nt} ] ++ rhs
                    in
                    let otherSymset =
                        match
                          mapLookup nt firstSet
                        with
                          Some s
                        then
                          s
                        else
                          emptySymSet
                      in
                      let symset = { symset with syms = mapUnion symset.syms otherSymset.syms }
                      in
                      match
                        otherSymset.eps
                      with
                        true
                      then
                        work symset rhs
                      else
                        symset
            in
            work emptySymSet
          in
          let addProdToFollow: ParserConcrete_Production prodLabel state -> Map Name (Map (SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel) ()) -> Map Name (Map (SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel) ()) =
            lam prod.
              lam follow.
                match
                  prod
                with
                  {rhs = rhs, nt = prodNt}
                in
                recursive
                    let work =
                      lam follow.
                        lam rhs.
                          match
                            rhs
                          with
                            ""
                          then
                            follow
                          else match
                            rhs
                          with
                            [ NtSpec nt ] ++ rhs
                          then
                            let ntFollow =
                              match
                                mapLookup nt follow
                              with
                                Some s
                              then
                                s
                              else
                                mapEmpty vParseCalc_compareSpecSymbol
                            in
                            let otherSymset = firstOfRhs rhs in
                            let ntFollow = mapUnion ntFollow otherSymset.syms in
                            let ntFollow =
                              match
                                otherSymset.eps
                              with
                                true
                              then
                                mapUnion ntFollow (mapFindExn prodNt follow)
                              else
                                ntFollow
                            in
                            work (mapInsert nt ntFollow follow) rhs
                          else match
                            rhs
                          with
                            [ _ ] ++ rhs
                          in
                          work follow rhs
                  in
                  work follow rhs
          in
          let followSet: Map Name (Map (SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel) ()) =
            _iterateUntilFixpoint
              eqFollowSet
              (lam prev.
                 foldl
                   (lam prev.
                      lam prod.
                        addProdToFollow prod prev)
                   prev
                   productions)
              (mapInsert
                 startNt
                 (mapInsert
                    (TokSpec
                       (TokenReprEOF_EOFRepr
                          {}))
                    {}
                    (mapEmpty vParseCalc_compareSpecSymbol))
                 (mapMap
                    (lam #var"".
                       mapEmpty vParseCalc_compareSpecSymbol)
                    groupedProds))
          in
          let emptyTableTarget = ref (mapEmpty vParseCalc_compareSpecSymbol)
          in
          let table: Map Name (Ref (Map (SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel) {syms: [SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel], label: prodLabel, action: Action TokenParser_Token state})) =
            mapMap
              (lam #var"".
                 ref (mapEmpty vParseCalc_compareSpecSymbol))
              groupedProds
          in
          let specSymToGenSym =
            lam sym.
              match
                sym
              with
                NtSpec nt
              then
                NtSym
                  { nt = nt,
                    table = optionGetOr emptyTableTarget (mapLookup nt table) }
              else
                sym
          in
          let hasLl1Error = ref false in
          let ll1Errors =
            mapMap
              (lam #var"".
                 ref (mapEmpty vParseCalc_compareSpecSymbol))
              groupedProds
          in
          let addProdToTable =
            lam prod: ParserConcrete_Production prodLabel state.
              let tableRef = mapFindExn prod.nt table in
              let prev = deref tableRef in
              let firstSymset = firstOfRhs prod.rhs in
              let symset =
                match
                  firstSymset.eps
                with
                  true
                then
                  mapUnion firstSymset.syms (mapFindExn prod.nt followSet)
                else firstSymset.syms
              in
              let newProd =
                { action = prod.action,
                  label = prod.label,
                  syms = map specSymToGenSym prod.rhs }
              in
              let tableAdditions = mapMap (lam #var"".
                     newProd) symset
              in
              (for_
                   (mapBindings tableAdditions)
                   (lam binding: (SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel, ParserConcrete_TableProd prodLabel state).
                      match
                        binding
                      with
                        (sym, {label = label})
                      in
                      match
                          mapLookup sym prev
                        with
                          Some prevProd
                        then
                          let prevProd: ParserConcrete_TableProd prodLabel state = prevProd
                          in
                          (modref hasLl1Error true)
                          ; let errRef = mapFindExn prod.nt ll1Errors in
                          let errTab = deref errRef in
                          let errList =
                            match
                              mapLookup sym errTab
                            with
                              Some prods
                            then
                              snoc prods label
                            else
                              [ prevProd.label,
                                label ]
                          in
                          modref errRef (mapInsert sym errList errTab)
                        else
                          {}))
              ; modref tableRef (mapUnion prev tableAdditions)
          in
          (iter addProdToTable productions)
          ; let addLitToLits =
            lam lits.
              lam sym.
                match
                  sym
                with
                  LitSpec {lit = lit}
                then
                  mapInsert lit {} lits
                else
                  lits
          in
          let lits =
            foldl
              (lam acc.
                 lam prod: ParserConcrete_Production prodLabel state.
                   foldl addLitToLits acc prod.rhs)
              (mapEmpty cmpString)
              productions
          in
          match
            deref hasLl1Error
          with
            true
          then
            Left
              (mapFromSeq
                 nameCmp
                 (filter
                    (lam binding: (Unknown, Unknown).
                       not (null (mapBindings binding.1)))
                    (mapBindings (mapMap deref ll1Errors))))
          else
            Right
              { lits = lits,
                start =
                  { nt = startNt,
                    table = optionGetOr emptyTableTarget (mapLookup startNt table) },
                firstOfRhs = firstOfRhs }
  let vParseCalc_getTerms_FileOp: all lstyle. all rstyle. FileOpBase_FileOp lstyle rstyle -> [Info] =
    lam __sem_target.
      match
        __sem_target
      with
        FileOp1_FileOp1 x
      in
      x.__br_terms
  let vParseCalc_compareSpecSymbol: all state. all prodLabel. SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel -> SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel -> Int =
    lam l.
      lam __sem_target.
        match
          __sem_target
        with
          r
        in
        vParseCalc__compareSpecSymbol (l, r)
  let vParseCalc_getInfo_DSLExprOp: all lstyle. all rstyle. DSLExprOpBase_DSLExprOp lstyle rstyle -> Info =
    lam __sem_target.
      match
        __sem_target
      with
        NumDSLExprOp_NumDSLExprOp x
      then
        x.__br_info
      else match
        __sem_target
      with
        VariableDSLExprOp_VariableDSLExprOp x
      then
        x.__br_info
      else match
        __sem_target
      with
        AddDSLExprOp_AddDSLExprOp x
      then
        x.__br_info
      else match
        __sem_target
      with
        SubDSLExprOp_SubDSLExprOp x
      then
        x.__br_info
      else match
        __sem_target
      with
        MulDSLExprOp_MulDSLExprOp x
      then
        x.__br_info
      else match
        __sem_target
      with
        DivDSLExprOp_DivDSLExprOp x
      then
        x.__br_info
      else match
        __sem_target
      with
        DSLExprGrouping_DSLExprGrouping x
      in
      x.__br_info
  let vParseCalc_topAllowed_FileOp: all lstyle. all rstyle. FileOpBase_FileOp lstyle rstyle -> Bool =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      true
  let vParseCalc_unsplit_DSLExprOp: PermanentNode DSLExprOpBase_DSLExprOp -> (Info, CalcBaseAst_DSLExpr) =
    lam __sem_target.
      match
        __sem_target
      with
        AtomP {self = NumDSLExprOp_NumDSLExprOp x}
      then
        (x.__br_info, NumDSLExprAst_NumDSLExpr
          { info = x.__br_info,
            val =
              match
                x.val
              with
                [ x1 ] ++ _
              in
              x1 })
      else match
        __sem_target
      with
        AtomP {self = VariableDSLExprOp_VariableDSLExprOp x}
      then
        (x.__br_info, VariableDSLExprAst_VariableDSLExpr
          { info = x.__br_info,
            ident =
              match
                x.ident
              with
                [ x1 ] ++ _
              in
              x1 })
      else match
        __sem_target
      with
        InfixP {self = AddDSLExprOp_AddDSLExprOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _}
      then
        match
          (vParseCalc_unsplit_DSLExprOp l, vParseCalc_unsplit_DSLExprOp r)
        with
          ((linfo, l), (rinfo, r))
        in
        let info = foldl mergeInfo linfo [ x.__br_info,
                rinfo ]
          in
          (info, AddDSLExprAst_AddDSLExpr
            { info = info,
              left =
                match
                  [ l ]
                with
                  [ x1 ] ++ _
                in
                x1,
              right =
                match
                  [ r ]
                with
                  [ x2 ] ++ _
                in
                x2 })
      else match
        __sem_target
      with
        InfixP {self = SubDSLExprOp_SubDSLExprOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _}
      then
        match
          (vParseCalc_unsplit_DSLExprOp l, vParseCalc_unsplit_DSLExprOp r)
        with
          ((linfo, l), (rinfo, r))
        in
        let info = foldl mergeInfo linfo [ x.__br_info,
                rinfo ]
          in
          (info, SubDSLExprAst_SubDSLExpr
            { info = info,
              left =
                match
                  [ l ]
                with
                  [ x1 ] ++ _
                in
                x1,
              right =
                match
                  [ r ]
                with
                  [ x2 ] ++ _
                in
                x2 })
      else match
        __sem_target
      with
        InfixP {self = MulDSLExprOp_MulDSLExprOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _}
      then
        match
          (vParseCalc_unsplit_DSLExprOp l, vParseCalc_unsplit_DSLExprOp r)
        with
          ((linfo, l), (rinfo, r))
        in
        let info = foldl mergeInfo linfo [ x.__br_info,
                rinfo ]
          in
          (info, MulDSLExprAst_MulDSLExpr
            { info = info,
              left =
                match
                  [ l ]
                with
                  [ x1 ] ++ _
                in
                x1,
              right =
                match
                  [ r ]
                with
                  [ x2 ] ++ _
                in
                x2 })
      else match
        __sem_target
      with
        InfixP {self = DivDSLExprOp_DivDSLExprOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _}
      then
        match
          (vParseCalc_unsplit_DSLExprOp l, vParseCalc_unsplit_DSLExprOp r)
        with
          ((linfo, l), (rinfo, r))
        in
        let info = foldl mergeInfo linfo [ x.__br_info,
                rinfo ]
          in
          (info, DivDSLExprAst_DivDSLExpr
            { info = info,
              left =
                match
                  [ l ]
                with
                  [ x1 ] ++ _
                in
                x1,
              right =
                match
                  [ r ]
                with
                  [ x2 ] ++ _
                in
                x2 })
      else match
        __sem_target
      with
        AtomP {self = DSLExprGrouping_DSLExprGrouping x}
      in
      (x.__br_info, x.inner)
  let vParseCalc__compareSpecSymbol: all state. all prodLabel. (SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel, SpecSymbol TokenParser_Token TokenReprBase_TokenRepr state prodLabel) -> Int =
    lam __sem_target.
      match
        __sem_target
      with
        (TokSpec l, TokSpec r)
      then
        vParseCalc_tokReprCmp l r
      else match
        __sem_target
      with
        (LitSpec l, LitSpec r)
      then
        cmpString l.lit r.lit
      else match
        __sem_target
      with
        (NtSpec l, NtSpec r)
      then
        nameCmp l r
      else match
        __sem_target
      with
        (l, r)
      in
      subi (constructorTag l) (constructorTag r)
  let vParseCalc_getTerms_DSLExprOp: all lstyle. all rstyle. DSLExprOpBase_DSLExprOp lstyle rstyle -> [Info] =
    lam __sem_target.
      match
        __sem_target
      with
        NumDSLExprOp_NumDSLExprOp x
      then
        x.__br_terms
      else match
        __sem_target
      with
        VariableDSLExprOp_VariableDSLExprOp x
      then
        x.__br_terms
      else match
        __sem_target
      with
        AddDSLExprOp_AddDSLExprOp x
      then
        x.__br_terms
      else match
        __sem_target
      with
        SubDSLExprOp_SubDSLExprOp x
      then
        x.__br_terms
      else match
        __sem_target
      with
        MulDSLExprOp_MulDSLExprOp x
      then
        x.__br_terms
      else match
        __sem_target
      with
        DivDSLExprOp_DivDSLExprOp x
      then
        x.__br_terms
      else match
        __sem_target
      with
        DSLExprGrouping_DSLExprGrouping x
      in
      x.__br_terms
  let vParseCalc_leftAllowed_FileOp: all lstyle. all style. all rstyle. {child: FileOpBase_FileOp lstyle rstyle, parent: FileOpBase_FileOp LOpen style} -> Bool =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      true
  let vParseCalc_parenAllowed_FileOp: all lstyle. all rstyle. FileOpBase_FileOp lstyle rstyle -> AllowedDirection =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      GEither
          {}
  let vParseCalc_rightAllowed_FileOp: all style. all lstyle. all rstyle. {child: FileOpBase_FileOp lstyle rstyle, parent: FileOpBase_FileOp style ROpen} -> Bool =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      true
  let vParseCalc_topAllowed_DSLExprOp: all lstyle. all rstyle. DSLExprOpBase_DSLExprOp lstyle rstyle -> Bool =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      true
  let vParseCalc_leftAllowed_DSLExprOp: all lstyle. all style. all rstyle. {child: DSLExprOpBase_DSLExprOp lstyle rstyle, parent: DSLExprOpBase_DSLExprOp LOpen style} -> Bool =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      true
  let vParseCalc_getInfo_DSLStatementOp: all lstyle. all rstyle. DSLStatementOpBase_DSLStatementOp lstyle rstyle -> Info =
    lam __sem_target.
      match
        __sem_target
      with
        AssignmentDSLStatementOp_AssignmentDSLStatementOp x
      in
      x.__br_info
  let vParseCalc_parenAllowed_DSLExprOp: all lstyle. all rstyle. DSLExprOpBase_DSLExprOp lstyle rstyle -> AllowedDirection =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      GEither
          {}
  let vParseCalc_rightAllowed_DSLExprOp: all style. all lstyle. all rstyle. {child: DSLExprOpBase_DSLExprOp lstyle rstyle, parent: DSLExprOpBase_DSLExprOp style ROpen} -> Bool =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      true
  let vParseCalc_unsplit_DSLStatementOp: PermanentNode DSLStatementOpBase_DSLStatementOp -> (Info, CalcBaseAst_DSLStatement) =
    lam __sem_target.
      match
        __sem_target
      with
        AtomP {self = AssignmentDSLStatementOp_AssignmentDSLStatementOp x}
      in
      (x.__br_info, AssignmentDSLStatementAst_AssignmentDSLStatement
          { info = x.__br_info,
            ident =
              match
                x.ident
              with
                [ x2 ] ++ _
              in
              x2,
            val =
              match
                x.val
              with
                [ x1 ] ++ _
              in
              x1 })
  let vParseCalc_getTerms_DSLStatementOp: all lstyle. all rstyle. DSLStatementOpBase_DSLStatementOp lstyle rstyle -> [Info] =
    lam __sem_target.
      match
        __sem_target
      with
        AssignmentDSLStatementOp_AssignmentDSLStatementOp x
      in
      x.__br_terms
  let vParseCalc_groupingsAllowed_FileOp: all lstyle. all rstyle. (FileOpBase_FileOp lstyle ROpen, FileOpBase_FileOp LOpen rstyle) -> AllowedDirection =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      GEither
          {}
  let vParseCalc_topAllowed_DSLStatementOp: all lstyle. all rstyle. DSLStatementOpBase_DSLStatementOp lstyle rstyle -> Bool =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      true
  let vParseCalc_groupingsAllowed_DSLExprOp: all lstyle. all rstyle. (DSLExprOpBase_DSLExprOp lstyle ROpen, DSLExprOpBase_DSLExprOp LOpen rstyle) -> AllowedDirection =
    lam __sem_target.
      match
        __sem_target
      with
        (AddDSLExprOp_AddDSLExprOp _, MulDSLExprOp_MulDSLExprOp _)
      then
        GRight
          {}
      else match
        __sem_target
      with
        (MulDSLExprOp_MulDSLExprOp _, AddDSLExprOp_AddDSLExprOp _)
      then
        GLeft
          {}
      else match
        __sem_target
      with
        (AddDSLExprOp_AddDSLExprOp _, DivDSLExprOp_DivDSLExprOp _)
      then
        GRight
          {}
      else match
        __sem_target
      with
        (DivDSLExprOp_DivDSLExprOp _, AddDSLExprOp_AddDSLExprOp _)
      then
        GLeft
          {}
      else match
        __sem_target
      with
        (SubDSLExprOp_SubDSLExprOp _, MulDSLExprOp_MulDSLExprOp _)
      then
        GRight
          {}
      else match
        __sem_target
      with
        (MulDSLExprOp_MulDSLExprOp _, SubDSLExprOp_SubDSLExprOp _)
      then
        GLeft
          {}
      else match
        __sem_target
      with
        (SubDSLExprOp_SubDSLExprOp _, DivDSLExprOp_DivDSLExprOp _)
      then
        GRight
          {}
      else match
        __sem_target
      with
        (DivDSLExprOp_DivDSLExprOp _, SubDSLExprOp_SubDSLExprOp _)
      then
        GLeft
          {}
      else match
        __sem_target
      with
        _
      in
      GEither
          {}
  let vParseCalc_leftAllowed_DSLStatementOp: all lstyle. all style. all rstyle. {child: DSLStatementOpBase_DSLStatementOp lstyle rstyle, parent: DSLStatementOpBase_DSLStatementOp LOpen style} -> Bool =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      true
  let vParseCalc_parenAllowed_DSLStatementOp: all lstyle. all rstyle. DSLStatementOpBase_DSLStatementOp lstyle rstyle -> AllowedDirection =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      GEither
          {}
  let vParseCalc_rightAllowed_DSLStatementOp: all style. all lstyle. all rstyle. {child: DSLStatementOpBase_DSLStatementOp lstyle rstyle, parent: DSLStatementOpBase_DSLStatementOp style ROpen} -> Bool =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      true
  let vParseCalc_groupingsAllowed_DSLStatementOp: all lstyle. all rstyle. (DSLStatementOpBase_DSLStatementOp lstyle ROpen, DSLStatementOpBase_DSLStatementOp LOpen rstyle) -> AllowedDirection =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      GEither
          {}
in
let _table =
  let target =
    vParseCalc_genParsingTable
      (let #var"File" = nameSym "File" in
       let #var"DSLStatement" = nameSym "DSLStatement" in
       let #var"DSLExpr" = nameSym "DSLExpr" in
       let #var"FilePostfix" = nameSym "FilePostfix" in
       let #var"FilePrefix" = nameSym "FilePrefix" in
       let #var"FileInfix" = nameSym "FileInfix" in
       let #var"FileAtom" = nameSym "FileAtom" in
       let #var"DSLStatementPostfix" = nameSym "DSLStatementPostfix"
       in
       let #var"DSLStatementPrefix" = nameSym "DSLStatementPrefix" in
       let #var"DSLStatementInfix" = nameSym "DSLStatementInfix" in
       let #var"DSLStatementAtom" = nameSym "DSLStatementAtom" in
       let #var"DSLExprPostfix" = nameSym "DSLExprPostfix" in
       let #var"DSLExprPrefix" = nameSym "DSLExprPrefix" in
       let #var"DSLExprInfix" = nameSym "DSLExprInfix" in
       let #var"DSLExprAtom" = nameSym "DSLExprAtom" in
       let kleene = nameSym "kleene" in
       let #var"File_lclosed" = nameSym "File_lclosed" in
       let #var"File_lopen" = nameSym "File_lopen" in
       let #var"DSLStatement_lclosed" = nameSym "DSLStatement_lclosed"
       in
       let #var"DSLStatement_lopen" = nameSym "DSLStatement_lopen" in
       let #var"DSLExpr_lclosed" = nameSym "DSLExpr_lclosed" in
       let #var"DSLExpr_lopen" = nameSym "DSLExpr_lopen" in
       { productions =
           let config =
             { topAllowed = #frozen"vParseCalc_topAllowed_FileOp",
               leftAllowed = #frozen"vParseCalc_leftAllowed_FileOp",
               parenAllowed = #frozen"vParseCalc_parenAllowed_FileOp",
               rightAllowed = #frozen"vParseCalc_rightAllowed_FileOp",
               groupingsAllowed = #frozen"vParseCalc_groupingsAllowed_FileOp" }
           in
           let reportConfig =
             { topAllowed = #frozen"vParseCalc_topAllowed_FileOp",
               parenAllowed = #frozen"vParseCalc_parenAllowed_FileOp",
               lpar = "(",
               rpar = ")",
               getInfo = #frozen"vParseCalc_getInfo_FileOp",
               terminalInfos = #frozen"vParseCalc_getTerms_FileOp" }
           in
           let addFileOpAtom =
             lam #var"".
               lam x3.
                 lam st.
                   optionMap (breakableAddAtom config x3) st
           in
           let addFileOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: [Char]}.
               lam x3.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config x3 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (vParseCalc_getInfo_FileOp x3, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addFileOpPrefix =
             lam #var"".
               lam x3.
                 lam st.
                   optionMap (breakableAddPrefix config x3) st
           in
           let addFileOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: [Char]}.
               lam x3.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config x3 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (vParseCalc_getInfo_FileOp x3, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeFileOp =
             lam p: {errors: Ref [(Info, [Char])], content: [Char]}.
               lam st.
                 let res10 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse config st
                        with
                          Some (tops & ([ top ] ++ _))
                        then
                          let errs = breakableDefaultHighlight reportConfig p.content tops
                          in
                          let res10 = vParseCalc_unsplit_FileOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res10
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res10.0, BadFileAst_BadFile
                                { info = res10.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished File")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadFileAst_BadFile
                     { info = NoInfo
                           {} })
                   res10
           in
           let config1 =
             { topAllowed = #frozen"vParseCalc_topAllowed_DSLStatementOp",
               leftAllowed = #frozen"vParseCalc_leftAllowed_DSLStatementOp",
               parenAllowed = #frozen"vParseCalc_parenAllowed_DSLStatementOp",
               rightAllowed = #frozen"vParseCalc_rightAllowed_DSLStatementOp",
               groupingsAllowed =
                 #frozen"vParseCalc_groupingsAllowed_DSLStatementOp" }
           in
           let reportConfig1 =
             { topAllowed = #frozen"vParseCalc_topAllowed_DSLStatementOp",
               parenAllowed = #frozen"vParseCalc_parenAllowed_DSLStatementOp",
               lpar = "(",
               rpar = ")",
               getInfo = #frozen"vParseCalc_getInfo_DSLStatementOp",
               terminalInfos = #frozen"vParseCalc_getTerms_DSLStatementOp" }
           in
           let addDSLStatementOpAtom =
             lam #var"".
               lam x3.
                 lam st.
                   optionMap (breakableAddAtom config1 x3) st
           in
           let addDSLStatementOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: [Char]}.
               lam x3.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config1 x3 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (vParseCalc_getInfo_DSLStatementOp x3, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addDSLStatementOpPrefix =
             lam #var"".
               lam x3.
                 lam st.
                   optionMap (breakableAddPrefix config1 x3) st
           in
           let addDSLStatementOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: [Char]}.
               lam x3.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config1 x3 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (vParseCalc_getInfo_DSLStatementOp x3, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeDSLStatementOp =
             lam p: {errors: Ref [(Info, [Char])], content: [Char]}.
               lam st.
                 let res10 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse config1 st
                        with
                          Some (tops & ([ top ] ++ _))
                        then
                          let errs = breakableDefaultHighlight reportConfig1 p.content tops
                          in
                          let res10 = vParseCalc_unsplit_DSLStatementOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res10
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res10.0, BadDSLStatementAst_BadDSLStatement
                                { info = res10.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished DSLStatement")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadDSLStatementAst_BadDSLStatement
                     { info = NoInfo
                           {} })
                   res10
           in
           let config2 =
             { topAllowed = #frozen"vParseCalc_topAllowed_DSLExprOp",
               leftAllowed = #frozen"vParseCalc_leftAllowed_DSLExprOp",
               parenAllowed = #frozen"vParseCalc_parenAllowed_DSLExprOp",
               rightAllowed = #frozen"vParseCalc_rightAllowed_DSLExprOp",
               groupingsAllowed =
                 #frozen"vParseCalc_groupingsAllowed_DSLExprOp" }
           in
           let reportConfig2 =
             { topAllowed = #frozen"vParseCalc_topAllowed_DSLExprOp",
               parenAllowed = #frozen"vParseCalc_parenAllowed_DSLExprOp",
               lpar = "(",
               rpar = ")",
               getInfo = #frozen"vParseCalc_getInfo_DSLExprOp",
               terminalInfos = #frozen"vParseCalc_getTerms_DSLExprOp" }
           in
           let addDSLExprOpAtom =
             lam #var"".
               lam x3.
                 lam st.
                   optionMap (breakableAddAtom config2 x3) st
           in
           let addDSLExprOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: [Char]}.
               lam x3.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config2 x3 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (vParseCalc_getInfo_DSLExprOp x3, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addDSLExprOpPrefix =
             lam #var"".
               lam x3.
                 lam st.
                   optionMap (breakableAddPrefix config2 x3) st
           in
           let addDSLExprOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: [Char]}.
               lam x3.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config2 x3 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (vParseCalc_getInfo_DSLExprOp x3, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeDSLExprOp =
             lam p: {errors: Ref [(Info, [Char])], content: [Char]}.
               lam st.
                 let res10 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse config2 st
                        with
                          Some (tops & ([ top ] ++ _))
                        then
                          let errs = breakableDefaultHighlight reportConfig2 p.content tops
                          in
                          let res10 = vParseCalc_unsplit_DSLExprOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res10
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res10.0, BadDSLExprAst_BadDSLExpr
                                { info = res10.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished DSLExpr")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadDSLExprAst_BadDSLExpr
                     { info = NoInfo
                           {} })
                   res10
           in
           [ { rhs =
                 [ vParseCalc_ntSym #var"DSLStatement",
                   vParseCalc_litSym ";",
                   vParseCalc_ntSym kleene ],
               nt = kleene,
               action =
                 lam state: {errors: Ref [(Info, [Char])], content: [Char]}.
                   lam res.
                     match
                       res
                     with
                       [ UserSym ntVal,
                         LitParsed l,
                         UserSym val1 ]
                     in
                     let ntVal: (Info, CalcBaseAst_DSLStatement) = fromDyn ntVal in
                       let val1: {s: [CalcBaseAst_DSLStatement], __br_info: Info, __br_terms: [Info]} = fromDyn val1
                       in
                       asDyn
                         { s = concat [ ntVal.1 ] val1.s,
                           __br_info =
                             foldl
                               mergeInfo
                               ntVal.0
                               [ l.info,
                                 val1.__br_info ],
                           __br_terms = concat [ l.info ] val1.__br_terms },
               label = {} },
             { rhs = "",
               nt = kleene,
               action =
                 lam state1: {errors: Ref [(Info, [Char])], content: [Char]}.
                   lam res1.
                     match
                       res1
                     with
                       ""
                     in
                     asDyn
                         { s = "",
                           __br_info = NoInfo
                               {},
                           __br_terms = "" },
               label = {} },
             { rhs = [ vParseCalc_ntSym kleene ],
               nt = #var"FileAtom",
               action =
                 lam state2: {errors: Ref [(Info, [Char])], content: [Char]}.
                   lam res2.
                     match
                       res2
                     with
                       [ UserSym val1 ]
                     in
                     let val1: {s: [CalcBaseAst_DSLStatement], __br_info: Info, __br_terms: [Info]} = fromDyn val1
                       in
                       asDyn
                         (FileOp1_FileOp1
                            { s = val1.s,
                              __br_info = val1.__br_info,
                              __br_terms = val1.__br_terms }),
               label = {} },
             { rhs =
                 [ vParseCalc_litSym "let",
                   vParseCalc_tokSym (LIdentTokenParser_LIdentRepr
                        {}),
                   vParseCalc_litSym "=",
                   vParseCalc_ntSym #var"DSLExpr" ],
               nt = #var"DSLStatementAtom",
               action =
                 lam state3: {errors: Ref [(Info, [Char])], content: [Char]}.
                   lam res3.
                     match
                       res3
                     with
                       [ LitParsed l1,
                         TokParsed (LIdentTokenParser_LIdentTok x),
                         LitParsed l2,
                         UserSym ntVal1 ]
                     in
                     let ntVal1: (Info, CalcBaseAst_DSLExpr) = fromDyn ntVal1 in
                       asDyn
                         (AssignmentDSLStatementOp_AssignmentDSLStatementOp
                            { ident = [ { i = x.info, v = x.val } ],
                              val = [ ntVal1.1 ],
                              __br_info =
                                foldl
                                  mergeInfo
                                  l1.info
                                  [ x.info,
                                    l2.info,
                                    ntVal1.0 ],
                              __br_terms =
                                join
                                  [ [ l1.info ],
                                    [ x.info ],
                                    [ l2.info ] ] }),
               label = {} },
             { rhs =
                 [ vParseCalc_tokSym (UFloatTokenParser_FloatRepr
                        {}) ],
               nt = #var"DSLExprAtom",
               action =
                 lam state4: {errors: Ref [(Info, [Char])], content: [Char]}.
                   lam res4.
                     match
                       res4
                     with
                       [ TokParsed (UFloatTokenParser_FloatTok x1) ]
                     in
                     asDyn
                         (NumDSLExprOp_NumDSLExprOp
                            { val = [ { i = x1.info, v = x1.val } ],
                              __br_info = x1.info,
                              __br_terms = [ x1.info ] }),
               label = {} },
             { rhs =
                 [ vParseCalc_tokSym (LIdentTokenParser_LIdentRepr
                        {}) ],
               nt = #var"DSLExprAtom",
               action =
                 lam state5: {errors: Ref [(Info, [Char])], content: [Char]}.
                   lam res5.
                     match
                       res5
                     with
                       [ TokParsed (LIdentTokenParser_LIdentTok x2) ]
                     in
                     asDyn
                         (VariableDSLExprOp_VariableDSLExprOp
                            { ident = [ { i = x2.info, v = x2.val } ],
                              __br_info = x2.info,
                              __br_terms = [ x2.info ] }),
               label = {} },
             { rhs = [ vParseCalc_litSym "+" ],
               nt = #var"DSLExprInfix",
               action =
                 lam state6: {errors: Ref [(Info, [Char])], content: [Char]}.
                   lam res6.
                     match
                       res6
                     with
                       [ LitParsed l3 ]
                     in
                     asDyn
                         (AddDSLExprOp_AddDSLExprOp
                            { __br_info = l3.info, __br_terms = [ l3.info ] }),
               label = {} },
             { rhs = [ vParseCalc_litSym "-" ],
               nt = #var"DSLExprInfix",
               action =
                 lam state7: {errors: Ref [(Info, [Char])], content: [Char]}.
                   lam res7.
                     match
                       res7
                     with
                       [ LitParsed l4 ]
                     in
                     asDyn
                         (SubDSLExprOp_SubDSLExprOp
                            { __br_info = l4.info, __br_terms = [ l4.info ] }),
               label = {} },
             { rhs = [ vParseCalc_litSym "*" ],
               nt = #var"DSLExprInfix",
               action =
                 lam state8: {errors: Ref [(Info, [Char])], content: [Char]}.
                   lam res8.
                     match
                       res8
                     with
                       [ LitParsed l5 ]
                     in
                     asDyn
                         (MulDSLExprOp_MulDSLExprOp
                            { __br_info = l5.info, __br_terms = [ l5.info ] }),
               label = {} },
             { rhs = [ vParseCalc_litSym "/" ],
               nt = #var"DSLExprInfix",
               action =
                 lam state9: {errors: Ref [(Info, [Char])], content: [Char]}.
                   lam res9.
                     match
                       res9
                     with
                       [ LitParsed l6 ]
                     in
                     asDyn
                         (DivDSLExprOp_DivDSLExprOp
                            { __br_info = l6.info, __br_terms = [ l6.info ] }),
               label = {} },
             { rhs =
                 [ vParseCalc_litSym "(",
                   vParseCalc_ntSym #var"DSLExpr",
                   vParseCalc_litSym ")" ],
               nt = #var"DSLExprAtom",
               action =
                 lam #var"".
                   lam seq.
                     match
                       seq
                     with
                       [ LitParsed l7,
                         UserSym ntVal2,
                         LitParsed l8 ]
                     in
                     let ntVal2: (Info, CalcBaseAst_DSLExpr) = fromDyn ntVal2 in
                       asDyn
                         (DSLExprGrouping_DSLExprGrouping
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l7.info
                                  [ ntVal2.0,
                                    l8.info ],
                              __br_terms =
                                [ l7.info,
                                  l8.info ],
                              inner =
                                match
                                  [ ntVal2.1 ]
                                with
                                  [ x3 ]
                                in
                                x3 }),
               label = {} },
             { rhs = [ vParseCalc_ntSym #var"File_lclosed" ],
               nt = #var"File",
               action =
                 lam #var"".
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})),
               label = {} },
             { rhs =
                 [ vParseCalc_ntSym #var"FileAtom",
                   vParseCalc_ntSym #var"File_lopen" ],
               nt = #var"File_lclosed",
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x3,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addFileOpAtom p (fromDyn x3) st)),
               label = {} },
             { rhs =
                 [ vParseCalc_ntSym #var"FileInfix",
                   vParseCalc_ntSym #var"File_lclosed" ],
               nt = #var"File_lopen",
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x3,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addFileOpInfix p (fromDyn x3) st)),
               label = {} },
             { rhs =
                 [ vParseCalc_ntSym #var"FilePrefix",
                   vParseCalc_ntSym #var"File_lclosed" ],
               nt = #var"File_lclosed",
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x3,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addFileOpPrefix p (fromDyn x3) st)),
               label = {} },
             { rhs =
                 [ vParseCalc_ntSym #var"FilePostfix",
                   vParseCalc_ntSym #var"File_lopen" ],
               nt = #var"File_lopen",
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x3,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addFileOpPostfix p (fromDyn x3) st)),
               label = {} },
             { rhs = "",
               nt = #var"File_lopen",
               action =
                 lam p.
                   lam #var"".
                     asDyn (lam st.
                          finalizeFileOp p st),
               label = {} },
             { rhs = [ vParseCalc_ntSym #var"DSLStatement_lclosed" ],
               nt = #var"DSLStatement",
               action =
                 lam #var"".
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})),
               label = {} },
             { rhs =
                 [ vParseCalc_ntSym #var"DSLStatementAtom",
                   vParseCalc_ntSym #var"DSLStatement_lopen" ],
               nt = #var"DSLStatement_lclosed",
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x3,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addDSLStatementOpAtom p (fromDyn x3) st)),
               label = {} },
             { rhs =
                 [ vParseCalc_ntSym #var"DSLStatementInfix",
                   vParseCalc_ntSym #var"DSLStatement_lclosed" ],
               nt = #var"DSLStatement_lopen",
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x3,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addDSLStatementOpInfix p (fromDyn x3) st)),
               label = {} },
             { rhs =
                 [ vParseCalc_ntSym #var"DSLStatementPrefix",
                   vParseCalc_ntSym #var"DSLStatement_lclosed" ],
               nt = #var"DSLStatement_lclosed",
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x3,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addDSLStatementOpPrefix p (fromDyn x3) st)),
               label = {} },
             { rhs =
                 [ vParseCalc_ntSym #var"DSLStatementPostfix",
                   vParseCalc_ntSym #var"DSLStatement_lopen" ],
               nt = #var"DSLStatement_lopen",
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x3,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addDSLStatementOpPostfix p (fromDyn x3) st)),
               label = {} },
             { rhs = "",
               nt = #var"DSLStatement_lopen",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeDSLStatementOp p st),
               label = {} },
             { rhs = [ vParseCalc_ntSym #var"DSLExpr_lclosed" ],
               nt = #var"DSLExpr",
               action =
                 lam #var"".
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})),
               label = {} },
             { rhs =
                 [ vParseCalc_ntSym #var"DSLExprAtom",
                   vParseCalc_ntSym #var"DSLExpr_lopen" ],
               nt = #var"DSLExpr_lclosed",
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x3,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addDSLExprOpAtom p (fromDyn x3) st)),
               label = {} },
             { rhs =
                 [ vParseCalc_ntSym #var"DSLExprInfix",
                   vParseCalc_ntSym #var"DSLExpr_lclosed" ],
               nt = #var"DSLExpr_lopen",
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x3,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addDSLExprOpInfix p (fromDyn x3) st)),
               label = {} },
             { rhs =
                 [ vParseCalc_ntSym #var"DSLExprPrefix",
                   vParseCalc_ntSym #var"DSLExpr_lclosed" ],
               nt = #var"DSLExpr_lclosed",
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x3,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addDSLExprOpPrefix p (fromDyn x3) st)),
               label = {} },
             { rhs =
                 [ vParseCalc_ntSym #var"DSLExprPostfix",
                   vParseCalc_ntSym #var"DSLExpr_lopen" ],
               nt = #var"DSLExpr_lopen",
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x3,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addDSLExprOpPostfix p (fromDyn x3) st)),
               label = {} },
             { rhs = "",
               nt = #var"DSLExpr_lopen",
               action =
                 lam p.
                   lam #var"".
                     asDyn (lam st.
                          finalizeDSLExprOp p st),
               label = {} } ],
         start = #var"File" })
  in
  match
    target
  with
    Right table
  in
  table
in
recursive
  let readBytesBuffered: ReadChannel -> Int -> Option [Int] =
    lam rc.
      lam len.
        let #var"X" = fileReadBytes rc len in
        match
          #var"X"
        with
          Some s
        then
          let actual_length = length s in
          match
            eqi actual_length len
          with
            true
          then
            Some
              s
          else match
            readBytesBuffered rc (subi len actual_length)
          with
            Some s2
          then
            Some
              (join [ s,
                   s2 ])
          else
            None
              {}
        else match
          #var"X"
        with
          None {}
        in
        None
            {}
in
let eprintln: [Char] -> () =
  lam s.
    (fileWriteString fileStderr (join [ s,
              "\n" ]))
    ; flushStderr {}
in
let println: [Char] -> () =
  lam s.
    (fileWriteString fileStdout (join [ s,
              "\n" ]))
    ; flushStdout {}
in
let rpcprint =
  lam s.
    let len = addi 1 (length s) in
    (println
         (join
            [ "Content-Length: ",
              int2string len,
              "\r\n\r\n",
              s ]))
    ; {}
in
let jsonKeyObject: [([Char], JsonValue)] -> JsonValue =
  lam content.
    JsonObject
      (mapFromSeq cmpString content)
in
type TextDocumentPositionParams =
  {uri: [Char], line: Int, character: Int}
in
let getTextDocumentPositionParams: Map [Char] JsonValue -> TextDocumentPositionParams =
  lam params.
    match
      mapLookup "textDocument" params
    with
      Some (JsonObject textDocument)
    in
    match
        mapLookup "uri" textDocument
      with
        Some (JsonString uri)
      in
      match
          mapLookup "position" params
        with
          Some (JsonObject position)
        in
        match
            mapLookup "line" position
          with
            Some (JsonInt line)
          in
          match
              mapLookup "character" position
            with
              Some (JsonInt character)
            in
            { uri = uri, line = line, character = character }
in
type ProdPosition =
  {colEnd: Int, lineEnd: Int, colStart: Int, filename: [Char], lineStart: Int}
in
let getFileInfo: Info -> ProdPosition =
  lam fi.
    match
      fi
    with
      NoInfo {}
    then
      { filename = "", colEnd = 0, lineEnd = 0, colStart = 0, lineStart = 0 }
    else match
      fi
    with
      Info (r & {row1 = 0})
    then
      { filename = r.filename,
        colEnd = 0,
        lineEnd = 0,
        colStart = 0,
        lineStart = 0 }
    else match
      fi
    with
      Info r
    in
    { filename = r.filename,
        colEnd = r.col2,
        lineEnd = r.row2,
        colStart = r.col1,
        lineStart = r.row1 }
in
let stripUriProtocol =
  lam uri.
    match
      uri
    with
      "file://" ++ rest
    then
      rest
    else
      uri
in
let collision: (Int, Int) -> Int -> Bool =
  lam target.
    lam element.
      and (geqi element target.0) (leqi element target.1)
in
let infoCollision: Info -> [Char] -> Int -> Int -> Bool =
  lam info.
    lam filename.
      lam line.
        lam character.
          match
            info
          with
            Info r
          then
            and
              (eqString (stripUriProtocol r.filename) (stripUriProtocol filename))
              (and
                 (collision (r.row1, r.row2) line)
                 (collision (r.col1, r.col2) character))
          else
            false
in
let infoContainsInfo: Info -> Info -> Bool =
  lam info1.
    lam info2.
      match
        info1
      with
        Info r1
      then
        match
          info2
        with
          Info r2
        then
          and
            (eqString r1.filename r2.filename)
            (and
               (and (geqi r1.row1 r2.row1) (leqi r1.row2 r2.row2))
               (and (geqi r1.col1 r2.col1) (leqi r1.col2 r2.col2)))
        else
          false
      else
        false
in
type LSPHoverImplementation =
  {info: Info, content: [Char]}
in
type LSPImplementations =
  {hover: [LSPHoverImplementation]}
in
type RPCRequest =
  {id: Option Int, method: [Char], params: Map [Char] JsonValue}
in
type RPCError =
  {code: Int, data: JsonValue, message: [Char]}
in
type RPCResult
in
con RPCSuccess: JsonValue -> RPCResult in
con RPCFailure: RPCError -> RPCResult in
type RPCResponse =
  {id: Int, result: RPCResult}
in
let getRPCRequest: JsonValue -> RPCRequest =
  lam request.
    match
      request
    with
      JsonObject request
    in
    let id =
        match
          mapLookup "id" request
        with
          Some (JsonInt id)
        then
          Some
            id
        else
          None
            {}
      in
      match
        mapLookup "method" request
      with
        Some (JsonString method)
      in
      match
          mapLookup "params" request
        with
          Some (JsonObject params)
        in
        { params = params, id = id, method = method }
in
type CompileFunc =
  [Char] -> [Char] -> Either [(Info, [Char])] (Ast_Expr, LSPImplementations)
in
type LSPFileEnvironment =
  {findVariable: [Char] -> Int -> Int -> Option (Info, Name, Ast_Type), findDefinition: Name -> Option Info}
in
type LSPEnvironment =
  {files: Map [Char] LSPFileEnvironment}
in
type LSPExecutionContext =
  {compileFunc: CompileFunc, environment: LSPEnvironment}
in
type LSPResult =
  {response: Option JsonValue, environment: LSPEnvironment}
in
type LSPRoot_Params
in
con LSPUnknownMethod_UnknownMethod: () -> LSPRoot_Params in
con LSPInitialize_Initialized: () -> LSPRoot_Params in
con LSPInitialize_Initialize: () -> LSPRoot_Params in
type Annotator_Annotation =
  [Char]
in
type Annotator_Output =
  [Char]
in
type Annotator_Title =
  [Char]
in
type AnnotateSources_InfoRecord =
  {col1: Int, col2: Int, row1: Int, row2: Int, filename: [Char]}
in
type AnnotateSources_Pos =
  (Int, Int)
in
type AnnotateSources_StackItem =
  {res: Annotator_Output, annot: Annotator_Annotation, endPos: AnnotateSources_Pos}
in
type AnnotateSources_State =
  {pos: AnnotateSources_Pos, stack: [AnnotateSources_StackItem], annots: [(AnnotateSources_InfoRecord, Annotator_Annotation)]}
in
type NonExpansive_Guarded =
  Bool
in
type NormPat_SimpleCon
in
type NormPat_SNPat
in
type NormPat_NPat
in
type NormPat_NormPat =
  [NormPat_NPat]
in
con NPatImpl_NPatNot: Set NormPat_SimpleCon -> NormPat_NPat in
con NPatImpl_SNPat: NormPat_SNPat -> NormPat_NPat in
con IntNormPat_IntCon: Int -> NormPat_SimpleCon in
con IntNormPat_NPatInt: Int -> NormPat_SNPat in
con CharNormPat_CharCon: Char -> NormPat_SimpleCon in
con CharNormPat_NPatChar: Char -> NormPat_SNPat in
con BoolNormPat_BoolCon: Bool -> NormPat_SimpleCon in
con BoolNormPat_NPatBool: Bool -> NormPat_SNPat in
con ConNormPat_ConCon: Name -> NormPat_SimpleCon in
con ConNormPat_NPatCon: {ident: Name, subpat: NormPat_NPat} -> NormPat_SNPat in
con RecordNormPat_NPatRecord: Map SID NormPat_NPat -> NormPat_SNPat in
con SeqNormPat_NPatSeqEdge: {prefix: [NormPat_NPat], postfix: [NormPat_NPat], disallowed: Set Int} -> NormPat_SNPat in
con SeqNormPat_NPatSeqTot: [NormPat_NPat] -> NormPat_SNPat in
type Level =
  Int
in
type MetaVarTypeAst_MetaVar
in
type MetaVarTypeAst_MetaVarRec =
  {kind: Ast_Kind, ident: Name, level: Level}
in
con MetaVarTypeAst_Unbound: MetaVarTypeAst_MetaVarRec -> MetaVarTypeAst_MetaVar in
con MetaVarTypeAst_Link: Ast_Type -> MetaVarTypeAst_MetaVar in
con MetaVarTypeAst_TyMetaVar: {info: Info, contents: Ref MetaVarTypeAst_MetaVar} -> Ast_Type in
type Result w e a
in
con ResultOk: all w. all e. all a. {value: a, warnings: Map Symbol w} -> Result w e a in
con ResultErr: all w. all e. all a. {errors: Map Symbol e, warnings: Map Symbol w} -> Result w e a in
let _emptyMap: all x. () -> Map Symbol x =
  lam #var"".
    mapEmpty
      (lam l.
         lam r.
           subi (sym2hash l) (sym2hash r))
in
let _consume: all w. all e. all a. Result w e a -> ([w], Either [e] a) =
  lam val.
    let #var"X" = val in
    match
      #var"X"
    with
      ResultOk r
    then
      (mapValues r.warnings, Right
        r.value)
    else match
      #var"X"
    with
      ResultErr r
    in
    (mapValues r.warnings, Left
        (mapValues r.errors))
in
let _ok: all w. all e. all a. a -> Result w e a =
  lam value.
    ResultOk
      { value = value, warnings = _emptyMap {} }
in
let _err: all w. all e. all a. e -> Result w e a =
  lam err.
    let id = gensym {} in
    ResultErr
      { warnings = _emptyMap {},
        errors = mapInsert id err (_emptyMap {}) }
in
let _warn: all w. all e. w -> Result w e () =
  lam warn.
    let id = gensym {} in
    ResultOk
      { value = {}, warnings = mapInsert id warn (_emptyMap {}) }
in
let _warns: all w. all e. all a. Map Symbol w -> Result w e a -> Result w e a =
  lam warns.
    lam val.
      let #var"X" = val in
      match
        #var"X"
      with
        ResultOk r
      then
        ResultOk
          { r with warnings = mapUnion r.warnings warns }
      else match
        #var"X"
      with
        ResultErr r
      in
      ResultErr
          { r with warnings = mapUnion r.warnings warns }
in
let _asError: all w. all e. all a. Result w e a -> {errors: Map Symbol e, warnings: Map Symbol w} =
  lam start.
    let #var"X" = start in
    match
      #var"X"
    with
      ResultOk r
    then
      { warnings = r.warnings, errors = _emptyMap {} }
    else match
      #var"X"
    with
      ResultErr r
    in
    r
in
let _mergeErrors: all w. all e. {errors: Map Symbol e, warnings: Map Symbol w} -> {errors: Map Symbol e, warnings: Map Symbol w} -> {errors: Map Symbol e, warnings: Map Symbol w} =
  lam a.
    lam b.
      { warnings = mapUnion a.warnings b.warnings,
        errors = mapUnion a.errors b.errors }
in
let _map: all w. all e. all a. all b. (a -> b) -> Result w e a -> Result w e b =
  lam f.
    lam start.
      let #var"X" = start in
      match
        #var"X"
      with
        ResultOk {value = v, warnings = w}
      then
        ResultOk
          { value = f v, warnings = w }
      else match
        #var"X"
      with
        ResultErr r
      in
      ResultErr
          r
in
let _apply: all w. all e. all a. all b. Result w e (a -> b) -> Result w e a -> Result w e b =
  lam f.
    lam a.
      match
        (f, a)
      with
        (ResultOk f, ResultOk a)
      then
        ResultOk
          { value = f.value a.value,
            warnings = mapUnion f.warnings a.warnings }
      else
        ResultErr
          (_mergeErrors (_asError f) (_asError a))
in
let _map2: all w. all e. all a1. all a2. all b. (a1 -> a2 -> b) -> Result w e a1 -> Result w e a2 -> Result w e b =
  lam f.
    lam a.
      lam b.
        match
          (a, b)
        with
          (ResultOk a, ResultOk b)
        then
          ResultOk
            { value = f a.value b.value,
              warnings = mapUnion a.warnings b.warnings }
        else
          ResultErr
            (_mergeErrors (_asError a) (_asError b))
in
let _withAnnotations: all w. all e. all a. all b. Result w e a -> Result w e b -> Result w e b =
  lam r1.
    lam r2.
      _map2 (lam #var"".
           lam b.
             b) r1 r2
in
let _map3: all w. all e. all a1. all a2. all a3. all b. (a1 -> a2 -> a3 -> b) -> Result w e a1 -> Result w e a2 -> Result w e a3 -> Result w e b =
  lam f.
    lam a.
      lam b.
        lam c.
          match
            (a, b, c)
          with
            (ResultOk a, ResultOk b, ResultOk c)
          then
            ResultOk
              { value = f a.value b.value c.value,
                warnings =
                  mapUnion (mapUnion a.warnings b.warnings) c.warnings }
          else
            ResultErr
              (_mergeErrors (_mergeErrors (_asError a) (_asError b)) (_asError c))
in
let _map4: all w. all e. all a1. all a2. all a3. all a4. all b. (a1 -> a2 -> a3 -> a4 -> b) -> Result w e a1 -> Result w e a2 -> Result w e a3 -> Result w e a4 -> Result w e b =
  lam f.
    lam a.
      lam b.
        lam c.
          lam d.
            match
              (a, b, c, d)
            with
              (ResultOk a, ResultOk b, ResultOk c, ResultOk d)
            then
              ResultOk
                { value = f a.value b.value c.value d.value,
                  warnings =
                    mapUnion
                      (mapUnion (mapUnion a.warnings b.warnings) c.warnings)
                      d.warnings }
            else
              ResultErr
                (_mergeErrors
                   (_mergeErrors (_mergeErrors (_asError a) (_asError b)) (_asError c))
                   (_asError d))
in
let _map5: all w. all e. all a1. all a2. all a3. all a4. all a5. all b. (a1 -> a2 -> a3 -> a4 -> a5 -> b) -> Result w e a1 -> Result w e a2 -> Result w e a3 -> Result w e a4 -> Result w e a5 -> Result w e b =
  lam f.
    lam a.
      lam b.
        lam c.
          lam d.
            lam e.
              match
                (a, b, c, d, e)
              with
                (ResultOk a, ResultOk b, ResultOk c, ResultOk d, ResultOk e)
              then
                ResultOk
                  { value = f a.value b.value c.value d.value e.value,
                    warnings =
                      mapUnion
                        (mapUnion
                           (mapUnion (mapUnion a.warnings b.warnings) c.warnings)
                           d.warnings)
                        e.warnings }
              else
                ResultErr
                  (_mergeErrors
                     (_mergeErrors
                        (_mergeErrors (_mergeErrors (_asError a) (_asError b)) (_asError c))
                        (_asError d))
                     (_asError e))
in
let _mapM: all w. all e. all a. all b. (a -> Result w e b) -> [a] -> Result w e [b] =
  lam f.
    recursive
      let workOk: Map Symbol w -> [b] -> [a] -> Result w e [b] =
        lam accWarn.
          lam acc.
            lam list.
              match
                list
              with
                [ a ] ++ list
              then
                let #var"X" = f a in
                match
                  #var"X"
                with
                  ResultOk r
                then
                  workOk (mapUnion accWarn r.warnings) (snoc acc r.value) list
                else match
                  #var"X"
                with
                  ResultErr r
                in
                workErr { r with warnings = mapUnion accWarn r.warnings } list
              else
                ResultOk
                  { value = acc, warnings = accWarn }
      let workErr: {errors: Map Symbol e, warnings: Map Symbol w} -> [a] -> Result w e [b] =
        lam acc.
          lam list.
            match
              list
            with
              [ a ] ++ list
            then
              workErr (_mergeErrors acc (_asError (f a))) list
            else
              ResultErr
                acc
    in
    workOk (_emptyMap {}) ""
in
let _foldlM: all w. all e. all a. all b. (a -> b -> Result w e a) -> a -> [b] -> Result w e a =
  lam f.
    lam acc.
      recursive
        let workOK: Map Symbol w -> a -> [b] -> Result w e a =
          lam accWarn.
            lam acc.
              lam seq.
                match
                  seq
                with
                  [ hd ] ++ tl
                then
                  let #var"X" = f acc hd in
                  match
                    #var"X"
                  with
                    ResultOk c
                  then
                    workOK (mapUnion accWarn c.warnings) c.value tl
                  else match
                    #var"X"
                  with
                    ResultErr c
                  in
                  ResultErr
                      { c with warnings = mapUnion accWarn c.warnings }
                else
                  ResultOk
                    { value = acc, warnings = accWarn }
      in
      workOK (_emptyMap {}) acc
in
let _mapAccumLM: all w. all e. all a. all b. all c. (a -> b -> (a, Result w e c)) -> a -> [b] -> (a, Result w e [c]) =
  lam f.
    lam acc.
      recursive
        let workOK: Map Symbol w -> (a, [c]) -> [b] -> (a, Result w e [c]) =
          lam accWarn.
            lam acc.
              lam seq.
                match
                  acc
                with
                  (a, cs)
                in
                match
                    seq
                  with
                    [ b ] ++ seq
                  then
                    let #var"X" = f a b in
                    match
                      #var"X"
                    with
                      (a, ResultOk c)
                    then
                      workOK (mapUnion accWarn c.warnings) (a, snoc cs c.value) seq
                    else match
                      #var"X"
                    with
                      (a, ResultErr c)
                    in
                    workErr { c with warnings = mapUnion accWarn c.warnings } a seq
                  else
                    (a, ResultOk
                      { value = cs, warnings = accWarn })
        let workErr: {errors: Map Symbol e, warnings: Map Symbol w} -> a -> [b] -> (a, Result w e [c]) =
          lam accErr.
            lam a.
              lam seq.
                match
                  seq
                with
                  [ b ] ++ seq
                then
                  match
                    f a b
                  with
                    (a, c)
                  in
                  workErr (_mergeErrors accErr (_asError c)) a seq
                else
                  (a, ResultErr
                    accErr)
      in
      workOK (_emptyMap {}) (acc, "")
in
let _toOption: all w. all e. all a. Result w e a -> Option a =
  lam r.
    match
      r
    with
      ResultOk x
    then
      Some
        x.value
    else
      None
        {}
in
let _bind: all w. all e. all a. all b. Result w e a -> (a -> Result w e b) -> Result w e b =
  lam start.
    lam f.
      let #var"X" = start in
      match
        #var"X"
      with
        ResultOk r
      then
        _warns r.warnings (f r.value)
      else match
        #var"X"
      with
        ResultErr r
      in
      ResultErr
          r
in
let _bind2: all w. all e. all a1. all a2. all b. Result w e a1 -> Result w e a2 -> (a1 -> a2 -> Result w e b) -> Result w e b =
  lam a.
    lam b.
      lam f.
        match
          (a, b)
        with
          (ResultOk a, ResultOk b)
        then
          _warns (mapUnion a.warnings b.warnings) (f a.value b.value)
        else
          ResultErr
            (_mergeErrors (_asError a) (_asError b))
in
let _bind3: all w. all e. all a1. all a2. all a3. all b. Result w e a1 -> Result w e a2 -> Result w e a3 -> (a1 -> a2 -> a3 -> Result w e b) -> Result w e b =
  lam a.
    lam b.
      lam c.
        lam f.
          match
            (a, b, c)
          with
            (ResultOk a, ResultOk b, ResultOk c)
          then
            _warns
              (mapUnion (mapUnion a.warnings b.warnings) c.warnings)
              (f a.value b.value c.value)
          else
            ResultErr
              (_mergeErrors (_mergeErrors (_asError a) (_asError b)) (_asError c))
in
let _bind4: all w. all e. all a1. all a2. all a3. all a4. all b. Result w e a1 -> Result w e a2 -> Result w e a3 -> Result w e a4 -> (a1 -> a2 -> a3 -> a4 -> Result w e b) -> Result w e b =
  lam a.
    lam b.
      lam c.
        lam d.
          lam f.
            match
              (a, b, c, d)
            with
              (ResultOk a, ResultOk b, ResultOk c, ResultOk d)
            then
              _warns
                (mapUnion
                   (mapUnion (mapUnion a.warnings b.warnings) c.warnings)
                   d.warnings)
                (f a.value b.value c.value d.value)
            else
              ResultErr
                (_mergeErrors
                   (_mergeErrors (_mergeErrors (_asError a) (_asError b)) (_asError c))
                   (_asError d))
in
let _bind5: all w. all e. all a1. all a2. all a3. all a4. all a5. all b. Result w e a1 -> Result w e a2 -> Result w e a3 -> Result w e a4 -> Result w e a5 -> (a1 -> a2 -> a3 -> a4 -> a5 -> Result w e b) -> Result w e b =
  lam a.
    lam b.
      lam c.
        lam d.
          lam e.
            lam f.
              match
                (a, b, c, d, e)
              with
                (ResultOk a, ResultOk b, ResultOk c, ResultOk d, ResultOk e)
              then
                _warns
                  (mapUnion
                     (mapUnion
                        (mapUnion (mapUnion a.warnings b.warnings) c.warnings)
                        d.warnings)
                     e.warnings)
                  (f a.value b.value c.value d.value e.value)
              else
                ResultErr
                  (_mergeErrors
                     (_mergeErrors
                        (_mergeErrors (_mergeErrors (_asError a) (_asError b)) (_asError c))
                        (_asError d))
                     (_asError e))
in
let _bindParallel2: all w1. all e1. all w2. all e2. all a1. all a2. all b1. all b2. (Result w1 e1 a1, Result w2 e2 a2) -> (a1 -> a2 -> (Result w1 e1 b1, Result w2 e2 b2)) -> (Result w1 e1 b1, Result w2 e2 b2) =
  lam p.
    lam f.
      let #var"X" = p in
      match
        #var"X"
      with
        (ResultOk a1, ResultOk a2)
      then
        match
          f a1.value a2.value
        with
          (b1, b2)
        in
        (_warns a1.warnings b1, _warns a2.warnings b2)
      else match
        #var"X"
      with
        (a1, a2)
      in
      (ResultErr
          (_asError a1), ResultErr
          (_asError a2))
in
let _orElse: all w. all e. all a. (() -> Result w e a) -> Result w e a -> Result w e a =
  lam f.
    lam r.
      match
        r
      with
        ResultOk _
      then
        r
      else
        f {}
in
let _or: all w. all e. all a. Result w e a -> Result w e a -> Result w e a =
  lam r1.
    lam r2.
      let #var"X" = (r1, r2) in
      match
        #var"X"
      with
        (ResultOk _, ResultOk r2)
      then
        _warns r2.warnings r1
      else match
        #var"X"
      with
        (ResultOk _, ResultErr r2)
      then
        _warns r2.warnings r1
      else match
        #var"X"
      with
        (ResultErr r1, ResultOk _)
      then
        _warns r1.warnings r2
      else match
        #var"X"
      with
        (ResultErr r1, ResultErr r2)
      in
      ResultErr
          (_mergeErrors r1 r2)
in
let result =
  { err = _err,
    ok = _ok,
    or = _or,
    map = _map,
    bind = _bind,
    map2 = _map2,
    map3 = _map3,
    map4 = _map4,
    map5 = _map5,
    mapM = _mapM,
    warn = _warn,
    apply = _apply,
    bind2 = _bind2,
    bind3 = _bind3,
    bind4 = _bind4,
    bind5 = _bind5,
    foldlM = _foldlM,
    orElse = _orElse,
    consume = _consume,
    toOption = _toOption,
    mapAccumLM = _mapAccumLM,
    bindParallel2 = _bindParallel2,
    withAnnotations = _withAnnotations }
in
type Unify_UnifyError
in
type Unify_UnifyEnv =
  {boundNames: BiNameMap, wrappedLhs: Ast_Type, wrappedRhs: Ast_Type}
in
type Unify_Unifier u =
  {err: Unify_UnifyError -> u, empty: u, unify: Unify_UnifyEnv -> Ast_Type -> Ast_Type -> u, combine: u -> u -> u, unifyRepr: Unify_UnifyEnv -> ReprVar -> ReprVar -> u}
in
con Unify_Records: (Map SID Ast_Type, Map SID Ast_Type) -> Unify_UnifyError in
con Unify_Types: (Ast_Type, Ast_Type) -> Unify_UnifyError in
con Unify_Kinds: (Ast_Kind, Ast_Kind) -> Unify_UnifyError in
type UnifyPure_UnifyPureResult a =
  Result () Unify_UnifyError a
in
type UnifyPure_UnifyPureUnifier =
  [(Unify_UnifyEnv, Ast_Type, Ast_Type)]
in
type PUFContent k out
in
con PUFLink: all k. all out. k -> PUFContent k out in
con PUFEmpty: all k. all out. () -> PUFContent k out in
con PUFOut: all k. all out. out -> PUFContent k out in
type PureUnionFind k out =
  Map k {level: Int, content: PUFContent k out}
in
type PUFResult k out side =
  {puf: PureUnionFind k out, side: Option side}
in
type PUFResults k out side =
  {puf: PureUnionFind k out, sides: [side]}
in
type Unification =
  {reprs: PureUnionFind Symbol Name, types: PureUnionFind Name Ast_Type}
in
type UnifyPure_UniVarSubst =
  {metas: Map Name (Name, Int), reprs: Map Symbol (Symbol, Int)}
in
type ReprSubst =
  {pat: Ast_Type, repr: Ast_Type, vars: [Name]}
in
type TCEnv =
  {conEnv: Map Name (Level, Ast_Type), varEnv: Map Name Ast_Type, conDeps: Map Name (Set Name), matches: [Map Name NormPat_NormPat], matchLvl: Level, reptypes: {inImpl: Bool, reprEnv: Map Name ReprSubst, reprScope: Int, nextReprScope: Ref Int, opNamesInScope: Map Name (Option (Name, SID)), delayedReprUnifications: Ref [(ReprVar, ReprVar)]}, tyConEnv: Map Name (Level, [Name], Ast_Type), tyVarEnv: Map Name (Level, Ast_Kind), typeDeps: Map Name (Set Name), matchVars: Map Name Level, currentLvl: Level, disableConstructorTypes: Bool, disableRecordPolymorphism: Bool}
in
let typcheckEnvEmpty =
  { conEnv = mapEmpty nameCmp,
    varEnv = mapEmpty nameCmp,
    tyConEnv = mapEmpty nameCmp,
    tyVarEnv = mapEmpty nameCmp,
    conDeps = mapEmpty nameCmp,
    matches = [ mapEmpty nameCmp ],
    matchLvl = 0,
    reptypes =
      { reprScope = 0,
        delayedReprUnifications = ref "",
        reprEnv = mapEmpty nameCmp,
        inImpl = false,
        nextReprScope = ref 1,
        opNamesInScope = mapEmpty nameCmp },
    typeDeps = mapEmpty nameCmp,
    matchVars = mapEmpty nameCmp,
    currentLvl = 0,
    disableConstructorTypes = true,
    disableRecordPolymorphism = true }
in
let typecheckEnvAddBuiltinTypes: TCEnv -> [([Char], [[Char]])] -> TCEnv =
  lam env.
    lam tys.
      { env
        with
        tyConEnv =
          foldl
            (lam env.
               lam t.
                 mapInsert (nameNoSym t.0) (0, map nameSym t.1, tyvariant_ "") env)
            env.tyConEnv
            tys }
in
let typcheckEnvDefault = typecheckEnvAddBuiltinTypes typcheckEnvEmpty builtinTypes
in
let _tcEnvEmpty = typcheckEnvDefault in
type IsEmpty_Open a
in
type IsEmpty_Bounds =
  Map Ast_Type (Map Name (Set Name))
in
con IsEmpty_Neither: all a. () -> IsEmpty_Open a in
con IsEmpty_ROpen: all a. a -> IsEmpty_Open a in
con IsEmpty_LOpen: all a. a -> IsEmpty_Open a in
con PprintTyAnnot_FakeType: {id: Int, real: Ast_Type, result: Ref [Char]} -> Ast_Type in
con PprintTyAnnot_FakeExpr: {id: Int, real: Ast_Expr, result: Ref [Char]} -> Ast_Expr in
con PprintTyAnnot_FakePat: {id: Int, real: Ast_Pat, result: Ref [Char]} -> Ast_Pat in
recursive
  let vKeywordMaker_isKeyword =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      false
  let vKeywordMaker_makeKeywords: Ast_Expr -> Ast_Expr =
    lam __sem_target.
      match
        __sem_target
      with
        expr
      in
      let expr = vKeywordMaker_makeExprKeywords "" expr in
        let expr =
          vKeywordMaker_mapPre_Expr_Expr
            (lam expr.
               vKeywordMaker_smap_Expr_Type (vKeywordMaker_makeTypeKeywords "") expr)
            expr
        in
        expr
  let vKeywordMaker_smap_Pat_Pat: (Ast_Pat -> Ast_Pat) -> Ast_Pat -> Ast_Pat =
    lam f.
      lam __sem_target.
        match
          __sem_target
        with
          p
        in
        let res =
            vKeywordMaker_smapAccumL_Pat_Pat
              (lam #var"".
                 lam a.
                   ({}, f a))
              {}
              p
          in
          res.1
  let vKeywordMaker_isTypeKeyword =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      false
  let vKeywordMaker_smap_Expr_Expr: (Ast_Expr -> Ast_Expr) -> Ast_Expr -> Ast_Expr =
    lam f.
      lam __sem_target.
        match
          __sem_target
        with
          p
        in
        let res =
            vKeywordMaker_smapAccumL_Expr_Expr
              (lam #var"".
                 lam a.
                   ({}, f a))
              {}
              p
          in
          res.1
  let vKeywordMaker_smap_Expr_Type: (Ast_Type -> Ast_Type) -> Ast_Expr -> Ast_Expr =
    lam f.
      lam __sem_target.
        match
          __sem_target
        with
          p
        in
        let res =
            vKeywordMaker_smapAccumL_Expr_Type
              (lam #var"".
                 lam a.
                   ({}, f a))
              {}
              p
          in
          res.1
  let vKeywordMaker_smap_Type_Type: (Ast_Type -> Ast_Type) -> Ast_Type -> Ast_Type =
    lam f.
      lam __sem_target.
        match
          __sem_target
        with
          p
        in
        let res =
            vKeywordMaker_smapAccumL_Type_Type
              (lam #var"".
                 lam a.
                   ({}, f a))
              {}
              p
          in
          res.1
  let vKeywordMaker_matchKeywordPat =
    lam __sem_target.
      match
        __sem_target
      with
        NamedPat_PatNamed r
      then
        match
          r.ident
        with
          PName name
        then
          let ident = nameGetStr name in
          match
            vKeywordMaker_matchKeywordString r.info ident
          with
            Some _
          then
            errorSingle
              [ r.info ]
              (join
                 [ "Keyword \'",
                   ident,
                   "\' cannot be used inside a pattern." ])
          else
            NamedPat_PatNamed
              r
        else
          NamedPat_PatNamed
            r
      else match
        __sem_target
      with
        pat
      in
      vKeywordMaker_smap_Pat_Pat vKeywordMaker_matchKeywordPat pat
  let vKeywordMaker_makeExprKeywords =
    lam args: [Ast_Expr].
      lam __sem_target.
        match
          __sem_target
        with
          AppAst_TmApp r
        then
          let rhs = vKeywordMaker_makeExprKeywords "" r.rhs in
          let lhs = vKeywordMaker_makeExprKeywords (cons rhs args) r.lhs
          in
          match
            vKeywordMaker_isKeyword lhs
          with
            true
          then
            lhs
          else
            AppAst_TmApp
              { r with lhs = lhs, rhs = rhs }
        else match
          __sem_target
        with
          VarAst_TmVar r
        then
          let ident = nameGetStr r.ident in
          match
            vKeywordMaker_matchKeywordString r.info ident
          with
            Some n
          then
            match
              n
            with
              (noArgs, f)
            in
            match
                eqi noArgs (length args)
              with
                true
              then
                f args
              else
                vKeywordMaker_makeKeywordError r.info noArgs (length args) ident
          else
            VarAst_TmVar
              r
        else match
          __sem_target
        with
          DataAst_TmConApp r
        then
          let ident = nameGetStr r.ident in
          match
            vKeywordMaker_matchKeywordString r.info ident
          with
            Some n
          then
            match
              n
            with
              (noArgs, f)
            in
            let args = cons r.body args in
              match
                eqi noArgs (length args)
              with
                true
              then
                f args
              else
                vKeywordMaker_makeKeywordError r.info noArgs (length args) ident
          else
            DataAst_TmConApp
              r
        else match
          __sem_target
        with
          DataAst_TmConDef r
        then
          let ident = nameGetStr r.ident in
          match
            vKeywordMaker_matchKeywordString r.info ident
          with
            Some _
          then
            errorSingle
              [ r.info ]
              (join
                 [ "Keyword \'",
                   ident,
                   "\' cannot be used in a constructor definition." ])
          else
            DataAst_TmConDef
              { r with inexpr = vKeywordMaker_makeExprKeywords "" r.inexpr }
        else match
          __sem_target
        with
          TypeAst_TmType r
        then
          let ident = nameGetStr r.ident in
          match
            vKeywordMaker_matchTypeKeywordString r.info ident
          with
            Some _
          then
            errorSingle
              [ r.info ]
              (join
                 [ "Type keyword \'",
                   ident,
                   "\' cannot be used in a type definition." ])
          else
            TypeAst_TmType
              { r with inexpr = vKeywordMaker_makeExprKeywords "" r.inexpr }
        else match
          __sem_target
        with
          LamAst_TmLam r
        then
          let ident = nameGetStr r.ident in
          match
            vKeywordMaker_matchKeywordString r.info ident
          with
            Some _
          then
            errorSingle
              [ r.info ]
              (join
                 [ "Keyword \'",
                   ident,
                   "\' cannot be used in a lambda expressions." ])
          else
            LamAst_TmLam
              { r with body = vKeywordMaker_makeExprKeywords "" r.body }
        else match
          __sem_target
        with
          LetAst_TmLet r
        then
          let ident = nameGetStr r.ident in
          match
            vKeywordMaker_matchKeywordString r.info ident
          with
            Some _
          then
            errorSingle
              [ r.info ]
              (join
                 [ "Keyword \'",
                   ident,
                   "\' cannot be used in a let expressions." ])
          else
            LetAst_TmLet
              { r
                with
                body = vKeywordMaker_makeExprKeywords "" r.body,
                  inexpr = vKeywordMaker_makeExprKeywords "" r.inexpr }
        else match
          __sem_target
        with
          MatchAst_TmMatch r
        then
          MatchAst_TmMatch
            { r
              with
              target = vKeywordMaker_makeExprKeywords "" r.target,
                pat = vKeywordMaker_matchKeywordPat r.pat,
                thn = vKeywordMaker_makeExprKeywords "" r.thn,
                els = vKeywordMaker_makeExprKeywords "" r.els }
        else match
          __sem_target
        with
          expr
        in
        vKeywordMaker_smap_Expr_Expr (vKeywordMaker_makeExprKeywords "") expr
  let vKeywordMaker_makeKeywordError: all a. Info -> Int -> Int -> [Char] -> a =
    lam info: Info.
      lam n1: Int.
        lam n2: Int.
          lam __sem_target.
            match
              __sem_target
            with
              ident
            in
            errorSingle
                [ info ]
                (join
                   [ "Unexpected number of arguments for construct \'",
                     ident,
                     "\'. ",
                     "Expected ",
                     int2string n1,
                     " arguments, but found ",
                     int2string n2,
                     "." ])
  let vKeywordMaker_makeTypeKeywords: [Ast_Type] -> Ast_Type -> Ast_Type =
    lam args.
      lam __sem_target.
        match
          __sem_target
        with
          AppTypeAst_TyApp r
        then
          let rhs = vKeywordMaker_makeTypeKeywords "" r.rhs in
          let lhs = vKeywordMaker_makeTypeKeywords (cons rhs args) r.lhs
          in
          match
            vKeywordMaker_isTypeKeyword lhs
          with
            true
          then
            lhs
          else
            AppTypeAst_TyApp
              { r with lhs = lhs, rhs = rhs }
        else match
          __sem_target
        with
          ConTypeAst_TyCon r
        then
          let ident = nameGetStr r.ident in
          match
            vKeywordMaker_matchTypeKeywordString r.info ident
          with
            Some n
          then
            match
              n
            with
              (noArgs, f)
            in
            match
                eqi noArgs (length args)
              with
                true
              then
                f args
              else
                vKeywordMaker_makeKeywordError r.info noArgs (length args) ident
          else
            ConTypeAst_TyCon
              r
        else match
          __sem_target
        with
          ty
        in
        vKeywordMaker_smap_Type_Type (vKeywordMaker_makeTypeKeywords "") ty
  let vKeywordMaker_mapPre_Expr_Expr: (Ast_Expr -> Ast_Expr) -> Ast_Expr -> Ast_Expr =
    lam f.
      lam __sem_target.
        match
          __sem_target
        with
          expr
        in
        let expr = f expr in
          vKeywordMaker_smap_Expr_Expr (vKeywordMaker_mapPre_Expr_Expr f) expr
  let vKeywordMaker_matchKeywordString =
    lam info: Info.
      lam __sem_target.
        match
          __sem_target
        with
          _
        in
        None
            {}
  let vKeywordMaker_smapAccumL_Pat_Pat: all acc. (acc -> Ast_Pat -> (acc, Ast_Pat)) -> acc -> Ast_Pat -> (acc, Ast_Pat) =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            p
          in
          (acc, p)
  let vKeywordMaker_smapAccumL_Expr_Expr: all acc. (acc -> Ast_Expr -> (acc, Ast_Expr)) -> acc -> Ast_Expr -> (acc, Ast_Expr) =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            AppAst_TmApp t
          then
            match
              f acc t.lhs
            with
              (acc, lhs)
            in
            match
                f acc t.rhs
              with
                (acc, rhs)
              in
              (acc, AppAst_TmApp
                  { t with lhs = lhs, rhs = rhs })
          else match
            __sem_target
          with
            LamAst_TmLam t
          then
            match
              f acc t.body
            with
              (acc, body)
            in
            (acc, LamAst_TmLam
                { t with body = body })
          else match
            __sem_target
          with
            LetAst_TmLet t
          then
            match
              f acc t.body
            with
              (acc, body)
            in
            match
                f acc t.inexpr
              with
                (acc, inexpr)
              in
              (acc, LetAst_TmLet
                  { t with body = body, inexpr = inexpr })
          else match
            __sem_target
          with
            TypeAst_TmType t
          then
            match
              f acc t.inexpr
            with
              (acc, inexpr)
            in
            (acc, TypeAst_TmType
                { t with inexpr = inexpr })
          else match
            __sem_target
          with
            DataAst_TmConDef t
          then
            match
              f acc t.inexpr
            with
              (acc, inexpr)
            in
            (acc, DataAst_TmConDef
                { t with inexpr = inexpr })
          else match
            __sem_target
          with
            DataAst_TmConApp t
          then
            match
              f acc t.body
            with
              (acc, body)
            in
            (acc, DataAst_TmConApp
                { t with body = body })
          else match
            __sem_target
          with
            MatchAst_TmMatch t
          then
            match
              f acc t.target
            with
              (acc, target)
            in
            match
                f acc t.thn
              with
                (acc, thn)
              in
              match
                  f acc t.els
                with
                  (acc, els)
                in
                (acc, MatchAst_TmMatch
                    { t with target = target, thn = thn, els = els })
          else match
            __sem_target
          with
            p
          in
          (acc, p)
  let vKeywordMaker_smapAccumL_Expr_Type: all acc. (acc -> Ast_Type -> (acc, Ast_Type)) -> acc -> Ast_Expr -> (acc, Ast_Expr) =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            LamAst_TmLam t
          then
            match
              f acc t.tyAnnot
            with
              (acc, tyAnnot)
            in
            (acc, LamAst_TmLam
                { t with tyAnnot = tyAnnot })
          else match
            __sem_target
          with
            LetAst_TmLet t
          then
            match
              f acc t.tyAnnot
            with
              (acc, tyAnnot)
            in
            (acc, LetAst_TmLet
                { t with tyAnnot = tyAnnot })
          else match
            __sem_target
          with
            TypeAst_TmType t
          then
            match
              f acc t.tyIdent
            with
              (acc, tyIdent)
            in
            (acc, TypeAst_TmType
                { t with tyIdent = tyIdent })
          else match
            __sem_target
          with
            DataAst_TmConDef t
          then
            match
              f acc t.tyIdent
            with
              (acc, tyIdent)
            in
            (acc, DataAst_TmConDef
                { t with tyIdent = tyIdent })
          else match
            __sem_target
          with
            p
          in
          (acc, p)
  let vKeywordMaker_smapAccumL_Type_Type: all acc. (acc -> Ast_Type -> (acc, Ast_Type)) -> acc -> Ast_Type -> (acc, Ast_Type) =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            ConTypeAst_TyCon t
          then
            match
              f acc t.data
            with
              (acc, data)
            in
            (acc, ConTypeAst_TyCon
                { t with data = data })
          else match
            __sem_target
          with
            AppTypeAst_TyApp t
          then
            match
              f acc t.lhs
            with
              (acc, lhs)
            in
            match
                f acc t.rhs
              with
                (acc, rhs)
              in
              (acc, AppTypeAst_TyApp
                  { t with lhs = lhs, rhs = rhs })
          else match
            __sem_target
          with
            p
          in
          (acc, p)
  let vKeywordMaker_matchTypeKeywordString: Info -> [Char] -> Option (Int, [Ast_Type] -> Ast_Type) =
    lam info: Info.
      lam __sem_target.
        match
          __sem_target
        with
          _
        in
        None
            {}
in
con #con"_testKeywordMaker_TyThreeArgs": {arg1: Ast_Type, arg2: Ast_Type, arg3: Ast_Type, info: Info} -> Ast_Type in
con #con"_testKeywordMaker_TyTwoArgs": {arg1: Ast_Type, arg2: Ast_Type, info: Info} -> Ast_Type in
con #con"_testKeywordMaker_TyOneArg": {arg1: Ast_Type, info: Info} -> Ast_Type in
con #con"_testKeywordMaker_TyNoArgs": {info: Info} -> Ast_Type in
con #con"_testKeywordMaker_TmThreeArgs": {arg1: Ast_Expr, arg2: Ast_Expr, arg3: Ast_Expr, info: Info} -> Ast_Expr in
con #con"_testKeywordMaker_TmTwoArgs": {arg1: Ast_Expr, arg2: Ast_Expr, info: Info} -> Ast_Expr in
con #con"_testKeywordMaker_TmOneArg": {arg1: Ast_Expr, info: Info} -> Ast_Expr in
con #con"_testKeywordMaker_TmNoArgs": {info: Info} -> Ast_Expr in
let getPublishDiagnostic =
  lam uri.
    lam version.
      lam diagnostics.
        jsonKeyObject
          [ ("jsonrpc", JsonString
              "2.0"),
            ("method", JsonString
              "textDocument/publishDiagnostics"),
            ("params", jsonKeyObject
              [ ("uri", JsonString
                  uri),
                ("version", JsonInt
                  version),
                ("diagnostics", JsonArray
                  diagnostics) ]) ]
in
con LSPChange_DidChange: {uri: [Char], text: [Char], version: Int} -> LSPRoot_Params in
recursive
  let vSuperPrettyPrint_infoTy: Ast_Type -> Info =
    lam __sem_target.
      match
        __sem_target
      with
        UnknownTypeAst_TyUnknown r
      then
        r.info
      else match
        __sem_target
      with
        BoolTypeAst_TyBool r
      then
        r.info
      else match
        __sem_target
      with
        IntTypeAst_TyInt r
      then
        r.info
      else match
        __sem_target
      with
        FloatTypeAst_TyFloat r
      then
        r.info
      else match
        __sem_target
      with
        CharTypeAst_TyChar r
      then
        r.info
      else match
        __sem_target
      with
        FunTypeAst_TyArrow r
      then
        r.info
      else match
        __sem_target
      with
        SeqTypeAst_TySeq r
      then
        r.info
      else match
        __sem_target
      with
        TensorTypeAst_TyTensor r
      then
        r.info
      else match
        __sem_target
      with
        RecordTypeAst_TyRecord r
      then
        r.info
      else match
        __sem_target
      with
        VariantTypeAst_TyVariant r
      then
        r.info
      else match
        __sem_target
      with
        ConTypeAst_TyCon r
      then
        r.info
      else match
        __sem_target
      with
        DataTypeAst_TyData r
      then
        r.info
      else match
        __sem_target
      with
        VarTypeAst_TyVar t
      then
        t.info
      else match
        __sem_target
      with
        AllTypeAst_TyAll t
      then
        t.info
      else match
        __sem_target
      with
        AppTypeAst_TyApp r
      then
        r.info
      else match
        __sem_target
      with
        AliasTypeAst_TyAlias t
      in
      vSuperPrettyPrint_infoTy t.display
  let vSuperPrettyPrint_type2str =
    lam __sem_target.
      match
        __sem_target
      with
        ty
      in
      vSuperPrettyPrint_typeToString pprintEnvEmpty ty
  let vSuperPrettyPrint_unwrapType: Ast_Type -> Ast_Type =
    lam __sem_target.
      match
        __sem_target
      with
        ty
      in
      vSuperPrettyPrint_rapp_Type_Type vSuperPrettyPrint_unwrapType ty
  let vSuperPrettyPrint_computeData: DataTypeAst_DataRec -> Map Name (Set Name) =
    lam __sem_target.
      match
        __sem_target
      with
        r
      in
      match
          r.positive
        with
          true
        then
          mapMap (setIntersect r.cons) r.universe
        else
          mapMap (lam x.
               setSubtract x r.cons) r.universe
  let vSuperPrettyPrint_typeToString =
    lam env: PprintEnv.
      lam __sem_target.
        match
          __sem_target
        with
          ty
        in
        match
            vSuperPrettyPrint_getTypeStringCode 0 env ty
          with
            (_, str)
          in
          str
  let vSuperPrettyPrint_pprintConName: PprintEnv -> Name -> (PprintEnv, [Char]) =
    lam env: PprintEnv.
      lam __sem_target.
        match
          __sem_target
        with
          name
        in
        match
            vSuperPrettyPrint_pprintEnvGetStr env name
          with
            (env, str)
          in
          let s = pprintConString str in
            (env, s)
  let vSuperPrettyPrint_pprintVarName: PprintEnv -> Name -> (PprintEnv, [Char]) =
    lam env: PprintEnv.
      lam __sem_target.
        match
          __sem_target
        with
          name
        in
        match
            vSuperPrettyPrint_pprintEnvGetStr env name
          with
            (env, str)
          in
          let s = pprintVarString str in
            (env, s)
  let vSuperPrettyPrint_pprintTypeName: PprintEnv -> Name -> (PprintEnv, [Char]) =
    lam env: PprintEnv.
      lam __sem_target.
        match
          __sem_target
        with
          name
        in
        match
            vSuperPrettyPrint_pprintEnvGetStr env name
          with
            (env, str)
          in
          let s = pprintTypeString str in
            (env, s)
  let vSuperPrettyPrint_printTypeParen =
    lam indent: Int.
      lam prec: Int.
        lam env: PprintEnv.
          lam __sem_target.
            match
              __sem_target
            with
              ty
            in
            let i =
                match
                  leqi prec (vSuperPrettyPrint_typePrecedence ty)
                with
                  true
                then
                  indent
                else
                  addi 1 indent
              in
              match
                vSuperPrettyPrint_getTypeStringCode i env ty
              with
                (env, str)
              in
              match
                  leqi prec (vSuperPrettyPrint_typePrecedence ty)
                with
                  true
                then
                  (env, str)
                else
                  (env, join
                    [ "(",
                      str,
                      ")" ])
  let vSuperPrettyPrint_rapp_Type_Type: (Ast_Type -> Ast_Type) -> Ast_Type -> Ast_Type =
    lam f.
      lam __sem_target.
        match
          __sem_target
        with
          ty
        in
        let res =
            vSuperPrettyPrint_rappAccumL_Type_Type
              (lam #var"".
                 lam t.
                   ({}, f t))
              {}
              ty
          in
          res.1
  let vSuperPrettyPrint_typePrecedence =
    lam __sem_target.
      match
        __sem_target
      with
        FunTypeAst_TyArrow _
      then
        0
      else match
        __sem_target
      with
        AllTypeAst_TyAll _
      then
        0
      else match
        __sem_target
      with
        AppTypeAst_TyApp _
      then
        1
      else match
        __sem_target
      with
        AliasTypeAst_TyAlias t
      then
        vSuperPrettyPrint_typePrecedence t.display
      else match
        __sem_target
      with
        ty
      in
      100000
  let vSuperPrettyPrint_pprintEnvGetStr =
    lam env: PprintEnv.
      lam __sem_target.
        match
          __sem_target
        with
          name
        in
        match
            pprintEnvLookup name env
          with
            Some str
          then
            (env, str)
          else
            let baseStr = nameGetStr name in
            match
              pprintEnvFree baseStr env
            with
              true
            then
              (pprintEnvAdd name baseStr 1 env, baseStr)
            else match
              env
            with
              {count = count}
            in
            let start =
                match
                  mapLookup baseStr count
                with
                  Some i
                then
                  i
                else
                  1
              in
              recursive
                let findFree: [Char] -> Int -> ([Char], Int) =
                  lam baseStr.
                    lam i.
                      let proposal = concat baseStr (int2string i) in
                      match
                        pprintEnvFree proposal env
                      with
                        true
                      then
                        (proposal, i)
                      else
                        findFree baseStr (addi i 1)
              in
              match
                findFree baseStr start
              with
                (str, i)
              in
              (pprintEnvAdd name str (addi i 1) env, str)
  let vSuperPrettyPrint_getKindStringCode =
    lam indent: Int.
      lam env: PprintEnv.
        lam __sem_target.
          match
            __sem_target
          with
            PolyKindAst_Poly {}
          then
            (env, "Poly")
          else match
            __sem_target
          with
            MonoKindAst_Mono {}
          then
            (env, "Mono")
          else match
            __sem_target
          with
            RecordKindAst_Record r
          then
            let tyrec =
              RecordTypeAst_TyRecord
                { info = NoInfo
                      {}, fields = r.fields }
            in
            vSuperPrettyPrint_getTypeStringCode indent env tyrec
          else match
            __sem_target
          with
            DataKindAst_Data r
          in
          let cons2str =
              lam env.
                lam cons.
                  match
                    setIsEmpty cons
                  with
                    true
                  then
                    (env, None
                      {})
                  else match
                    mapAccumL vSuperPrettyPrint_pprintConName env (setToSeq cons)
                  with
                    (env, kstr)
                  in
                  (env, Some
                      (strJoin " " kstr))
            in
            match
              mapFoldWithKey
                (lam acc.
                   lam t.
                     lam ks.
                       match
                         vSuperPrettyPrint_pprintTypeName acc.0 t
                       with
                         (env, tstr)
                       in
                       match
                           cons2str env ks.lower
                         with
                           (env, lower)
                         in
                         match
                             match
                               ks.upper
                             with
                               Some u
                             then
                               cons2str env u
                             else
                               (env, None
                                 {})
                           with
                             (env, upper)
                           in
                           let prefix =
                               match
                                 optionIsSome upper
                               with
                                 true
                               then
                                 "< "
                               else match
                                 optionMapOr false setIsEmpty ks.upper
                               with
                                 true
                               then
                                 "| "
                               else
                                 "> "
                             in
                             let consstr =
                               optionCombine
                                 (lam x.
                                    lam y.
                                      Some
                                        (join
                                           [ x,
                                             " | ",
                                             y ]))
                                 upper
                                 lower
                             in
                             (env, snoc
                               acc.1
                               (join
                                  [ tstr,
                                    "[",
                                    prefix,
                                    optionGetOr "" consstr,
                                    "]" ])))
                (env, "")
                r.types
            with
              (env, consstr)
            in
            (env, join
                [ "{",
                  strJoin ", " consstr,
                  "}" ])
  let vSuperPrettyPrint_getTypeStringCode =
    lam indent: Int.
      lam env: PprintEnv.
        lam __sem_target.
          match
            __sem_target
          with
            UnknownTypeAst_TyUnknown _
          then
            (env, "Unknown")
          else match
            __sem_target
          with
            BoolTypeAst_TyBool _
          then
            (env, "Bool")
          else match
            __sem_target
          with
            IntTypeAst_TyInt _
          then
            (env, "Int")
          else match
            __sem_target
          with
            FloatTypeAst_TyFloat _
          then
            (env, "Float")
          else match
            __sem_target
          with
            CharTypeAst_TyChar _
          then
            (env, "Char")
          else match
            __sem_target
          with
            FunTypeAst_TyArrow t
          then
            match
              vSuperPrettyPrint_printTypeParen indent 1 env t.from
            with
              (env, from)
            in
            match
                vSuperPrettyPrint_getTypeStringCode indent env t.to
              with
                (env, to)
              in
              (env, join
                  [ from,
                    " -> ",
                    to ])
          else match
            __sem_target
          with
            SeqTypeAst_TySeq t
          then
            match
              vSuperPrettyPrint_getTypeStringCode indent env t.ty
            with
              (env, ty)
            in
            (env, join [ "[",
                  ty,
                  "]" ])
          else match
            __sem_target
          with
            TensorTypeAst_TyTensor t
          then
            match
              vSuperPrettyPrint_getTypeStringCode indent env t.ty
            with
              (env, ty)
            in
            (env, join [ "Tensor[",
                  ty,
                  "]" ])
          else match
            __sem_target
          with
            RecordTypeAst_TyRecord t & ty
          then
            match
              mapIsEmpty t.fields
            with
              true
            then
              (env, "()")
            else
              let orderedFields = vSuperPrettyPrint_tyRecordOrderedFields ty
              in
              let tuple =
                let seq =
                  map
                    (lam b: (SID, Ast_Type).
                       (sidToString b.0, b.1))
                    orderedFields
                in
                match
                  forAll
                    (lam t: ([Char], Ast_Type).
                       stringIsInt t.0)
                    seq
                with
                  true
                then
                  let seq =
                    map
                      (lam t: ([Char], Ast_Type).
                         (string2int t.0, t.1))
                      seq
                  in
                  let seq: [(Int, Ast_Type)] =
                    sort
                      (lam l: (Int, Ast_Type).
                         lam r: (Int, Ast_Type).
                           subi l.0 r.0)
                      seq
                  in
                  let fst = lam x: (Int, Ast_Type).
                      x.0 in
                  let first = fst (head seq) in
                  let last = fst (last seq) in
                  match
                    eqi first 0
                  with
                    true
                  then
                    match
                      eqi last (subi (length seq) 1)
                    with
                      true
                    then
                      Some
                        (map
                           (lam t: (Int, Ast_Type).
                              t.1)
                           seq)
                    else
                      None
                        {}
                  else
                    None
                      {}
                else
                  None
                    {}
              in
              match
                tuple
              with
                Some tuple
              then
                match
                  mapAccumL (vSuperPrettyPrint_getTypeStringCode indent) env tuple
                with
                  (env, tuple)
                in
                let singletonComma =
                    match
                      tuple
                    with
                      [ _ ]
                    then
                      ","
                    else
                      ""
                  in
                  (env, join
                    [ "(",
                      strJoin ", " tuple,
                      singletonComma,
                      ")" ])
              else
                let f =
                  lam env.
                    lam field.
                      match
                        field
                      with
                        (sid, ty)
                      in
                      match
                          vSuperPrettyPrint_getTypeStringCode indent env ty
                        with
                          (env, tyStr)
                        in
                        (env, (sid, tyStr))
                in
                match
                  mapAccumL f env orderedFields
                with
                  (env, fields)
                in
                let fields =
                    map
                      (lam b: (SID, [Char]).
                         (vSuperPrettyPrint_pprintLabelString b.0, b.1))
                      fields
                  in
                  let conventry =
                    lam entry: ([Char], [Char]).
                      join
                        [ entry.0,
                          ": ",
                          entry.1 ]
                  in
                  (env, join
                    [ "{",
                      strJoin ", " (map conventry fields),
                      "}" ])
          else match
            __sem_target
          with
            VariantTypeAst_TyVariant t
          then
            match
              eqi (mapLength t.constrs) 0
            with
              true
            then
              (env, "<>")
            else
              (env, join
                [ "Variant<",
                  strJoin ", " (map nameGetStr (mapKeys t.constrs)),
                  ">" ])
          else match
            __sem_target
          with
            ConTypeAst_TyCon t
          then
            match
              vSuperPrettyPrint_pprintTypeName env t.ident
            with
              (env, idstr)
            in
            let d = vSuperPrettyPrint_unwrapType t.data in
              match
                d
              with
                UnknownTypeAst_TyUnknown _
              then
                (env, idstr)
              else match
                vSuperPrettyPrint_getTypeStringCode indent env t.data
              with
                (env, datastr)
              in
              match
                  d
                with
                  DataTypeAst_TyData _
                then
                  (env, concat idstr datastr)
                else
                  (env, join
                    [ idstr,
                      "{",
                      datastr,
                      "}" ])
          else match
            __sem_target
          with
            DataTypeAst_TyData t
          then
            match
              mapFoldWithKey
                (lam acc.
                   lam #var"".
                     lam ks.
                       match
                         setIsEmpty ks
                       with
                         true
                       then
                         acc
                       else match
                         mapAccumL vSuperPrettyPrint_pprintConName acc.0 (setToSeq ks)
                       with
                         (env, kstr)
                       in
                       (env, snoc acc.1 (strJoin " " kstr)))
                (env, "")
                (vSuperPrettyPrint_computeData t)
            with
              (env, consstr)
            in
            (env, join
                [ "{",
                  strJoin " " consstr,
                  "}" ])
          else match
            __sem_target
          with
            VarTypeAst_TyVar t
          then
            vSuperPrettyPrint_pprintVarName env t.ident
          else match
            __sem_target
          with
            AllTypeAst_TyAll t
          then
            match
              vSuperPrettyPrint_pprintVarName env t.ident
            with
              (env, idstr)
            in
            match
                match
                  t.kind
                with
                  MonoKindAst_Mono {} | PolyKindAst_Poly {}
                then
                  (env, "")
                else match
                  vSuperPrettyPrint_getKindStringCode indent env t.kind
                with
                  (env, kistr)
                in
                (env, concat "::" kistr)
              with
                (env, kistr)
              in
              match
                  vSuperPrettyPrint_getTypeStringCode indent env t.ty
                with
                  (env, tystr)
                in
                (env, join
                    [ "all ",
                      idstr,
                      kistr,
                      ". ",
                      tystr ])
          else match
            __sem_target
          with
            AppTypeAst_TyApp t
          then
            match
              vSuperPrettyPrint_printTypeParen indent 1 env t.lhs
            with
              (env, lhs)
            in
            match
                vSuperPrettyPrint_printTypeParen indent 2 env t.rhs
              with
                (env, rhs)
              in
              (env, join [ lhs,
                    " ",
                    rhs ])
          else match
            __sem_target
          with
            AliasTypeAst_TyAlias t
          in
          vSuperPrettyPrint_getTypeStringCode indent env t.display
  let vSuperPrettyPrint_pprintLabelString: SID -> [Char] =
    lam __sem_target.
      match
        __sem_target
      with
        sid
      in
      _parserStr (sidToString sid) "#label" _isValidLowerIdent
  let vSuperPrettyPrint_rappAccumL_Type_Type: all acc. (acc -> Ast_Type -> (acc, Ast_Type)) -> acc -> Ast_Type -> (acc, Ast_Type) =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            AliasTypeAst_TyAlias t
          then
            f acc t.content
          else match
            __sem_target
          with
            ty
          in
          (acc, ty)
  let vSuperPrettyPrint_tyRecordOrderedFields =
    lam __sem_target.
      match
        __sem_target
      with
        RecordTypeAst_TyRecord {fields = fields}
      then
        let labels = recordOrderedLabels (mapKeys fields) in
        map (lam sid.
             (sid, mapFindExn sid fields)) labels
      else match
        __sem_target
      with
        ty
      in
      errorSingle
          [ vSuperPrettyPrint_infoTy ty ]
          "Not a TyRecord, cannot extract fields."
in
con LSPHover_Hover: {id: Int, textDocument: TextDocumentPositionParams} -> LSPRoot_Params in
con LSPGotoDefinition_GotoDefinition: {id: Int, textDocument: TextDocumentPositionParams} -> LSPRoot_Params in
recursive
  let vLSP_execute: LSPExecutionContext -> LSPRoot_Params -> LSPResult =
    lam context.
      lam __sem_target.
        match
          __sem_target
        with
          LSPUnknownMethod_UnknownMethod {}
        then
          { environment = context.environment,
            response = None
                {} }
        else match
          __sem_target
        with
          LSPInitialize_Initialized {}
        then
          { environment = context.environment,
            response = None
                {} }
        else match
          __sem_target
        with
          LSPInitialize_Initialize {}
        then
          { environment = context.environment,
            response =
              Some
                (jsonKeyObject
                   [ ("jsonrpc", JsonString
                       "2.0"),
                     ("id", JsonInt
                       0),
                     ("result", jsonKeyObject
                       [ ("capabilities", jsonKeyObject
                           [ ("diagnosticProvider", jsonKeyObject
                               [ ("interFileDependencies", JsonBool
                                   false),
                                 ("workspaceDiagnostics", JsonBool
                                   false) ]),
                             ("hoverProvider", JsonBool
                               true),
                             ("textDocumentSync", JsonInt
                               1),
                             ("definitionProvider", JsonBool
                               true),
                             ("typeDefinitionProvider", JsonBool
                               true),
                             ("completionProvider", jsonKeyObject
                               [ ("triggerCharacters", JsonArray
                                   [ JsonString
                                       "." ]) ]) ]),
                         ("serverInfo", jsonKeyObject
                           [ ("name", JsonString
                               "miking-lsp-server"),
                             ("version", JsonString
                               "0.1.0") ]) ]) ]) }
        else match
          __sem_target
        with
          LSPChange_DidChange {text = text, uri = uri, version = version}
        then
          let #var"X" = context.compileFunc uri text in
          match
            #var"X"
          with
            Left errors
          then
            (eprintln "[Compile Error]")
            ; let response = vLSP_getDiagnostics uri version errors in
            { environment = context.environment,
              response = Some
                  response }
          else match
            #var"X"
          with
            Right file
          in
          (eprintln "[Compile Success]")
            ; let response = getPublishDiagnostic uri version "" in
            let environment = vLSP_getEnvironment context uri file in
            { environment = environment,
              response = Some
                  response }
        else match
          __sem_target
        with
          LSPHover_Hover {id = id, textDocument = {uri = uri, line = line, character = character}}
        then
          let strippedUri = stripUriProtocol uri in
          let line = addi line 1 in
          let debugText =
            join
              [ "Uri: ",
                uri,
                ", ",
                "Line: ",
                int2string line,
                ", Character: ",
                int2string character,
                "\n\n" ]
          in
          let environment = mapLookup uri context.environment.files in
          let environment =
            match
              environment
            with
              Some environment
            then
              environment
            else
              error "Environment not found"
          in
          let variable = environment.findVariable uri line character in
          let response =
            match
              variable
            with
              Some (info, variable, ty)
            then
              let info = getFileInfo info in
              jsonKeyObject
                [ ("contents", JsonString
                    (join
                       [ debugText,
                         "\n\n",
                         join
                           [ "Variable: ",
                             nameGetStr variable,
                             " (",
                             vSuperPrettyPrint_type2str ty,
                             ")" ] ])),
                  ("range", jsonKeyObject
                    [ ("start", jsonKeyObject
                        [ ("line", JsonInt
                            (subi info.lineStart 1)),
                          ("character", JsonInt
                            info.colStart) ]),
                      ("end", jsonKeyObject
                        [ ("line", JsonInt
                            (subi info.lineEnd 1)),
                          ("character", JsonInt
                            info.colEnd) ]) ]) ]
            else
              (eprintln "Variable not found")
              ; jsonKeyObject
                [ ("contents", JsonString
                    (join [ debugText,
                         "[nothing found]" ])) ]
          in
          { environment = context.environment,
            response =
              Some
                (jsonKeyObject
                   [ ("jsonrpc", JsonString
                       "2.0"),
                     ("id", JsonInt
                       id),
                     ("result", response) ]) }
        else match
          __sem_target
        with
          LSPGotoDefinition_GotoDefinition {id = id, textDocument = {uri = uri, line = line, character = character}}
        in
        let line = addi line 1 in
          let strippedUri = stripUriProtocol uri in
          let environment = mapLookup uri context.environment.files in
          let environment =
            match
              environment
            with
              Some environment
            then
              environment
            else
              error "Environment not found"
          in
          let variable = environment.findVariable uri line character in
          let definition = optionBind variable (vLSP_getDefinition environment)
          in
          let response = vLSP_getLSPResponse context id definition in
          { environment = context.environment, response = response }
  let vLSP_getParams: RPCRequest -> [Char] -> LSPRoot_Params =
    lam request.
      lam __sem_target.
        match
          __sem_target
        with
          "initialized"
        then
          LSPInitialize_Initialized
            {}
        else match
          __sem_target
        with
          "initialize"
        then
          LSPInitialize_Initialize
            {}
        else match
          __sem_target
        with
          "textDocument/didChange"
        then
          match
            mapLookup "textDocument" request.params
          with
            Some (JsonObject textDocument)
          in
          match
              mapLookup "uri" textDocument
            with
              Some (JsonString uri)
            in
            match
                mapLookup "version" textDocument
              with
                Some (JsonInt version)
              in
              match
                  mapLookup "contentChanges" request.params
                with
                  Some (JsonArray changes)
                in
                match
                    head changes
                  with
                    JsonObject contentChange
                  in
                  match
                      mapLookup "text" contentChange
                    with
                      Some (JsonString text)
                    in
                    LSPChange_DidChange
                        { text = text, uri = uri, version = version }
        else match
          __sem_target
        with
          "textDocument/didOpen"
        then
          match
            mapLookup "textDocument" request.params
          with
            Some (JsonObject textDocument)
          in
          match
              mapLookup "uri" textDocument
            with
              Some (JsonString uri)
            in
            match
                mapLookup "version" textDocument
              with
                Some (JsonInt version)
              in
              match
                  mapLookup "text" textDocument
                with
                  Some (JsonString text)
                in
                LSPChange_DidChange
                    { text = text, uri = uri, version = version }
        else match
          __sem_target
        with
          "textDocument/hover"
        then
          match
            request.id
          with
            Some id
          in
          LSPHover_Hover
              { id = id,
                textDocument = getTextDocumentPositionParams request.params }
        else match
          __sem_target
        with
          "textDocument/definition"
        then
          match
            request.id
          with
            Some id
          in
          LSPGotoDefinition_GotoDefinition
              { id = id,
                textDocument = getTextDocumentPositionParams request.params }
        else match
          __sem_target
        with
          _method
        in
        LSPUnknownMethod_UnknownMethod
            {}
  let vLSP_getDefinition =
    lam environment.
      lam __sem_target.
        match
          __sem_target
        with
          (_info, variable_name, _ty)
        in
        environment.findDefinition variable_name
  let vLSP_getDiagnostics =
    lam uri.
      lam version.
        lam __sem_target.
          match
            __sem_target
          with
            errors
          in
          let error = head errors in
            match
              error
            with
              (info, msg)
            in
            let fileInfo = getFileInfo info in
              (eprintln "[Compile Failed]")
              ; let uri = fileInfo.filename in
              getPublishDiagnostic
                uri
                version
                [ jsonKeyObject
                    [ ("message", JsonString
                        msg),
                      ("severity", JsonInt
                        1),
                      ("source", JsonString
                        "miking-lsp"),
                      ("range", jsonKeyObject
                        [ ("start", jsonKeyObject
                            [ ("line", JsonInt
                                (subi fileInfo.lineStart 1)),
                              ("character", JsonInt
                                fileInfo.colStart) ]),
                          ("end", jsonKeyObject
                            [ ("line", JsonInt
                                (subi fileInfo.lineEnd 1)),
                              ("character", JsonInt
                                fileInfo.colEnd) ]) ]) ] ]
  let vLSP_getEnvironment =
    lam context.
      lam uri.
        lam __sem_target.
          match
            __sem_target
          with
            (expr, implementations)
          in
          (eprintln "Getting environment")
            ; let strippedUri = stripUriProtocol uri in
            (eprintln "Make keywords")
            ; let expr = vKeywordMaker_makeKeywords expr in
            (eprintln "Symbolizing")
            ; let expr = vMExpr_symbolizeAllowFree expr in
            (eprintln "Creating definition lookup")
            ; recursive
              let createDefinitionLookup: Ast_Expr -> Map Name Info =
                lam expr.
                  let m = mapEmpty nameCmp in
                  let m =
                    let #var"X" = expr in
                    match
                      #var"X"
                    with
                      LetAst_TmLet {info = info, ident = ident}
                    then
                      mapInsert ident info m
                    else match
                      #var"X"
                    with
                      LamAst_TmLam {ty = ty, info = info, ident = ident, body = body, tyAnnot = tyAnnot, tyParam = tyParam}
                    then
                      match
                        info
                      with
                        Info r
                      then
                        let info =
                          Info
                            { r
                              with
                              row2 = r.row1,
                                col1 = addi r.col1 (length "lam "),
                                col2 =
                                addi
                                  r.col1
                                  (length
                                     (join
                                        [ "lam ",
                                          nameGetStr ident ])) }
                        in
                        mapInsert ident info m
                      else
                        m
                    else match
                      #var"X"
                    with
                      RecLetsAst_TmRecLets {bindings = bindings}
                    then
                      let f =
                        lam acc.
                          lam x.
                            let ident = x.ident in
                            let info = x.info in
                            mapInsert ident info acc
                      in
                      foldl f m bindings
                    else match
                      #var"X"
                    with
                      _
                    in
                    m
                  in
                  vMExpr_sfold_Expr_Expr
                    (lam acc.
                       lam e.
                         let children = createDefinitionLookup e in
                         mapUnion acc children)
                    m
                    expr
            in
            let definitionLookup = createDefinitionLookup expr in
            recursive
              let createVariableLookup: Ast_Expr -> Map Info (Name, Ast_Type) =
                lam expr.
                  let m = mapEmpty infoCmp in
                  let m =
                    let #var"X" = expr in
                    match
                      #var"X"
                    with
                      VarAst_TmVar {ty = ty, info = info, ident = ident}
                    then
                      match
                        info
                      with
                        Info realInfo
                      then
                        mapInsert info (ident, ty) m
                      else
                        m
                    else match
                      #var"X"
                    with
                      _
                    in
                    m
                  in
                  vMExpr_sfold_Expr_Expr
                    (lam acc.
                       lam e.
                         let children = createVariableLookup e in
                         mapUnion acc children)
                    m
                    expr
            in
            let variableLookup = createVariableLookup expr in
            recursive
              let findVariables =
                lam acc.
                  lam variableLookupSeq.
                    lam filename.
                      lam line.
                        lam character.
                          match
                            variableLookupSeq
                          with
                            [ x ] ++ seq
                          then
                            let info = x.0 in
                            let variable = x.1 in
                            let collision = infoCollision info filename line character in
                            let acc =
                              match
                                collision
                              with
                                true
                              then
                                let name = variable.0 in
                                let ty = variable.1 in
                                concat [ (info, name, ty) ] acc
                              else
                                acc
                            in
                            findVariables acc seq filename line character
                          else
                            acc
            in
            let findVariable: [Char] -> Int -> Int -> Option (Info, Name, Ast_Type) =
              lam filename.
                lam line.
                  lam character.
                    let foundVariables =
                      findVariables "" (mapToSeq variableLookup) filename line character
                    in
                    let seq = foundVariables in
                    (eprintln
                         (join
                            [ "Found variables (",
                              int2string (length seq),
                              "):" ]))
                    ; let f =
                      lam x.
                        (eprintln
                             (join
                                [ info2str x.0,
                                  ": ",
                                  nameGetStr x.1 ]))
                        ; {}
                    in
                    (iter f seq)
                    ; match
                      foundVariables
                    with
                      [ x ] ++ seq
                    then
                      let f =
                        lam var1.
                          lam var2.
                            let info1 = var1.0 in
                            let info2 = var2.0 in
                            match
                              infoContainsInfo info1 info2
                            with
                              true
                            then
                              var1
                            else
                              var2
                      in
                      Some
                        (foldl f x seq)
                    else
                      None
                        {}
            in
            recursive
              let _findDefinition =
                lam definitionLookupSeq.
                  lam name.
                    match
                      definitionLookupSeq
                    with
                      [ x ] ++ seq
                    then
                      let info = x.1 in
                      let collision = nameEq name x.0 in
                      match
                        collision
                      with
                        true
                      then
                        Some
                          info
                      else
                        _findDefinition seq name
                    else
                      None
                        {}
            in
            let findDefinition = _findDefinition (mapToSeq definitionLookup)
            in
            { context.environment
              with
              files =
                mapInsert
                  uri
                  { findVariable = findVariable, findDefinition = findDefinition }
                  context.environment.files }
  let vLSP_getLSPResponse =
    lam context.
      lam id.
        lam __sem_target.
          match
            __sem_target
          with
            definition
          in
          match
              definition
            with
              Some info
            then
              let info = getFileInfo info in
              let filename = info.filename in
              (eprintln
                   (join
                      [ "Going to: ",
                        filename,
                        ":",
                        int2string info.lineStart,
                        ":",
                        int2string info.colStart,
                        "-",
                        int2string info.lineEnd,
                        ":",
                        int2string info.colEnd ]))
              ; Some
                (jsonKeyObject
                   [ ("jsonrpc", JsonString
                       "2.0"),
                     ("id", JsonInt
                       id),
                     ("result", jsonKeyObject
                       [ ("uri", JsonString
                           filename),
                         ("range", jsonKeyObject
                           [ ("start", jsonKeyObject
                               [ ("line", JsonInt
                                   (subi info.lineStart 1)),
                                 ("character", JsonInt
                                   info.colStart) ]),
                             ("end", jsonKeyObject
                               [ ("line", JsonInt
                                   (subi info.lineEnd 1)),
                                 ("character", JsonInt
                                   info.colEnd) ]) ]) ]) ])
            else
              Some
                (jsonKeyObject
                   [ ("jsonrpc", JsonString
                       "2.0"),
                     ("id", JsonInt
                       id),
                     ("result", JsonNull
                       {}) ])
in
type LSPContext =
  ()
in
recursive
  let _pprintjson2string: Int -> JsonValue -> [Char] =
    lam indent.
      lam value.
        let indAmount = 2 in
        let indStr = strJoin "" (make (muli indent indAmount) " ") in
        let #var"X" = value in
        match
          #var"X"
        with
          JsonObject properties
        then
          let indStrChildren = strJoin "" (make (muli (addi indent 1) indAmount) " ")
          in
          let proplist =
            mapFoldWithKey
              (lam acc.
                 lam k.
                   lam v.
                     snoc
                       acc
                       (join
                          [ indStrChildren,
                            _pprintjson2string
                              (addi indent 1)
                              (JsonString
                                 k),
                            ": ",
                            _pprintjson2string (addi indent 1) v ]))
              ""
              properties
          in
          join
            [ "{\n",
              strJoin ",\n" proplist,
              "\n",
              indStr,
              "}" ]
        else match
          #var"X"
        with
          JsonArray values
        then
          cons
            '['
            (snoc
               (strJoin ",\n" (map (_pprintjson2string (addi indent 1)) values))
               ']')
        else match
          #var"X"
        with
          JsonString s
        then
          let escape: [Char] -> Char -> [Char] =
            lam acc.
              lam c.
                let cval: Int = char2int c in
                match
                  eqi cval 8
                with
                  true
                then
                  concat acc "\\b"
                else match
                  eqi cval 12
                with
                  true
                then
                  concat acc "\\f"
                else match
                  or (lti cval 32) (eqi cval 127)
                with
                  true
                then
                  let tohex: Int -> Char =
                    lam x.
                      match
                        lti x 10
                      with
                        true
                      then
                        int2char (addi x (char2int '0'))
                      else
                        int2char (addi (subi x 10) (char2int 'a'))
                  in
                  concat
                    acc
                    [ '\\',
                      'u',
                      '0',
                      '0',
                      tohex (divi cval 16),
                      tohex (modi cval 16) ]
                else
                  let #var"X" = c in
                  match
                    #var"X"
                  with
                    '\"'
                  then
                    concat acc "\\\""
                  else match
                    #var"X"
                  with
                    '\\'
                  then
                    concat acc "\\\\"
                  else match
                    #var"X"
                  with
                    '/'
                  then
                    concat acc "\\/"
                  else match
                    #var"X"
                  with
                    '\n'
                  then
                    concat acc "\\n"
                  else match
                    #var"X"
                  with
                    '\r'
                  then
                    concat acc "\\r"
                  else match
                    #var"X"
                  with
                    '\t'
                  then
                    concat acc "\\t"
                  else match
                    #var"X"
                  with
                    _
                  in
                  snoc acc c
          in
          snoc (foldl escape "\"" s) '\"'
        else match
          #var"X"
        with
          JsonFloat f
        then
          match
            neqf f f
          with
            true
          then
            "{\"__float__\": \"nan\"}"
          else match
            eqf f inf
          with
            true
          then
            "{\"__float__\": \"inf\"}"
          else match
            eqf f (negf inf)
          with
            true
          then
            "{\"__float__\": \"-inf\"}"
          else
            let str = float2string f in
            let #var"X" = str in
            match
              #var"X"
            with
              _ ++ "."
            then
              snoc str '0'
            else match
              #var"X"
            with
              "." ++ _
            then
              cons '0' str
            else match
              #var"X"
            with
              _
            in
            str
        else match
          #var"X"
        with
          JsonInt i
        then
          int2string i
        else match
          #var"X"
        with
          JsonBool b
        then
          match
            b
          with
            true
          then
            "true"
          else
            "false"
        else match
          #var"X"
        with
          JsonNull {}
        in
        "null"
in
recursive
  let pprintjson2string: JsonValue -> [Char] = lam value.
      _pprintjson2string 0 value
in
let handleRequest =
  lam compileFunc.
    lam environment.
      lam request.
        let request = getRPCRequest request in
        let method = request.method in
        (eprintln method)
        ; let params = vLSP_getParams request request.method in
        match
          params
        with
          LSPUnknownMethod_UnknownMethod {}
        then
          (eprintln (join [ "[Unknown method] ",
                    method ]))
          ; environment
        else
          let executionContext = { compileFunc = compileFunc, environment = environment }
          in
          let result = vLSP_execute executionContext params in
          (match
               result.response
             with
               Some response
             then
               (eprintln "Responding to request\n")
               ; (eprintln (pprintjson2string response))
               ; (eprintln "")
               ; rpcprint (json2string response)
             else
               eprintln "")
          ; result.environment
in
let getContentLength: [Char] -> Int =
  lam header.
    match
      header
    with
      "Content-Length: " ++ len ++ "\n"
    then
      string2int len
    else
      error "Content-Length not found"
in
recursive
  let readJsonRPC =
    lam compileFunc.
      lam environment.
        let #var"X" = fileReadLine fileStdin in
        match
          #var"X"
        with
          None _
        then
          {}
        else match
          #var"X"
        with
          Some header
        in
        let contentHeaderLength = addi (getContentLength header) 2 in
          let #var"X" = readBytesBuffered fileStdin contentHeaderLength
          in
          match
            #var"X"
          with
            None _
          then
            {}
          else match
            #var"X"
          with
            Some body
          in
          let asciiBody = map int2char body in
            let json = jsonParseExn asciiBody in
            let environment = handleRequest compileFunc environment json in
            readJsonRPC compileFunc environment
in
type ArgResult a =
  {options: a, strings: [[Char]]}
in
type ParseType
in
con ParseTypeInt: [Char] -> ParseType in
con ParseTypeIntMin: ([Char], Int) -> ParseType in
con ParseTypeFloat: [Char] -> ParseType in
con ParseTypeFloatMin: ([Char], Float) -> ParseType in
con ParseTypeFloatInterval: ([Char], Float, Float) -> ParseType in
con ParseTypeGeneric: ([Char], [Char]) -> ParseType in
type ArgPart a =
  {str: [Char], fail: Ref (Option ParseType), options: a}
in
type ParseOption =
  ([Char], [Char], [Char])
in
type ParseConfig a =
  [([ParseOption], [Char], ArgPart a -> a)]
in
type ParseResult a
in
con ParseOK: all a. ArgResult a -> ParseResult a in
con ParseFailUnknownOption: all a. [Char] -> ParseResult a in
con ParseFailMissingOpArg: all a. [Char] -> ParseResult a in
con ParseFailConversion: all a. (ParseType, [Char]) -> ParseResult a in
type Options_argHelpOptions =
  {indent: Int, lineWidth: Int, spaceToText: Int}
in
type Options_argParse =
  {args: [[Char]], optionsStartWith: [[Char]]}
in
type TomlTable
in
type TomlValue
in
type SearchMethod
in
con Exhaustive: () -> SearchMethod in
type TuneOptions =
  {args: [[Char]], seed: Option Int, iters: Int, method: SearchMethod, cleanup: Bool, verbose: Bool, warmups: Int, stepSize: Int, epsilonMs: Float, exitEarly: Bool, timeoutMs: Option Float, printStats: Bool, ignoreErrors: Bool, debugExpansion: Bool, dependencyAnalysis: Bool, reduceDependencies: Float, debugInstrumentation: Bool, debugDependencyAnalysis: Bool}
in
type Options =
  {toJVM: Bool, output: Option [Char], cpuOnly: Bool, jsTarget: [Char], runTests: Bool, useTuned: Bool, printHelp: Bool, accelerate: Bool, debugParse: Bool, exitBefore: Bool, debugPhases: Bool, tuneOptions: TuneOptions, debugProfile: Bool, debugShallow: Bool, disableJsTCO: Bool, keepDeadCode: Bool, toJavaScript: Bool, debugGenerate: Bool, mlangPipeline: Bool, runtimeChecks: Bool, debugTypeAnnot: Bool, debugTypeCheck: Bool, use32BitFloats: Bool, debugAccelerate: Bool, compileAfterTune: Bool, use32BitIntegers: Bool, debugConstantFold: Bool, enableConstantFold: Bool, disableOptimizations: Bool, enableConstructorTypes: Bool, accelerateTensorMaxRank: Int, disablePruneExternalUtests: Bool, disableJsGeneralOptimizations: Bool, disablePruneExternalUtestsWarning: Bool}
in
let _constWithInfos: Info -> Ast_Expr -> Ast_Expr =
  lam i: Info.
    lam tm: Ast_Expr.
      match
        tm
      with
        ConstAst_TmConst ({ty = UnknownTypeAst_TyUnknown ({info = NoInfo _} & ty), info = NoInfo _} & t)
      then
        ConstAst_TmConst
          { t
            with
            info = i,
              ty =
              UnknownTypeAst_TyUnknown
                { ty with info = i } }
      else
        tm
in
let gstr = lam t.
    lam n.
      bootParserGetString t n in
let gname = lam t.
    lam n.
      nameNoSym (bootParserGetString t n)
in
let gint = lam t.
    lam n.
      bootParserGetInt t n in
let gfloat = lam t.
    lam n.
      bootParserGetFloat t n in
let glistlen = lam t.
    lam n.
      bootParserGetListLength t n
in
type BootParser_BootParserParseMCoreFileArg =
  {builtin: [([Char], ConstAst_Const)], keywords: [[Char]], allowFree: Bool, keepUtests: Bool, externalsExclude: [[Char]], eliminateDeadCode: Bool, pruneExternalUtests: Bool, pruneExternalUtestsWarning: Bool}
in
type BootParser_BootParserParseMExprStringArg =
  {builtin: [([Char], ConstAst_Const)], keywords: [[Char]], allowFree: Bool}
in
recursive
  let vBootParser_gpat =
    lam t.
      lam __sem_target.
        match
          __sem_target
        with
          n
        in
        let t2 = bootParserGetPat t n in
          vBootParser_matchPat t2 (bootParserGetId t2)
  let vBootParser_ginfo =
    lam t.
      lam __sem_target.
        match
          __sem_target
        with
          n
        in
        let t2 = bootParserGetInfo t n in
          vBootParser_matchInfo t2 (bootParserGetId t2)
  let vBootParser_gterm =
    lam t.
      lam __sem_target.
        match
          __sem_target
        with
          n
        in
        let t2 = bootParserGetTerm t n in
          vBootParser_matchTerm t2 (bootParserGetId t2)
  let vBootParser_gtype =
    lam t.
      lam __sem_target.
        match
          __sem_target
        with
          n
        in
        let t2 = bootParserGetType t n in
          vBootParser_matchType t2 (bootParserGetId t2)
  let vBootParser_gconst =
    lam t.
      lam __sem_target.
        match
          __sem_target
        with
          n
        in
        let t2 = bootParserGetConst t n in
          vBootParser_matchConst t2 (bootParserGetId t2)
  let vBootParser_ctWorker =
    lam env: Map [Char] (Option Ast_Expr).
      lam __sem_target.
        match
          __sem_target
        with
          LetAst_TmLet r
        then
          let t1 = vBootParser_ctWorker env r.body in
          let env =
            mapInsert (nameGetStr r.ident) (None
                 {}) env
          in
          let t2 = vBootParser_ctWorker env r.inexpr in
          LetAst_TmLet
            { r with body = t1, inexpr = t2 }
        else match
          __sem_target
        with
          LamAst_TmLam r
        then
          let t =
            vBootParser_ctWorker
              (mapInsert (nameGetStr r.ident) (None
                    {}) env)
              r.body
          in
          LamAst_TmLam
            { r with body = t }
        else match
          __sem_target
        with
          VarAst_TmVar r
        then
          let ident = nameGetStr r.ident in
          match
            mapFindOrElse
              (lam #var"".
                 Some
                   (VarAst_TmVar
                      r))
              ident
              env
          with
            Some tm
          then
            _constWithInfos r.info tm
          else
            VarAst_TmVar
              r
        else match
          __sem_target
        with
          RecLetsAst_TmRecLets r
        then
          let fEnv =
            lam acc.
              lam b: RecLetsAst_RecLetBinding.
                mapInsert (nameGetStr b.ident) (None
                     {}) acc
          in
          let env = foldl fEnv env r.bindings in
          let bindings =
            map
              (lam b: RecLetsAst_RecLetBinding.
                 { b with body = vBootParser_ctWorker env b.body })
              r.bindings
          in
          RecLetsAst_TmRecLets
            { r
              with
              bindings = bindings,
                inexpr = vBootParser_ctWorker env r.inexpr }
        else match
          __sem_target
        with
          MatchAst_TmMatch r
        then
          let fEnv =
            lam acc.
              lam x.
                mapInsert x (None
                     {}) acc
          in
          let env2 = foldl fEnv env (vBootParser_ctGetPatVars "" r.pat)
          in
          MatchAst_TmMatch
            { r
              with
              target = vBootParser_ctWorker env r.target,
                thn = vBootParser_ctWorker env2 r.thn,
                els = vBootParser_ctWorker env r.els }
        else match
          __sem_target
        with
          ExtAst_TmExt r
        then
          let t =
            vBootParser_ctWorker
              (mapInsert (nameGetStr r.ident) (None
                    {}) env)
              r.inexpr
          in
          ExtAst_TmExt
            { r with inexpr = t }
        else match
          __sem_target
        with
          t
        in
        vBootParser_smap_Expr_Expr (vBootParser_ctWorker env) t
  let vBootParser_matchPat =
    lam t.
      lam __sem_target.
        match
          __sem_target
        with
          400
        then
          NamedPat_PatNamed
            { ty = tyunknown_,
              info = vBootParser_ginfo t 0,
              ident = vBootParser_strToPatName (gstr t 0) }
        else match
          __sem_target
        with
          401
        then
          SeqTotPat_PatSeqTot
            { ty = tyunknown_,
              info = vBootParser_ginfo t 0,
              pats =
                create
                  (glistlen t 0)
                  (lam n.
                     vBootParser_gpat t n) }
        else match
          __sem_target
        with
          402
        then
          let len = glistlen t 0 in
          SeqEdgePat_PatSeqEdge
            { ty = tyunknown_,
              info = vBootParser_ginfo t 0,
              middle = vBootParser_strToPatName (gstr t 0),
              prefix =
                create len (lam n.
                     vBootParser_gpat t n),
              postfix =
                create
                  (glistlen t 1)
                  (lam n.
                     vBootParser_gpat t (addi n len)) }
        else match
          __sem_target
        with
          403
        then
          let lst =
            create
              (glistlen t 0)
              (lam n.
                 (gstr t n, vBootParser_gpat t n))
          in
          RecordPat_PatRecord
            { ty = tyunknown_,
              info = vBootParser_ginfo t 0,
              bindings =
                mapFromSeq
                  cmpSID
                  (map
                     (lam b: ([Char], Ast_Pat).
                        (stringToSid b.0, b.1))
                     lst) }
        else match
          __sem_target
        with
          404
        then
          DataPat_PatCon
            { ty = tyunknown_,
              info = vBootParser_ginfo t 0,
              ident = gname t 0,
              subpat = vBootParser_gpat t 0 }
        else match
          __sem_target
        with
          405
        then
          IntPat_PatInt
            { ty = tyint_, info = vBootParser_ginfo t 0, val = gint t 0 }
        else match
          __sem_target
        with
          406
        then
          CharPat_PatChar
            { ty = tychar_,
              info = vBootParser_ginfo t 0,
              val = int2char (gint t 0) }
        else match
          __sem_target
        with
          407
        then
          BoolPat_PatBool
            { ty = tybool_,
              info = vBootParser_ginfo t 0,
              val = eqi (gint t 0) 1 }
        else match
          __sem_target
        with
          408
        then
          AndPat_PatAnd
            { ty = tyunknown_,
              info = vBootParser_ginfo t 0,
              lpat = vBootParser_gpat t 0,
              rpat = vBootParser_gpat t 1 }
        else match
          __sem_target
        with
          409
        then
          OrPat_PatOr
            { ty = tyunknown_,
              info = vBootParser_ginfo t 0,
              lpat = vBootParser_gpat t 0,
              rpat = vBootParser_gpat t 1 }
        else match
          __sem_target
        with
          410
        then
          NotPat_PatNot
            { ty = tyunknown_,
              info = vBootParser_ginfo t 0,
              subpat = vBootParser_gpat t 0 }
        else match
          __sem_target
        with
          _
        in
        error "Unknown pattern"
  let vBootParser_matchInfo =
    lam t.
      lam __sem_target.
        match
          __sem_target
        with
          500
        then
          Info
            { col1 = gint t 1,
              col2 = gint t 3,
              row1 = gint t 0,
              row2 = gint t 2,
              filename = gstr t 0 }
        else match
          __sem_target
        with
          501
        then
          NoInfo
            {}
        else match
          __sem_target
        with
          _
        in
        error "Unknown info"
  let vBootParser_matchTerm =
    lam t.
      lam __sem_target.
        match
          __sem_target
        with
          100
        then
          VarAst_TmVar
            { ty =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 },
              info = vBootParser_ginfo t 0,
              ident = gname t 0,
              frozen = neqi (gint t 0) 0 }
        else match
          __sem_target
        with
          101
        then
          AppAst_TmApp
            { ty =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 },
              info = vBootParser_ginfo t 0,
              lhs = vBootParser_gterm t 0,
              rhs = vBootParser_gterm t 1 }
        else match
          __sem_target
        with
          102
        then
          LamAst_TmLam
            { ty =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 },
              info = vBootParser_ginfo t 0,
              ident = gname t 0,
              body = vBootParser_gterm t 0,
              tyAnnot = vBootParser_gtype t 0,
              tyParam =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 } }
        else match
          __sem_target
        with
          103
        then
          LetAst_TmLet
            { ty =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 },
              info = vBootParser_ginfo t 0,
              ident = gname t 0,
              body = vBootParser_gterm t 0,
              tyAnnot = vBootParser_gtype t 0,
              inexpr = vBootParser_gterm t 1,
              tyBody =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 } }
        else match
          __sem_target
        with
          104
        then
          RecLetsAst_TmRecLets
            { ty =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 },
              info = vBootParser_ginfo t 0,
              inexpr = vBootParser_gterm t (glistlen t 0),
              bindings =
                create
                  (glistlen t 0)
                  (lam n.
                     { info = vBootParser_ginfo t (addi n 1),
                       ident = gname t n,
                       body = vBootParser_gterm t n,
                       tyAnnot = vBootParser_gtype t n,
                       tyBody =
                         UnknownTypeAst_TyUnknown
                           { info = vBootParser_ginfo t (addi n 1) } }) }
        else match
          __sem_target
        with
          105
        then
          let c = vBootParser_gconst t 0 in
          ConstAst_TmConst
            { ty =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 },
              info = vBootParser_ginfo t 0,
              val = vBootParser_gconst t 0 }
        else match
          __sem_target
        with
          106
        then
          SeqAst_TmSeq
            { ty =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 },
              info = vBootParser_ginfo t 0,
              tms =
                create
                  (glistlen t 0)
                  (lam n.
                     vBootParser_gterm t n) }
        else match
          __sem_target
        with
          107
        then
          let lst =
            create
              (glistlen t 0)
              (lam n.
                 (gstr t n, vBootParser_gterm t n))
          in
          RecordAst_TmRecord
            { ty =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 },
              info = vBootParser_ginfo t 0,
              bindings =
                mapFromSeq
                  cmpSID
                  (map
                     (lam b: ([Char], Ast_Expr).
                        (stringToSid b.0, b.1))
                     lst) }
        else match
          __sem_target
        with
          108
        then
          RecordAst_TmRecordUpdate
            { key = stringToSid (gstr t 0),
              value = vBootParser_gterm t 1,
              ty =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 },
              info = vBootParser_ginfo t 0,
              rec = vBootParser_gterm t 0 }
        else match
          __sem_target
        with
          109
        then
          TypeAst_TmType
            { ty =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 },
              info = vBootParser_ginfo t 0,
              ident = gname t 0,
              inexpr = vBootParser_gterm t 0,
              params = map (gname t) (range 1 (glistlen t 0) 1),
              tyIdent = vBootParser_gtype t 0 }
        else match
          __sem_target
        with
          110
        then
          DataAst_TmConDef
            { ty =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 },
              info = vBootParser_ginfo t 0,
              ident = gname t 0,
              inexpr = vBootParser_gterm t 0,
              tyIdent = vBootParser_gtype t 0 }
        else match
          __sem_target
        with
          111
        then
          DataAst_TmConApp
            { ty =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 },
              info = vBootParser_ginfo t 0,
              ident = gname t 0,
              body = vBootParser_gterm t 0 }
        else match
          __sem_target
        with
          112
        then
          MatchAst_TmMatch
            { ty =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 },
              info = vBootParser_ginfo t 0,
              els = vBootParser_gterm t 2,
              pat = vBootParser_gpat t 0,
              thn = vBootParser_gterm t 1,
              target = vBootParser_gterm t 0 }
        else match
          __sem_target
        with
          113
        then
          match
            let #var"X" = glistlen t 0 in
            match
              #var"X"
            with
              3
            then
              (None
                {}, None
                {})
            else match
              #var"X"
            with
              4
            then
              (Some
                (vBootParser_gterm t 3), None
                {})
            else match
              #var"X"
            with
              5
            then
              (Some
                (vBootParser_gterm t 3), Some
                (vBootParser_gterm t 4))
            else match
              #var"X"
            with
              _
            in
            error "BootParser.matchTerm: Invalid list length for tmUtest"
          with
            (tusing, tonfail)
          in
          UtestAst_TmUtest
              { next = vBootParser_gterm t 2,
                ty =
                  UnknownTypeAst_TyUnknown
                    { info = vBootParser_ginfo t 0 },
                info = vBootParser_ginfo t 0,
                test = vBootParser_gterm t 0,
                tusing = tusing,
                tonfail = tonfail,
                expected = vBootParser_gterm t 1 }
        else match
          __sem_target
        with
          114
        then
          NeverAst_TmNever
            { ty =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 },
              info = vBootParser_ginfo t 0 }
        else match
          __sem_target
        with
          115
        then
          ExtAst_TmExt
            { ty =
                UnknownTypeAst_TyUnknown
                  { info = vBootParser_ginfo t 0 },
              info = vBootParser_ginfo t 0,
              ident = gname t 0,
              inexpr = vBootParser_gterm t 0,
              tyIdent = vBootParser_gtype t 0,
              effect = neqi (gint t 0) 0 }
        else match
          __sem_target
        with
          _
        in
        error "Unknown expression"
  let vBootParser_matchType =
    lam t.
      lam __sem_target.
        match
          __sem_target
        with
          200
        then
          UnknownTypeAst_TyUnknown
            { info = vBootParser_ginfo t 0 }
        else match
          __sem_target
        with
          201
        then
          BoolTypeAst_TyBool
            { info = vBootParser_ginfo t 0 }
        else match
          __sem_target
        with
          202
        then
          IntTypeAst_TyInt
            { info = vBootParser_ginfo t 0 }
        else match
          __sem_target
        with
          203
        then
          FloatTypeAst_TyFloat
            { info = vBootParser_ginfo t 0 }
        else match
          __sem_target
        with
          204
        then
          CharTypeAst_TyChar
            { info = vBootParser_ginfo t 0 }
        else match
          __sem_target
        with
          205
        then
          FunTypeAst_TyArrow
            { info = vBootParser_ginfo t 0,
              to = vBootParser_gtype t 1,
              from = vBootParser_gtype t 0 }
        else match
          __sem_target
        with
          206
        then
          SeqTypeAst_TySeq
            { ty = vBootParser_gtype t 0, info = vBootParser_ginfo t 0 }
        else match
          __sem_target
        with
          207
        then
          let lst =
            create
              (glistlen t 0)
              (lam n.
                 (gstr t n, vBootParser_gtype t n))
          in
          RecordTypeAst_TyRecord
            { info = vBootParser_ginfo t 0,
              fields =
                mapFromSeq
                  cmpSID
                  (map
                     (lam b: ([Char], Ast_Type).
                        (stringToSid b.0, b.1))
                     lst) }
        else match
          __sem_target
        with
          208
        then
          match
            eqi (glistlen t 0) 0
          with
            true
          then
            VariantTypeAst_TyVariant
              { info = vBootParser_ginfo t 0, constrs = mapEmpty nameCmp }
          else
            error "Parsing of non-empty variant types not yet supported"
        else match
          __sem_target
        with
          209
        then
          let data =
            let makeData =
              lam positive.
                let cons = setOfSeq nameCmp (map (gname t) (range 1 (glistlen t 0) 1))
                in
                DataTypeAst_TyData
                  { info = vBootParser_ginfo t 0,
                    cons = cons,
                    positive = positive,
                    universe = mapEmpty nameCmp }
            in
            let #var"X" = gint t 0 in
            match
              #var"X"
            with
              0
            then
              UnknownTypeAst_TyUnknown
                { info = vBootParser_ginfo t 0 }
            else match
              #var"X"
            with
              1
            then
              makeData true
            else match
              #var"X"
            with
              2
            then
              makeData false
            else match
              #var"X"
            with
              3
            then
              VarTypeAst_TyVar
                { info = vBootParser_ginfo t 0, ident = gname t 1 }
            else match
              #var"X"
            with
              _
            in
            error "BootParser.matchTerm: Invalid data specifier for TyCon"
          in
          ConTypeAst_TyCon
            { info = vBootParser_ginfo t 0, ident = gname t 0, data = data }
        else match
          __sem_target
        with
          210
        then
          VarTypeAst_TyVar
            { info = vBootParser_ginfo t 0, ident = gname t 0 }
        else match
          __sem_target
        with
          211
        then
          AppTypeAst_TyApp
            { info = vBootParser_ginfo t 0,
              lhs = vBootParser_gtype t 0,
              rhs = vBootParser_gtype t 1 }
        else match
          __sem_target
        with
          212
        then
          TensorTypeAst_TyTensor
            { ty = vBootParser_gtype t 0, info = vBootParser_ginfo t 0 }
        else match
          __sem_target
        with
          213
        then
          let kind =
            let #var"X" = gint t 0 in
            match
              #var"X"
            with
              0
            then
              PolyKindAst_Poly
                {}
            else match
              #var"X"
            with
              1
            then
              let dlen = glistlen t 0 in
              let data =
                unfoldr
                  (lam idx.
                     match
                       lti idx.0 dlen
                     with
                       true
                     then
                       let tname = gname t idx.1 in
                       let totlen = glistlen t idx.0 in
                       let upperidx = glistlen t (addi 1 idx.0) in
                       let minidx = addi 1 idx.1 in
                       let maxidx = addi totlen minidx in
                       let cons = map (gname t) (range minidx maxidx 1) in
                       let ks =
                         match
                           eqi upperidx (negi 1)
                         with
                           true
                         then
                           { lower = setOfSeq nameCmp cons,
                             upper = None
                                 {} }
                         else match
                           splitAt cons upperidx
                         with
                           (lower, upper)
                         in
                         { lower = setOfSeq nameCmp lower,
                             upper =
                               Some
                                 (setOfSeq nameCmp upper) }
                       in
                       Some
                         ((tname, ks), (addi 2 idx.0, maxidx))
                     else
                       None
                         {})
                  (1, 1)
              in
              DataKindAst_Data
                { types = mapFromSeq nameCmp data }
            else match
              #var"X"
            with
              _
            in
            error "BootParser.matchTerm: Invalid data specifier for TyAll!"
          in
          AllTypeAst_TyAll
            { ty = vBootParser_gtype t 0,
              info = vBootParser_ginfo t 0,
              ident = gname t 0,
              kind = kind }
        else match
          __sem_target
        with
          _
        in
        error "Unknown type"
  let vBootParser_matchConst =
    lam t.
      lam __sem_target.
        match
          __sem_target
        with
          300
        then
          BoolAst_CBool
            { val = eqi (gint t 0) 1 }
        else match
          __sem_target
        with
          301
        then
          IntAst_CInt
            { val = gint t 0 }
        else match
          __sem_target
        with
          302
        then
          FloatAst_CFloat
            { val = gfloat t 0 }
        else match
          __sem_target
        with
          303
        then
          CharAst_CChar
            { val = int2char (gint t 0) }
        else match
          __sem_target
        with
          304
        then
          IOAst_CDPrint
            {}
        else match
          __sem_target
        with
          305
        then
          SysAst_CError
            {}
        else match
          __sem_target
        with
          _
        in
        error "Unknown constant"
  let vBootParser_ctGetPatVars =
    lam acc: [[Char]].
      lam __sem_target.
        match
          __sem_target
        with
          NamedPat_PatNamed r
        then
          match
            r.ident
          with
            PName n
          then
            cons (nameGetStr n) acc
          else
            acc
        else match
          __sem_target
        with
          t
        in
        vBootParser_sfold_Pat_Pat vBootParser_ctGetPatVars acc t
  let vBootParser_strToPatName =
    lam __sem_target.
      match
        __sem_target
      with
        ""
      then
        PWildcard
          {}
      else match
        __sem_target
      with
        x
      in
      PName
          (nameNoSym x)
  let vBootParser_sfold_Pat_Pat: all acc. (acc -> Ast_Pat -> acc) -> acc -> Ast_Pat -> acc =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            p
          in
          let res =
              vBootParser_smapAccumL_Pat_Pat
                (lam acc.
                   lam a.
                     (f acc a, a))
                acc
                p
            in
            res.0
  let vBootParser_constTransform =
    lam builtin.
      lam __sem_target.
        match
          __sem_target
        with
          t
        in
        let f =
            lam acc.
              lam v.
                match
                  v
                with
                  (x, c)
                in
                mapInsert x (Some
                       (uconst_ c)) acc
          in
          let env = foldl f (mapEmpty cmpString) builtin in
          let t2 = vBootParser_ctWorker env t in
          t2
  let vBootParser_parseMCoreFile =
    lam arg: BootParser_BootParserParseMCoreFileArg.
      lam __sem_target.
        match
          __sem_target
        with
          filename
        in
        let t =
            bootParserParseMCoreFile
              (arg.keepUtests, arg.pruneExternalUtests, arg.externalsExclude, arg.pruneExternalUtestsWarning, arg.eliminateDeadCode, arg.allowFree)
              arg.keywords
              filename
          in
          vBootParser_constTransform arg.builtin (vBootParser_matchTerm t (bootParserGetId t))
  let vBootParser_smap_Expr_Expr: (Ast_Expr -> Ast_Expr) -> Ast_Expr -> Ast_Expr =
    lam f.
      lam __sem_target.
        match
          __sem_target
        with
          p
        in
        let res =
            vBootParser_smapAccumL_Expr_Expr
              (lam #var"".
                 lam a.
                   ({}, f a))
              {}
              p
          in
          res.1
  let vBootParser_smapAccumL_Pat_Pat: all acc. (acc -> Ast_Pat -> (acc, Ast_Pat)) -> acc -> Ast_Pat -> (acc, Ast_Pat) =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            SeqTotPat_PatSeqTot r
          then
            match
              mapAccumL f acc r.pats
            with
              (acc, pats)
            in
            (acc, SeqTotPat_PatSeqTot
                { r with pats = pats })
          else match
            __sem_target
          with
            SeqEdgePat_PatSeqEdge p
          then
            match
              mapAccumL f acc p.prefix
            with
              (acc, prefix)
            in
            match
                mapAccumL f acc p.postfix
              with
                (acc, postfix)
              in
              (acc, SeqEdgePat_PatSeqEdge
                  { p with prefix = prefix, postfix = postfix })
          else match
            __sem_target
          with
            RecordPat_PatRecord p
          then
            match
              mapMapAccum
                (lam acc.
                   lam #var"".
                     lam p.
                       f acc p)
                acc
                p.bindings
            with
              (acc, bindings)
            in
            (acc, RecordPat_PatRecord
                { p with bindings = bindings })
          else match
            __sem_target
          with
            DataPat_PatCon c
          then
            match
              f acc c.subpat
            with
              (acc, subpat)
            in
            (acc, DataPat_PatCon
                { c with subpat = subpat })
          else match
            __sem_target
          with
            AndPat_PatAnd p
          then
            match
              f acc p.lpat
            with
              (acc, lpat)
            in
            match
                f acc p.rpat
              with
                (acc, rpat)
              in
              (acc, AndPat_PatAnd
                  { p with lpat = lpat, rpat = rpat })
          else match
            __sem_target
          with
            OrPat_PatOr p
          then
            match
              f acc p.lpat
            with
              (acc, lpat)
            in
            match
                f acc p.rpat
              with
                (acc, rpat)
              in
              (acc, OrPat_PatOr
                  { p with lpat = lpat, rpat = rpat })
          else match
            __sem_target
          with
            NotPat_PatNot p
          then
            match
              f acc p.subpat
            with
              (acc, subpat)
            in
            (acc, NotPat_PatNot
                { p with subpat = subpat })
          else match
            __sem_target
          with
            p
          in
          (acc, p)
  let vBootParser_smapAccumL_Expr_Expr: all acc. (acc -> Ast_Expr -> (acc, Ast_Expr)) -> acc -> Ast_Expr -> (acc, Ast_Expr) =
    lam f.
      lam acc.
        lam __sem_target.
          match
            __sem_target
          with
            AppAst_TmApp t
          then
            match
              f acc t.lhs
            with
              (acc, lhs)
            in
            match
                f acc t.rhs
              with
                (acc, rhs)
              in
              (acc, AppAst_TmApp
                  { t with lhs = lhs, rhs = rhs })
          else match
            __sem_target
          with
            LamAst_TmLam t
          then
            match
              f acc t.body
            with
              (acc, body)
            in
            (acc, LamAst_TmLam
                { t with body = body })
          else match
            __sem_target
          with
            LetAst_TmLet t
          then
            match
              f acc t.body
            with
              (acc, body)
            in
            match
                f acc t.inexpr
              with
                (acc, inexpr)
              in
              (acc, LetAst_TmLet
                  { t with body = body, inexpr = inexpr })
          else match
            __sem_target
          with
            RecLetsAst_TmRecLets t
          then
            let bindingFunc =
              lam acc.
                lam b: RecLetsAst_RecLetBinding.
                  match
                    f acc b.body
                  with
                    (acc, body)
                  in
                  (acc, { b with body = body })
            in
            match
              mapAccumL bindingFunc acc t.bindings
            with
              (acc, bindings)
            in
            match
                f acc t.inexpr
              with
                (acc, inexpr)
              in
              (acc, RecLetsAst_TmRecLets
                  { t with bindings = bindings, inexpr = inexpr })
          else match
            __sem_target
          with
            SeqAst_TmSeq t
          then
            match
              mapAccumL f acc t.tms
            with
              (acc, tms)
            in
            (acc, SeqAst_TmSeq
                { t with tms = tms })
          else match
            __sem_target
          with
            RecordAst_TmRecord t
          then
            match
              mapMapAccum
                (lam acc.
                   lam #var"".
                     lam e.
                       f acc e)
                acc
                t.bindings
            with
              (acc, bindings)
            in
            (acc, RecordAst_TmRecord
                { t with bindings = bindings })
          else match
            __sem_target
          with
            RecordAst_TmRecordUpdate t
          then
            match
              f acc t.rec
            with
              (acc, rec)
            in
            match
                f acc t.value
              with
                (acc, value)
              in
              (acc, RecordAst_TmRecordUpdate
                  { t with rec = rec, value = value })
          else match
            __sem_target
          with
            TypeAst_TmType t
          then
            match
              f acc t.inexpr
            with
              (acc, inexpr)
            in
            (acc, TypeAst_TmType
                { t with inexpr = inexpr })
          else match
            __sem_target
          with
            DataAst_TmConDef t
          then
            match
              f acc t.inexpr
            with
              (acc, inexpr)
            in
            (acc, DataAst_TmConDef
                { t with inexpr = inexpr })
          else match
            __sem_target
          with
            DataAst_TmConApp t
          then
            match
              f acc t.body
            with
              (acc, body)
            in
            (acc, DataAst_TmConApp
                { t with body = body })
          else match
            __sem_target
          with
            MatchAst_TmMatch t
          then
            match
              f acc t.target
            with
              (acc, target)
            in
            match
                f acc t.thn
              with
                (acc, thn)
              in
              match
                  f acc t.els
                with
                  (acc, els)
                in
                (acc, MatchAst_TmMatch
                    { t with target = target, thn = thn, els = els })
          else match
            __sem_target
          with
            UtestAst_TmUtest t
          then
            match
              f acc t.test
            with
              (acc, test)
            in
            match
                f acc t.expected
              with
                (acc, expected)
              in
              match
                  f acc t.next
                with
                  (acc, next)
                in
                match
                    optionMapAccum f acc t.tusing
                  with
                    (acc, tusing)
                  in
                  match
                      optionMapAccum f acc t.tonfail
                    with
                      (acc, tonfail)
                    in
                    (acc, UtestAst_TmUtest
                        { t
                          with
                          test = test,
                            expected = expected,
                            next = next,
                            tusing = tusing,
                            tonfail = tonfail })
          else match
            __sem_target
          with
            ExtAst_TmExt t
          then
            match
              f acc t.inexpr
            with
              (acc, inexpr)
            in
            (acc, ExtAst_TmExt
                { t with inexpr = inexpr })
          else match
            __sem_target
          with
            p
          in
          (acc, p)
  let vBootParser__defaultBootParserParseMCoreFileArg: () -> BootParser_BootParserParseMCoreFileArg =
    lam __sem_target.
      match
        __sem_target
      with
        _
      in
      { allowFree = false,
          builtin = builtin,
          keywords = "",
          keepUtests = true,
          externalsExclude = "",
          eliminateDeadCode = true,
          pruneExternalUtests = false,
          pruneExternalUtestsWarning = true }
in
let defaultBootParserParseMCoreFileArg = vBootParser__defaultBootParserParseMCoreFileArg {}
in
type ReturnCode =
  Int
in
type ExecResult =
  {stderr: [Char], stdout: [Char], returncode: ReturnCode}
in
let _pathSep = "/" in
let _tempBase = "/tmp" in
let _null = "/dev/null" in
let sysCommandExists: [Char] -> Bool =
  lam cmd.
    eqi
      0
      (command
         (join
            [ "command -v ",
              cmd,
              " >/dev/null 2>&1" ]))
in
let #var"ASSERT_MKDIR": () =
  match
    sysCommandExists "mkdir"
  with
    true
  then
    {}
  else
    error "Couldn\'t find \'mkdir\' on PATH, exiting."
in
let _commandListTime: [[Char]] -> (Float, Int) =
  lam cmd.
    let cmd = strJoin " " cmd in
    let t1 = wallTimeMs {} in
    let res = command cmd in
    let t2 = wallTimeMs {} in
    (subf t2 t1, res)
in
let _commandList =
  lam cmd: [[Char]].
    match
      _commandListTime cmd
    with
      (_, res)
    in
    res
in
let _commandListTimeoutWrap: Float -> [[Char]] -> [[Char]] =
  lam timeoutSec.
    lam cmd.
      join
        [ [ "timeout",
            "-k",
            "0.1",
            float2string timeoutSec,
            "bash",
            "-c",
            "\'{" ],
          cmd,
          [ "\'}" ] ]
in
let sysFileExists: [Char] -> Bool =
  lam file.
    match
      eqi
        (_commandList [ "test",
             "-e",
             file ])
        0
    with
      true
    then
      true
    else
      false
in
let sysDeleteDir =
  lam dir.
    _commandList [ "rm",
        "-rf",
        dir ]
in
let sysJoinPath =
  lam p1.
    lam p2.
      strJoin _pathSep [ p1,
          p2 ]
in
let sysTempMake =
  lam dir: Bool.
    lam prefix: [Char].
      lam #var"".
        let maxTries = 10000 in
        recursive
          let mk =
            lam base.
              lam i.
                match
                  lti i maxTries
                with
                  true
                then
                  let name = concat base (int2string i) in
                  match
                    _commandList
                      [ match
                          dir
                        with
                          true
                        then
                          "mkdir"
                        else
                          "touch",
                        sysJoinPath _tempBase name,
                        "2>",
                        _null ]
                  with
                    0
                  then
                    name
                  else
                    mk base (addi i 1)
                else
                  error "sysTempMake: Failed to make temporary directory."
        in
        let alphanumStr = create 10 (lam #var"".
               randAlphanum {})
        in
        let base = concat prefix alphanumStr in
        let name = mk base 0 in
        sysJoinPath _tempBase name
in
let sysTempDirMakePrefix: [Char] -> () -> [Char] = sysTempMake true
in
let sysTempDirMake: () -> [Char] = sysTempDirMakePrefix "miking-tmp."
in
let sysTempDirDelete = lam td.
    lam #var"".
      sysDeleteDir td
in
let sysRunCommandWithTimingTimeoutFileIO: Option Float -> [[Char]] -> [Char] -> [Char] -> [Char] -> [Char] -> (Float, ReturnCode) =
  lam timeoutSec.
    lam cmd.
      lam stdinFile.
        lam stdoutFile.
          lam stderrFile.
            lam cwd.
              let fullCmd =
                [ "cd",
                  cwd,
                  ";",
                  strJoin " " cmd,
                  ">",
                  stdoutFile,
                  "2>",
                  stderrFile,
                  "<",
                  stdinFile,
                  ";" ]
              in
              let fullCmd =
                match
                  timeoutSec
                with
                  Some timeout
                then
                  _commandListTimeoutWrap timeout fullCmd
                else
                  fullCmd
              in
              match
                _commandListTime fullCmd
              with
                (ms, retCode)
              in
              (ms, retCode)
in
let sysRunCommandWithTimingTimeout: Option Float -> [[Char]] -> [Char] -> [Char] -> (Float, ExecResult) =
  lam timeoutSec.
    lam cmd.
      lam stdin.
        lam cwd.
          let tempDir = sysTempDirMake {} in
          let tempStdin = sysJoinPath tempDir "stdin.txt" in
          (writeFile tempStdin stdin)
          ; let tempStdout = sysJoinPath tempDir "stdout.txt" in
          let tempStderr = sysJoinPath tempDir "stderr.txt" in
          let res =
            sysRunCommandWithTimingTimeoutFileIO timeoutSec cmd tempStdin tempStdout tempStderr cwd
          in
          (_commandList
               [ "echo",
                 "",
                 ">>",
                 tempStdout ])
          ; (_commandList
               [ "echo",
                 "",
                 ">>",
                 tempStderr ])
          ; let stdout = init (readFile tempStdout) in
          let stderr = init (readFile tempStderr) in
          (sysTempDirDelete tempDir {})
          ; (res.0, { stderr = stderr, stdout = stdout, returncode = res.1 })
in
let sysRunCommandWithTiming: [[Char]] -> [Char] -> [Char] -> (Float, ExecResult) = sysRunCommandWithTimingTimeout (None
       {})
in
let sysRunCommand: [[Char]] -> [Char] -> [Char] -> ExecResult =
  lam cmd.
    lam stdin.
      lam cwd.
        match
          sysRunCommandWithTiming cmd stdin cwd
        with
          (_, res)
        in
        res
in
let sysGetCwd: () -> [Char] =
  lam #var"".
    strTrim (sysRunCommand [ "pwd" ] "" ".").stdout
in
let sysGetEnv: [Char] -> Option [Char] =
  lam env.
    let res =
      strTrim
        (sysRunCommand [ "echo",
             concat "$" env ] "" ".").stdout
    in
    match
      null res
    with
      true
    then
      None
        {}
    else
      Some
        res
in
let implWithLibs =
  lam arg: {ty: Ast_Type, expr: [Char], libraries: [[Char]]}.
    { ty = arg.ty,
      expr = arg.expr,
      libraries = arg.libraries,
      cLibraries = "" }
in
let impl =
  lam arg: {ty: Ast_Type, expr: [Char]}.
    implWithLibs { ty = arg.ty, expr = arg.expr, libraries = "" }
in
let myrecty1 =
  otyrecordext_
    (otyvarext_ "Boot.Exttest.myrec1_t" "")
    [ { ty = tyint_, label = "a", asLabel = "c" },
      { ty = tyfloat_, label = "b", asLabel = "d" } ]
in
let myrecty2 =
  otyrecordext_
    (otyvarext_ "Boot.Exttest.myrec2_t" "")
    [ { ty = tyint_, label = "b", asLabel = "d" },
      { ty = otylist_ tyint_, label = "a", asLabel = "c" } ]
in
let myrecty3 =
  otyrecordext_
    (otyvarext_ "Boot.Exttest.myrec3_t" "")
    [ { ty = myrecty1, label = "a", asLabel = "c" },
      { ty = myrecty2, label = "b", asLabel = "d" } ]
in
let extTestMap =
  mapFromSeq
    cmpString
    [ ("extTestListOfLists", [ impl
          { ty = otylist_ (otylist_ tyint_),
            expr = "Boot.Exttest.list_of_lists" } ]),
      ("extTestListHeadHead", [ impl
          { ty =
              tyarrow_ (otylist_ (otylist_ (otyparam_ "a"))) (otyparam_ "a"),
            expr = "Boot.Exttest.list_hd_hd" } ]),
      ("extTestArrayOfArrays", [ impl
          { ty = otyarray_ (otyarray_ tyint_),
            expr = "Boot.Exttest.array_of_arrays" } ]),
      ("extTestArrayHeadHead", [ impl
          { ty =
              tyarrow_ (otyarray_ (otyarray_ (otyparam_ "a"))) (otyparam_ "a"),
            expr = "Boot.Exttest.array_hd_hd" } ]),
      ("extTestFlip", [ impl
          { ty =
              tyarrows_
                [ tyarrows_
                    [ otyparam_ "a",
                      otyparam_ "b",
                      otyparam_ "c" ],
                  otyparam_ "b",
                  otyparam_ "a",
                  otyparam_ "c" ],
            expr = "Fun.flip" } ]),
      ("extTestUnit1", [ impl
          { ty = tyarrow_ tyint_ (otytuple_ ""),
            expr = "Boot.Exttest.unit1" } ]),
      ("extTestUnit2", [ impl
          { ty = tyarrow_ (otytuple_ "") tyint_,
            expr = "Boot.Exttest.unit2" } ]),
      ("extTestTuple1", [ impl
          { ty = otytuple_ [ tyint_,
                  tyfloat_ ],
            expr = "Boot.Exttest.tuple1" } ]),
      ("extTestTuple2", [ impl
          { ty = otytuple_ [ otylist_ tyint_,
                  tyint_ ],
            expr = "Boot.Exttest.tuple2" } ]),
      ("extTestTuple10th", [ impl
          { ty =
              tyarrow_
                (otytuple_ [ tyint_,
                     tyfloat_ ]) tyint_,
            expr = "Boot.Exttest.tuple1_0th" } ]),
      ("extTestTuple20th", [ impl
          { ty =
              tyarrow_
                (otytuple_ [ otylist_ tyint_,
                     tyint_ ])
                (otylist_ tyint_),
            expr = "Boot.Exttest.tuple2_0th" } ]),
      ("extTestRecord1", [ impl { ty = myrecty1, expr = "Boot.Exttest.myrec1" } ]),
      ("extTestRecord1A", [ impl
          { ty = tyarrow_ myrecty1 tyint_, expr = "Boot.Exttest.myrec1_a" } ]),
      ("extTestRecord2", [ impl { ty = myrecty2, expr = "Boot.Exttest.myrec2" } ]),
      ("extTestRecord2A", [ impl
          { ty = tyarrow_ myrecty2 (otylist_ tyint_),
            expr = "Boot.Exttest.myrec2_a" } ]),
      ("extTestRecord3", [ impl { ty = myrecty3, expr = "Boot.Exttest.myrec3" } ]),
      ("extTestRecord3BA", [ impl
          { ty = tyarrow_ myrecty3 (otylist_ tyint_),
            expr = "Boot.Exttest.myrec3_b_a" } ]),
      ("extTestArgLabel", [ impl
          { ty =
              tyarrows_
                [ otylabel_ "b" tyint_,
                  otylabel_ "a" tyint_,
                  tyint_ ],
            expr = "Boot.Exttest.arg_label" } ]),
      ("extTestGenarrIntNumDims", [ impl
          { ty = tyarrow_ otygenarrayclayoutint_ tyint_,
            expr = "Bigarray.Genarray.num_dims" } ]),
      ("extTestGenarrFloatNumDims", [ impl
          { ty = tyarrow_ otygenarrayclayoutfloat_ tyint_,
            expr = "Bigarray.Genarray.num_dims" } ]),
      ("extTestGenarrIntSliceLeft", [ impl
          { ty =
              tyarrows_
                [ otygenarrayclayoutint_,
                  otyarray_ tyint_,
                  otygenarrayclayoutint_ ],
            expr = "Bigarray.Genarray.slice_left" } ]),
      ("extTestGenarrFloatSliceLeft", [ impl
          { ty =
              tyarrows_
                [ otygenarrayclayoutfloat_,
                  otyarray_ tyint_,
                  otygenarrayclayoutfloat_ ],
            expr = "Bigarray.Genarray.slice_left" } ]),
      ("extTestArray2IntSliceLeft", [ impl
          { ty =
              tyarrows_
                [ otybaarrayclayoutint_ 2,
                  tyint_,
                  otybaarrayclayoutint_ 1 ],
            expr = "Bigarray.Array2.slice_left" } ]),
      ("extTestArray2FloatSliceLeft", [ impl
          { ty =
              tyarrows_
                [ otybaarrayclayoutfloat_ 2,
                  tyint_,
                  otybaarrayclayoutfloat_ 1 ],
            expr = "Bigarray.Array2.slice_left" } ]),
      ("extTestArray2IntOfGenarr", [ impl
          { ty = tyarrow_ otygenarrayclayoutint_ (otybaarrayclayoutint_ 2),
            expr = "Bigarray.array2_of_genarray" } ]),
      ("extTestArray2FloatOfGenarr", [ impl
          { ty =
              tyarrow_ otygenarrayclayoutfloat_ (otybaarrayclayoutfloat_ 2),
            expr = "Bigarray.array2_of_genarray" } ]),
      ("extTestZero", [ impl { ty = tyfloat_, expr = "Float.zero" } ]),
      ("extTestExp", [ impl { ty = tyarrow_ tyfloat_ tyfloat_, expr = "Float.exp" } ]),
      ("extTestListMap", [ impl
          { ty =
              tyarrows_
                [ tyarrow_ (otyparam_ "a") (otyparam_ "b"),
                  otylist_ (otyparam_ "a"),
                  otylist_ (otyparam_ "b") ],
            expr = "List.map" } ]),
      ("extTestListConcatMap", [ impl
          { ty =
              tyarrows_
                [ tyarrow_ (otyparam_ "a") (otylist_ (otyparam_ "b")),
                  otylist_ (otyparam_ "a"),
                  otylist_ (otyparam_ "b") ],
            expr = "List.concat_map" } ]),
      ("extTestNonExistant", [ implWithLibs { ty = tyint_, expr = "none", libraries = [ "no-lib" ] } ]) ]
in
let impl =
  lam arg: {ty: Ast_Type, expr: [Char]}.
    { ty = arg.ty, expr = arg.expr, libraries = "", cLibraries = "" }
in
let mathExtMap =
  mapFromSeq
    cmpString
    [ ("externalExp", [ impl { ty = tyarrow_ tyfloat_ tyfloat_, expr = "Float.exp" } ]),
      ("externalLog", [ impl { ty = tyarrow_ tyfloat_ tyfloat_, expr = "Float.log" } ]),
      ("externalAtan", [ impl { ty = tyarrow_ tyfloat_ tyfloat_, expr = "Float.atan" } ]),
      ("externalSin", [ impl { ty = tyarrow_ tyfloat_ tyfloat_, expr = "Float.sin" } ]),
      ("externalCos", [ impl { ty = tyarrow_ tyfloat_ tyfloat_, expr = "Float.cos" } ]),
      ("externalAtan2", [ impl
          { ty =
              tyarrows_
                [ tyfloat_,
                  tyfloat_,
                  tyfloat_ ],
            expr = "Float.atan2" } ]),
      ("externalPow", [ impl
          { ty =
              tyarrows_
                [ tyfloat_,
                  tyfloat_,
                  tyfloat_ ],
            expr = "Float.pow" } ]),
      ("externalSqrt", [ impl { ty = tyarrow_ tyfloat_ tyfloat_, expr = "Float.sqrt" } ]),
      ("externalLogGamma", [ { ty = tyarrows_ [ tyfloat_,
                tyfloat_ ],
          expr = "Owl_maths.loggamma ",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalLogCombination", [ { ty =
            tyarrows_
              [ tyint_,
                tyint_,
                tyfloat_ ],
          expr = "Owl_maths.log_combination ",
          libraries = [ "owl" ],
          cLibraries = "" } ]) ]
in
let distExtMap =
  mapFromSeq
    cmpString
    [ ("externalExponentialSample", [ { ty =
            tyarrows_ [ otylabel_ "lambda" tyfloat_,
                tyfloat_ ],
          expr = "Owl_stats.exponential_rvs",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalGammaLogPdf", [ { ty =
            tyarrows_
              [ tyfloat_,
                otylabel_ "shape" tyfloat_,
                otylabel_ "scale" tyfloat_,
                tyfloat_ ],
          expr = "Owl_stats.gamma_logpdf",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalGammaSample", [ { ty =
            tyarrows_
              [ otylabel_ "shape" tyfloat_,
                otylabel_ "scale" tyfloat_,
                tyfloat_ ],
          expr = "Owl_stats.gamma_rvs",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalBinomialLogPmf", [ { ty =
            tyarrows_
              [ tyint_,
                otylabel_ "p" tyfloat_,
                otylabel_ "n" tyint_,
                tyfloat_ ],
          expr = "Owl_stats.binomial_logpdf",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalBinomialSample", [ { ty =
            tyarrows_
              [ otylabel_ "p" tyfloat_,
                otylabel_ "n" tyint_,
                tyint_ ],
          expr = "Owl_stats.binomial_rvs",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalBetaLogPdf", [ { ty =
            tyarrows_
              [ tyfloat_,
                otylabel_ "a" tyfloat_,
                otylabel_ "b" tyfloat_,
                tyfloat_ ],
          expr = "Owl_stats.beta_logpdf",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalBetaSample", [ { ty =
            tyarrows_
              [ otylabel_ "a" tyfloat_,
                otylabel_ "b" tyfloat_,
                tyfloat_ ],
          expr = "Owl_stats.beta_rvs",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalGaussianLogPdf", [ { ty =
            tyarrows_
              [ tyfloat_,
                otylabel_ "mu" tyfloat_,
                otylabel_ "sigma" tyfloat_,
                tyfloat_ ],
          expr = "Owl_stats.gaussian_logpdf",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalGaussianSample", [ { ty =
            tyarrows_
              [ otylabel_ "mu" tyfloat_,
                otylabel_ "sigma" tyfloat_,
                tyfloat_ ],
          expr = "Owl_stats.gaussian_rvs",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalMultinomialLogPmf", [ { ty =
            tyarrows_
              [ otyarray_ tyint_,
                otylabel_ "p" (otyarray_ tyfloat_),
                tyfloat_ ],
          expr = "Owl_stats.multinomial_logpdf ",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalMultinomialSample", [ { ty =
            tyarrows_
              [ tyint_,
                otylabel_ "p" (otyarray_ tyfloat_),
                otyarray_ tyint_ ],
          expr = "Owl_stats.multinomial_rvs ",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalCategoricalSample", [ { ty = tyarrows_ [ otyarray_ tyfloat_,
                tyint_ ],
          expr = "Owl_stats.categorical_rvs ",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalDirichletLogPdf", [ { ty =
            tyarrows_
              [ otyarray_ tyfloat_,
                otylabel_ "alpha" (otyarray_ tyfloat_),
                tyfloat_ ],
          expr = "Owl_stats.dirichlet_logpdf ",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalDirichletSample", [ { ty =
            tyarrows_
              [ otylabel_ "alpha" (otyarray_ tyfloat_),
                otyarray_ tyfloat_ ],
          expr = "Owl_stats.dirichlet_rvs ",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalUniformContinuousSample", [ { ty =
            tyarrows_
              [ tyfloat_,
                tyfloat_,
                tyfloat_ ],
          expr = "Owl_stats.uniform_rvs",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalUniformDiscreteSample", [ { ty =
            tyarrows_ [ tyint_,
                tyint_,
                tyint_ ],
          expr = "Owl_stats.uniform_int_rvs",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalLomaxSample", [ { ty =
            tyarrows_
              [ tyfloat_,
                tyfloat_,
                tyfloat_ ],
          expr = "Owl_stats.lomax_rvs",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalLomaxLogPdf", [ { ty =
            tyarrows_
              [ tyfloat_,
                otylabel_ "shape" tyfloat_,
                otylabel_ "scale" tyfloat_,
                tyfloat_ ],
          expr = "Owl_stats.lomax_logpdf",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalSetSeed", [ { ty = tyarrows_ [ tyint_,
                otyunit_ ],
          expr =
            "\n        fun seed -> (\n          Random.init seed;\n          Owl_base_stats_prng.init seed;\n          Owl_stats_prng.sfmt_seed seed;\n          Owl_stats_prng.ziggurat_init ()\n        )",
          libraries = [ "owl" ],
          cLibraries = "" } ]) ]
in
let matrixExtMap =
  mapFromSeq
    cmpString
    [ ("externalMatrixExponential", [ { ty =
            tyarrows_
              [ otygenarrayclayoutfloat_,
                otygenarrayclayoutfloat_ ],
          expr = "Owl_linalg_generic.expm",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalMatrixTranspose", [ { ty =
            tyarrows_
              [ otygenarrayclayoutfloat_,
                otygenarrayclayoutfloat_ ],
          expr = "Owl_dense.Matrix.D.transpose",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalMatrixMulFloat", [ { ty =
            tyarrows_
              [ tyfloat_,
                otygenarrayclayoutfloat_,
                otygenarrayclayoutfloat_ ],
          expr = "Owl_dense.Matrix.D.( $* )",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalMatrixMul", [ { ty =
            tyarrows_
              [ otygenarrayclayoutfloat_,
                otygenarrayclayoutfloat_,
                otygenarrayclayoutfloat_ ],
          expr = "Owl_dense.Matrix.D.( *@ )",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalMatrixElemMul", [ { ty =
            tyarrows_
              [ otygenarrayclayoutfloat_,
                otygenarrayclayoutfloat_,
                otygenarrayclayoutfloat_ ],
          expr = "Owl_dense.Matrix.D.( * )",
          libraries = [ "owl" ],
          cLibraries = "" } ]),
      ("externalMatrixElemAdd", [ { ty =
            tyarrows_
              [ otygenarrayclayoutfloat_,
                otygenarrayclayoutfloat_,
                otygenarrayclayoutfloat_ ],
          expr = "Owl_dense.Matrix.D.( + )",
          libraries = [ "owl" ],
          cLibraries = "" } ]) ]
in
let fileExtMap =
  mapFromSeq
    cmpString
    [ ("externalFileExists", [ { ty = tyarrows_ [ otystring_,
                tybool_ ],
          expr = "(fun s -> try Sys.file_exists s with _ -> false)",
          libraries = "",
          cLibraries = "" } ]),
      ("externalDeleteFile", [ { ty = tyarrows_ [ otystring_,
                otyunit_ ],
          expr = "(fun s -> try Sys.remove s with _ -> ())",
          libraries = "",
          cLibraries = "" } ]),
      ("externalFileSize", [ { ty = tyarrows_ [ otystring_,
                tyint_ ],
          expr =
            concat
              "(fun n -> try let f = open_in_bin n in let s = in_channel_length "
              "f in let _ = close_in_noerr f in s with _ -> 0)",
          libraries = "",
          cLibraries = "" } ]),
      ("externalWriteOpen", [ { ty =
            tyarrows_
              [ otystring_,
                otytuple_
                  [ otyvarext_ "out_channel" "",
                    tybool_ ] ],
          expr =
            "(fun s -> try (open_out_bin s, true) with _ -> (stdout, false))",
          libraries = "",
          cLibraries = "" } ]),
      ("externalWriteString", [ { ty =
            tyarrows_
              [ otyvarext_ "out_channel" "",
                otystring_,
                otyunit_ ],
          expr = "output_string",
          libraries = "",
          cLibraries = "" } ]),
      ("externalWriteFlush", [ { ty =
            tyarrows_ [ otyvarext_ "out_channel" "",
                otyunit_ ],
          expr = "flush",
          libraries = "",
          cLibraries = "" } ]),
      ("externalWriteClose", [ { ty =
            tyarrows_ [ otyvarext_ "out_channel" "",
                otyunit_ ],
          expr = "close_out_noerr",
          libraries = "",
          cLibraries = "" } ]),
      ("externalReadOpen", [ { ty =
            tyarrows_
              [ otystring_,
                otytuple_ [ otyvarext_ "in_channel" "",
                    tybool_ ] ],
          expr =
            "(fun s -> try (open_in_bin s, true) with _ -> (stdin, false))",
          libraries = "",
          cLibraries = "" } ]),
      ("externalReadLine", [ { ty =
            tyarrows_
              [ otyvarext_ "in_channel" "",
                otytuple_ [ otystring_,
                    tybool_ ] ],
          expr =
            "(fun rc -> try (input_line rc, false) with | End_of_file -> (\"\",true))",
          libraries = "",
          cLibraries = "" } ]),
      ("externalReadBytes", [ { ty =
            tyarrows_
              [ otyvarext_ "in_channel" "",
                tyint_,
                otytuple_
                  [ otylist_ tyint_,
                    tybool_,
                    tybool_ ] ],
          expr =
            "(fun rc len -> try let buf = Bytes.create len in let actual_len = input rc buf 0 len in let reached_eof = actual_len < len in let had_error = false in let int_list = List.init actual_len (fun i -> int_of_char (Bytes.get buf i)) in (int_list, reached_eof, had_error) with | Sys_error err -> ([], false, true))",
          libraries = "",
          cLibraries = "" } ]),
      ("externalReadString", [ { ty =
            tyarrows_ [ otyvarext_ "in_channel" "",
                otystring_ ],
          expr =
            "(fun f -> try really_input_string f (in_channel_length f) with _ -> \"\")",
          libraries = "",
          cLibraries = "" } ]),
      ("externalReadClose", [ { ty =
            tyarrows_ [ otyvarext_ "in_channel" "",
                otyunit_ ],
          expr = "close_in_noerr",
          libraries = "",
          cLibraries = "" } ]),
      ("externalStdin", [ { ty = otyvarext_ "in_channel" "",
          expr = "stdin",
          libraries = "",
          cLibraries = "" } ]),
      ("externalStdout", [ { ty = otyvarext_ "out_channel" "",
          expr = "stdout",
          libraries = "",
          cLibraries = "" } ]),
      ("externalStderr", [ { ty = otyvarext_ "out_channel" "",
          expr = "stderr",
          libraries = "",
          cLibraries = "" } ]) ]
in
let tyTomlTable_ = otyvarext_ "Toml.Types.table" "" in
let tyTomlValue_ = otyvarext_ "Toml.Types.value" "" in
let impl =
  lam arg: {ty: Ast_Type, expr: [Char]}.
    { ty = arg.ty,
      expr = arg.expr,
      libraries = [ "toml" ],
      cLibraries = "" }
in
let tomlExtMap =
  mapFromSeq
    cmpString
    [ ("externalTomlFromStringExn", [ impl
          { ty = tyarrows_ [ otystring_,
                  tyTomlTable_ ],
            expr =
              "fun s ->\n        match Toml.Parser.from_string s with\n        | `Ok table -> table\n        | `Error (str, loc) ->\n          raise (Invalid_argument (\"tomlFromStringExn: \"\n            ^ str ^ \" when parsing: \"\n            ^ s))" } ]),
      ("externalTomlFindExn", [ impl
          { ty =
              tyarrows_
                [ otystring_,
                  tyTomlTable_,
                  tyTomlValue_ ],
            expr =
              "fun key table -> Toml.Types.Table.find (Toml.Min.key key) table" } ]),
      ("externalTomlBindings", [ impl
          { ty =
              tyarrows_
                [ tyTomlTable_,
                  otylist_
                    (otytuple_ [ otystring_,
                         tyTomlValue_ ]) ],
            expr =
              "fun table ->\n        List.map\n          (fun (k, v) -> (Toml.Types.Table.Key.to_string k, v))\n          (Toml.Types.Table.bindings table)\n      " } ]),
      ("externalTomlValueToIntExn", [ impl
          { ty = tyarrows_ [ tyTomlValue_,
                  tyint_ ],
            expr =
              "fun v -> match v with\n         | Toml.Types.TInt v -> v\n         | _ -> raise (Invalid_argument (\"tomlValueToIntExn: \" ^ (Toml.Printer.string_of_value v)))\n      " } ]),
      ("externalTomlValueToStringExn", [ impl
          { ty = tyarrows_ [ tyTomlValue_,
                  otystring_ ],
            expr =
              "fun v -> match v with\n         | Toml.Types.TString v -> v\n         | _ -> raise (Invalid_argument (\"tomlValueToStringExn: \" ^ (Toml.Printer.string_of_value v)))\n      " } ]),
      ("externalTomlValueToFloatExn", [ impl
          { ty = tyarrows_ [ tyTomlValue_,
                  tyfloat_ ],
            expr =
              "fun v -> match v with\n         | Toml.Types.TFloat v -> v\n         | _ -> raise (Invalid_argument (\"tomlValueToFloatExn: \" ^ (Toml.Printer.string_of_value v)))\n      " } ]),
      ("externalTomlValueToBoolExn", [ impl
          { ty = tyarrows_ [ tyTomlValue_,
                  tybool_ ],
            expr =
              "fun v -> match v with\n         | Toml.Types.TBool v -> v\n         | _ -> raise (Invalid_argument (\"tomlValueToBoolExn: \" ^ (Toml.Printer.string_of_value v)))\n      " } ]),
      ("externalTomlValueToTableExn", [ impl
          { ty =
              tyarrows_ [ tyTomlValue_,
                  tyTomlTable_ ],
            expr =
              "fun v -> match v with\n         | Toml.Types.TTable v -> v\n         | _ -> raise (Invalid_argument (\"tomlValueToTableExn: \" ^ (Toml.Printer.string_of_value v)))\n      " } ]),
      ("externalTomlValueToIntSeqExn", [ impl
          { ty =
              tyarrows_ [ tyTomlValue_,
                  otylist_ tyint_ ],
            expr =
              "fun v -> match v with\n         | Toml.Types.TArray (Toml.Types.NodeInt v) -> v\n         | Toml.Types.TArray Toml.Types.NodeEmpty -> []\n         | _ -> raise (Invalid_argument (\"tomlValueToIntSeqExn: \" ^ (Toml.Printer.string_of_value v)))\n      " } ]),
      ("externalTomlValueToStringSeqExn", [ impl
          { ty =
              tyarrows_ [ tyTomlValue_,
                  otylist_ otystring_ ],
            expr =
              "fun v -> match v with\n         | Toml.Types.TArray (Toml.Types.NodeString v) -> v\n         | Toml.Types.TArray Toml.Types.NodeEmpty -> []\n         | _ ->\n           raise (Invalid_argument (\"tomlValueToStringSeqExn: \" ^ (Toml.Printer.string_of_value v)))\n      " } ]),
      ("externalTomlValueToFloatSeqExn", [ impl
          { ty =
              tyarrows_ [ tyTomlValue_,
                  otylist_ tyfloat_ ],
            expr =
              "fun v -> match v with\n         | Toml.Types.TArray (Toml.Types.NodeFloat v) -> v\n         | Toml.Types.TArray Toml.Types.NodeEmpty -> []\n         | _ -> raise (Invalid_argument (\"tomlValueToFloatSeqExn: \" ^ (Toml.Printer.string_of_value v)))\n      " } ]),
      ("externalTomlValueToTableSeqExn", [ impl
          { ty =
              tyarrows_ [ tyTomlValue_,
                  otylist_ tyTomlTable_ ],
            expr =
              "fun v -> match v with\n         | Toml.Types.TArray (Toml.Types.NodeTable v) -> v\n         | Toml.Types.TArray Toml.Types.NodeEmpty -> []\n         | _ -> raise (Invalid_argument (\"tomlValueToTableSeqExn: \" ^ (Toml.Printer.string_of_value v)))\n      " } ]),
      ("externalTomlValueToSeqSeqExn", [ impl
          { ty =
              tyarrows_ [ tyTomlValue_,
                  otylist_ tyTomlValue_ ],
            expr =
              "fun v -> match v with\n         | Toml.Types.TArray (Toml.Types.NodeArray a) ->\n           List.map (fun e -> Toml.Types.TArray e) a\n         | _ -> raise (Invalid_argument (\"tomlValueToSeqSeqExn: \" ^ (Toml.Printer.string_of_value v)))\n      " } ]),
      ("externalTomlValueToString", [ impl
          { ty = tyarrows_ [ tyTomlValue_,
                  otystring_ ],
            expr = "Toml.Printer.string_of_value" } ]),
      ("externalTomlToString", [ impl
          { ty = tyarrows_ [ tyTomlTable_,
                  otystring_ ],
            expr = "Toml.Printer.string_of_table" } ]),
      ("externalTomlFromBindings", [ impl
          { ty =
              tyarrows_
                [ otylist_
                    (otytuple_ [ otystring_,
                         tyTomlValue_ ]),
                  tyTomlTable_ ],
            expr =
              "fun binds ->\n         Toml.Types.Table.of_key_values\n           (List.map (fun (k, v) -> (Toml.Types.Table.Key.of_string k, v)) binds)" } ]),
      ("externalTomlIntToValue", [ impl
          { ty = tyarrows_ [ tyint_,
                  tyTomlValue_ ],
            expr = "fun x -> Toml.Types.TInt x" } ]),
      ("externalTomlStringToValue", [ impl
          { ty = tyarrows_ [ otystring_,
                  tyTomlValue_ ],
            expr = "fun x -> Toml.Types.TString x" } ]),
      ("externalTomlFloatToValue", [ impl
          { ty = tyarrows_ [ tyfloat_,
                  tyTomlValue_ ],
            expr = "fun x -> Toml.Types.TFloat x" } ]),
      ("externalTomlTableToValue", [ impl
          { ty =
              tyarrows_ [ tyTomlTable_,
                  tyTomlValue_ ],
            expr = "fun x -> Toml.Types.TTable x" } ]),
      ("externalTomlIntSeqToValue", [ impl
          { ty =
              tyarrows_ [ otylist_ tyint_,
                  tyTomlValue_ ],
            expr = "fun x -> Toml.Types.TArray (Toml.Types.NodeInt x)" } ]),
      ("externalTomlStringSeqToValue", [ impl
          { ty =
              tyarrows_ [ otylist_ otystring_,
                  tyTomlValue_ ],
            expr = "fun x -> Toml.Types.TArray (Toml.Types.NodeString x)" } ]),
      ("externalTomlFloatSeqToValue", [ impl
          { ty =
              tyarrows_ [ otylist_ tyfloat_,
                  tyTomlValue_ ],
            expr = "fun x -> Toml.Types.TArray (Toml.Types.NodeFloat x)" } ]),
      ("externalTomlTableSeqToValue", [ impl
          { ty =
              tyarrows_ [ otylist_ tyTomlTable_,
                  tyTomlValue_ ],
            expr = "fun x -> Toml.Types.TArray (Toml.Types.NodeTable x)" } ]),
      ("externalTomlSeqSeqToValue", [ impl
          { ty =
              tyarrows_ [ otylist_ tyTomlValue_,
                  tyTomlValue_ ],
            expr =
              "fun x ->\n         let a = List.map (fun e -> match e with\n           | Toml.Types.TArray a -> a\n           | _ ->\n             raise (Invalid_argument (\"tomlSeqSeqToValue: \"\n                                     ^ (Toml.Printer.string_of_value e)))) x in\n         Toml.Types.TArray (Toml.Types.NodeArray a)\n      " } ]) ]
in
let asyncExtMap =
  mapFromSeq
    cmpString
    [ ("asyncSleepSec", [ { ty =
            tyarrows_ [ tyfloat_,
                otyvarext_ "\'a Lwt.t" "" ],
          expr = "Lwt_unix.sleep",
          libraries = [ "lwt.unix" ],
          cLibraries = "" } ]),
      ("asyncRun", [ { ty =
            tyarrows_
              [ otyvarext_ "\'a Lwt.t" "",
                otyvarext_ "\'a" "" ],
          expr = "Lwt_main.run",
          libraries = [ "lwt.unix" ],
          cLibraries = "" } ]),
      ("asyncBind", [ { ty =
            tyarrows_
              [ otyvarext_ "\'a Lwt.t" "",
                tyarrows_
                  [ otyvarext_ "\'a" "",
                    otyvarext_ "\'b Lwt.t" "" ],
                otyvarext_ "\'b Lwt.t" "" ],
          expr = "Lwt.bind",
          libraries = [ "lwt.unix" ],
          cLibraries = "" } ]),
      ("asyncPrint", [ { ty =
            tyarrows_ [ otystring_,
                otyvarext_ "unit Lwt.t" "" ],
          expr = "Lwt_io.print",
          libraries = [ "lwt.unix" ],
          cLibraries = "" } ]),
      ("asyncReturn", [ { ty =
            tyarrows_
              [ otyvarext_ "\'a" "",
                otyvarext_ "\'a Lwt.t" "" ],
          expr = "Lwt.return",
          libraries = [ "lwt.unix" ],
          cLibraries = "" } ]) ]
in
let tyts_ = tytuple_ [ tyint_,
      tyunknown_ ] in
let impl =
  lam arg: {ty: Ast_Type, expr: [Char]}.
    [ { ty = arg.ty,
        expr = arg.expr,
        libraries = [ "rtppl-support" ],
        cLibraries = [ "rt" ] } ]
in
let timespec = otytuple_ [ tyint_,
      tyint_ ] in
let readDistTy =
  lam ty.
    otyarray_ (otytuple_ [ tyfloat_,
           ty ])
in
let writeDistTy =
  lam ty.
    otytuple_ [ otyarray_ ty,
        otyarray_ tyfloat_ ]
in
let rtpplExtMap =
  mapFromSeq
    cmpString
    [ ("setSigintHandler", impl
        { ty = tyarrow_ (tyarrow_ tyint_ otyunit_) otyunit_,
          expr = "Rtppl.set_signal_handler Sys.sigint" }),
      ("getMonotonicTime", impl
        { ty = tyarrow_ otyunit_ timespec,
          expr = "Rtppl.get_monotonic_time" }),
      ("getWallClockTime", impl
        { ty = tyarrow_ otyunit_ timespec,
          expr = "Rtppl.get_wall_clock_time" }),
      ("getProcessCpuTime", impl
        { ty = tyarrow_ otyunit_ timespec,
          expr = "Rtppl.get_process_cpu_time" }),
      ("clockNanosleep", impl
        { ty = tyarrow_ timespec otyunit_,
          expr = "Rtppl.clock_nanosleep" }),
      ("rtpplSetMaxPriority", impl
        { ty = tyarrow_ otyunit_ tyint_,
          expr = "Rtppl.set_max_priority" }),
      ("rtpplSetPriority", impl
        { ty = tyarrow_ tyint_ tyint_, expr = "Rtppl.set_priority" }),
      ("rtpplOpenFileDescriptor", impl
        { ty =
            tyarrows_
              [ otystring_,
                tyint_,
                tyint_ ],
          expr = "Rtppl.open_file_descriptor" }),
      ("rtpplCloseFileDescriptor", impl
        { ty = tyarrow_ tyint_ otyunit_,
          expr = "Rtppl.close_file_descriptor" }),
      ("rtpplReadInt", impl
        { ty =
            tyarrow_
              tyint_
              (otyarray_ (otytuple_ [ timespec,
                      tyint_ ])),
          expr = "Rtppl.read_int" }),
      ("rtpplWriteInt", impl
        { ty =
            tyarrows_
              [ tyint_,
                otytuple_ [ timespec,
                    tyint_ ],
                otyunit_ ],
          expr = "Rtppl.write_int" }),
      ("rtpplReadFloat", impl
        { ty =
            tyarrow_
              tyint_
              (otyarray_ (otytuple_ [ timespec,
                      tyfloat_ ])),
          expr = "Rtppl.read_float" }),
      ("rtpplWriteFloat", impl
        { ty =
            tyarrows_
              [ tyint_,
                otytuple_ [ timespec,
                    tyfloat_ ],
                otyunit_ ],
          expr = "Rtppl.write_float" }),
      ("rtpplReadIntRecord", impl
        { ty =
            tyarrows_
              [ tyint_,
                tyint_,
                otyarray_ (otytuple_ [ timespec,
                       tyunknown_ ]) ],
          expr = "Rtppl.read_int_record" }),
      ("rtpplWriteIntRecord", impl
        { ty =
            tyarrows_
              [ tyint_,
                tyint_,
                otytuple_ [ timespec,
                    tyunknown_ ],
                otyunit_ ],
          expr = "Rtppl.write_int_record" }),
      ("rtpplReadFloatRecord", impl
        { ty =
            tyarrows_
              [ tyint_,
                tyint_,
                otyarray_ (otytuple_ [ timespec,
                       tyunknown_ ]) ],
          expr = "Rtppl.read_float_record" }),
      ("rtpplWriteFloatRecord", impl
        { ty =
            tyarrows_
              [ tyint_,
                tyint_,
                otytuple_ [ timespec,
                    tyunknown_ ],
                otyunit_ ],
          expr = "Rtppl.write_float_record" }),
      ("rtpplReadDistFloat", impl
        { ty =
            tyarrow_
              tyint_
              (otyarray_
                 (otytuple_ [ timespec,
                      readDistTy tyfloat_ ])),
          expr = "Rtppl.read_dist_float" }),
      ("rtpplWriteDistFloat", impl
        { ty =
            tyarrows_
              [ tyint_,
                otytuple_ [ timespec,
                    writeDistTy tyfloat_ ],
                otyunit_ ],
          expr = "Rtppl.write_dist_float" }),
      ("rtpplReadDistFloatRecord", impl
        { ty =
            tyarrows_
              [ tyint_,
                tyint_,
                otyarray_
                  (otytuple_ [ timespec,
                       readDistTy tyunknown_ ]) ],
          expr = "Rtppl.read_dist_float_record" }),
      ("rtpplWriteDistFloatRecord", impl
        { ty =
            tyarrows_
              [ tyint_,
                tyint_,
                otytuple_ [ timespec,
                    writeDistTy tyunknown_ ],
                otyunit_ ],
          expr = "Rtppl.write_dist_float_record" }) ]
in
let impl =
  lam arg: {ty: Ast_Type, expr: [Char]}.
    { ty = arg.ty,
      expr = arg.expr,
      libraries = [ "boot" ],
      cLibraries = "" }
in
let arrayExtMap =
  mapFromSeq
    cmpString
    [ ("arrayCreateFloat", [ impl
          { ty = tyarrow_ tyint_ otyopaque_, expr = "Array.create_float" } ]),
      ("arrayLength", [ impl { ty = tyarrow_ otyopaque_ tyint_, expr = "Array.length" } ]),
      ("arrayGet", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  tyint_,
                  otyopaque_ ],
            expr = "Array.get" } ]),
      ("arraySet", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  tyint_,
                  otyopaque_,
                  otyunit_ ],
            expr = "Array.set" } ]),
      ("cArray1CreateFloat", [ impl
          { ty = tyarrow_ tyint_ otyopaque_,
            expr =
              "Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout" } ]),
      ("cArray1Dim", [ impl
          { ty = tyarrow_ otyopaque_ tyint_, expr = "Bigarray.Array1.dim" } ]),
      ("cArray1Get", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  tyint_,
                  otyopaque_ ],
            expr = "Bigarray.Array1.get" } ]),
      ("cArray1Set", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  tyint_,
                  otyopaque_,
                  otyunit_ ],
            expr = "Bigarray.Array1.set" } ]) ]
in
let impl =
  lam arg: {ty: Ast_Type, expr: [Char]}.
    { ty = arg.ty,
      expr = arg.expr,
      libraries = [ "sundialsml" ],
      cLibraries = "" }
in
let sundialsExtMap =
  mapFromSeq
    cmpString
    [ ("nvectorSerialWrap", [ impl
          { ty = tyarrow_ (otybaarrayclayoutfloat_ 1) otyopaque_,
            expr = "Nvector_serial.wrap" } ]),
      ("nvectorSerialUnwrap", [ impl
          { ty = tyarrow_ otyopaque_ (otybaarrayclayoutfloat_ 1),
            expr = "Nvector_serial.unwrap" } ]),
      ("sundialsMatrixDense", [ impl
          { ty = tyarrows_ [ tyint_,
                  otyopaque_ ],
            expr = "Sundials.Matrix.dense" } ]),
      ("sundialsMatrixDenseUnwrap", [ impl
          { ty =
              tyarrows_ [ otyopaque_,
                  otybaarrayclayoutfloat_ 2 ],
            expr = "Sundials.Matrix.Dense.unwrap" } ]),
      ("sundialsMatrixDenseGet", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  tyint_,
                  tyint_,
                  tyfloat_ ],
            expr = "Sundials.Matrix.Dense.get" } ]),
      ("sundialsMatrixDenseSet", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  tyint_,
                  tyint_,
                  tyfloat_,
                  otyunit_ ],
            expr = "Sundials.Matrix.Dense.set" } ]),
      ("sundialsMatrixDenseUpdate", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  tyint_,
                  tyint_,
                  tyarrow_ tyfloat_ tyfloat_,
                  otyunit_ ],
            expr = "Sundials.Matrix.Dense.update" } ]),
      ("sundialsNonlinearSolverNewtonMake", [ impl
          { ty = tyarrows_ [ otyopaque_,
                  otyopaque_ ],
            expr = "Sundials_NonlinearSolver.Newton.make" } ]),
      ("sundialsNonlinearSolverFixedPointMake", [ impl
          { ty =
              tyarrows_
                [ otylabel_ "acceleration_vectors" tyint_,
                  otyopaque_,
                  otyopaque_ ],
            expr = "Sundials_NonlinearSolver.FixedPoint.make" } ]) ]
in
let impl =
  lam arg: {ty: Ast_Type, expr: [Char]}.
    { ty = arg.ty,
      expr = arg.expr,
      libraries = [ "sundialsml" ],
      cLibraries = "" }
in
let tyrealarray = otyvarext_ "Sundials.RealArray.t" "" in
let tyidatriple = otyvarext_ "Ida.triple" in
let tyidajacobianarg =
  otyvarext_
    "Ida.jacobian_arg"
    [ tyidatriple [ tyrealarray ],
      tyrealarray ]
in
let tyidajacf =
  tyarrows_
    [ otyrecordext_
        tyidajacobianarg
        [ { ty = tyfloat_, label = "jac_t", asLabel = "t" },
          { ty = otybaarrayclayoutfloat_ 1, label = "jac_y", asLabel = "y" },
          { ty = otybaarrayclayoutfloat_ 1,
            label = "jac_y\'",
            asLabel = "yp" },
          { ty = otybaarrayclayoutfloat_ 1,
            label = "jac_res",
            asLabel = "res" },
          { ty = tyfloat_, label = "jac_coef", asLabel = "c" },
          { ty =
              otytuple_
                [ otybaarrayclayoutfloat_ 1,
                  otybaarrayclayoutfloat_ 1,
                  otybaarrayclayoutfloat_ 1 ],
            label = "jac_tmp",
            asLabel = "tmp" } ],
      otyopaque_,
      otyunit_ ]
in
let tyidaresf =
  tyarrows_
    [ tyfloat_,
      otybaarrayclayoutfloat_ 1,
      otybaarrayclayoutfloat_ 1,
      otybaarrayclayoutfloat_ 1,
      otyunit_ ]
in
let tyidarootf = tyidaresf in
let idaExtMap =
  mapFromSeq
    cmpString
    [ ("idaDlsDense", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  otyopaque_,
                  otyopaque_ ],
            expr = "Ida.Dls.dense" } ]),
      ("idaDlsSolverJacf", [ impl
          { ty =
              tyarrows_
                [ otylabel_ "jac" tyidajacf,
                  otyopaque_,
                  otyopaque_ ],
            expr = "Ida.Dls.solver" } ]),
      ("idaDlsSolver", [ impl
          { ty = tyarrows_ [ otyopaque_,
                  otyopaque_ ],
            expr = "Ida.Dls.solver" } ]),
      ("idaVarIdAlgebraic", [ impl { ty = tyfloat_, expr = "Ida.VarId.algebraic" } ]),
      ("idaVarIdDifferential", [ impl { ty = tyfloat_, expr = "Ida.VarId.differential" } ]),
      ("idaSSTolerances", [ impl
          { ty =
              tyarrows_
                [ tyfloat_,
                  tyfloat_,
                  otyopaque_ ],
            expr = "fun rtol atol -> Ida.SStolerances (rtol, atol)" } ]),
      ("idaRetcode", [ impl
          { ty = tyarrow_ otyopaque_ tyint_,
            expr =
              "function\n  | Ida.Success ->\n      0\n  | Ida.StopTimeReached ->\n      1\n  | Ida.RootsFound ->\n      2" } ]),
      ("idaInit", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  otylabel_ "nlsolver" otyopaque_,
                  otylabel_ "lsolver" otyopaque_,
                  tyidaresf,
                  otylabel_ "varid" otyopaque_,
                  otylabel_
                    "roots"
                    (otytuple_ [ tyint_,
                         tyidarootf ]),
                  tyfloat_,
                  otyopaque_,
                  otyopaque_,
                  otyopaque_ ],
            expr = "Ida.init" } ]),
      ("idaCalcICYaYd", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  tyfloat_,
                  otyopaque_,
                  otyopaque_,
                  tyint_ ],
            expr =
              "\nbegin\n let calc_ic_ya_yd\' s t y y\' =\n   try Ida.calc_ic_ya_yd\' s t ~y:y ~y\':y\'; 0 with\n   | Ida.IdNotSet -> 1\n   | _ -> 2\n in calc_ic_ya_yd\'\nend\n" } ]),
      ("idaSolveNormal", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  tyfloat_,
                  otyopaque_,
                  otyopaque_,
                  otytuple_
                    [ tyfloat_,
                      otytuple_ [ tyint_,
                          tyint_ ] ] ],
            expr =
              "\nbegin\n  let solve_normal s tend y y\' =\n    try\n      match Ida.solve_normal s tend y y\' with\n      | (tend, Ida.Success) -> (tend, (0, -1))\n      | (tend, Ida.RootsFound) -> (tend, (1, -1))\n      | (tend, Ida.StopTimeReached ) -> (tend, (2, -1))\n    with\n    | Ida.IllInput -> (tend, (3, 0))\n    | Ida.TooMuchWork -> (tend, (3, 1))\n    | Ida.TooMuchAccuracy -> (tend, (3, 2))\n    | Ida.ErrFailure -> (tend, (3, 3))\n    | Ida.ConvergenceFailure -> (tend, (3, 4))\n    | Ida.LinearInitFailure -> (tend, (3, 5))\n    | Ida.LinearSetupFailure _ -> (tend, (3, 6))\n    | Ida.LinearSolveFailure _ -> (tend, (3, 7))\n    | Ida.ConstraintFailure -> (tend, (3, 8))\n    | Ida.FirstResFuncFailure -> (tend, (3, 9))\n    | Ida.RepeatedResFuncFailure -> (tend, (3, 10))\n    | Ida.ResFuncFailure -> (tend, (3, 11))\n    | Ida.RootFuncFailure -> (tend, (3, 12))\n    | _ -> (tend, (3, -1))\n  in solve_normal\nend\n" } ]),
      ("idaReinit", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  otylabel_
                    "roots"
                    (otytuple_ [ tyint_,
                         tyidarootf ]),
                  tyfloat_,
                  otyopaque_,
                  otyopaque_,
                  otyunit_ ],
            expr = "Ida.reinit" } ]),
      ("idaSetStopTime", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  tyfloat_,
                  otyunit_ ],
            expr = "Ida.set_stop_time" } ]) ]
in
let impl =
  lam arg: {ty: Ast_Type, expr: [Char]}.
    { ty = arg.ty,
      expr = arg.expr,
      libraries = [ "sundialsml" ],
      cLibraries = "" }
in
let tycvoderhs =
  tyarrows_
    [ tyfloat_,
      otybaarrayclayoutfloat_ 1,
      otybaarrayclayoutfloat_ 1,
      otyunit_ ]
in
let cvodeExtMap =
  mapFromSeq
    cmpString
    [ ("cvodeDlsDense", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  otyopaque_,
                  otyopaque_ ],
            expr = "Cvode.Dls.dense" } ]),
      ("cvodeDlsSolver", [ impl
          { ty = tyarrows_ [ otyopaque_,
                  otyopaque_ ],
            expr = "Cvode.Dls.solver" } ]),
      ("cvodeSSTolerances", [ impl
          { ty =
              tyarrows_
                [ tyfloat_,
                  tyfloat_,
                  otyopaque_ ],
            expr = "fun rtol atol -> Cvode.SStolerances (rtol, atol)" } ]),
      ("cvodeLMMAdams", [ impl { ty = otyopaque_, expr = "Cvode.Adams" } ]),
      ("cvodeLMMBDF", [ impl { ty = otyopaque_, expr = "Cvode.BDF" } ]),
      ("cvodeInit", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  otyopaque_,
                  otylabel_ "lsolver" otyopaque_,
                  tycvoderhs,
                  tyfloat_,
                  otyopaque_,
                  otyopaque_ ],
            expr = "Cvode.init" } ]),
      ("cvodeSolveNormal", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  tyfloat_,
                  otyopaque_,
                  otytuple_
                    [ tyfloat_,
                      otytuple_ [ tyint_,
                          tyint_ ] ] ],
            expr =
              "\nbegin\n  let solve_normal s tend y =\n    try\n      match Cvode.solve_normal s tend y with\n      | (tend, Cvode.Success) -> (tend, (0, -1))\n      | (tend, Cvode.RootsFound) -> (tend, (1, -1))\n      | (tend, Cvode.StopTimeReached ) -> (tend, (2, -1))\n    with\n    | Cvode.IllInput -> (tend, (3, 0))\n    | Cvode.TooClose -> (tend, (3, 1))\n    | Cvode.TooMuchWork -> (tend, (3, 2))\n    | Cvode.TooMuchAccuracy -> (tend, (3, 3))\n    | Cvode.ErrFailure -> (tend, (3, 4))\n    | Cvode.ConvergenceFailure -> (tend, (3, 5))\n    | Cvode.LinearInitFailure -> (tend, (3, 6))\n    | Cvode.LinearSetupFailure _ -> (tend, (3, 7))\n    | Cvode.LinearSolveFailure _ -> (tend, (3, 8))\n    | Cvode.RhsFuncFailure -> (tend, (3, 9))\n    | Cvode.FirstRhsFuncFailure -> (tend, (3, 10))\n    | Cvode.RepeatedRhsFuncFailure -> (tend, (3, 11))\n    | Cvode.UnrecoverableRhsFuncFailure -> (tend, (3, 12))\n    | Cvode.RootFuncFailure -> (tend, (3, 13))\n    | _ -> (tend, (3, -1))\n  in solve_normal\nend\n" } ]),
      ("cvodeSetStopTime", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  tyfloat_,
                  otyunit_ ],
            expr = "Cvode.set_stop_time" } ]) ]
in
let impl =
  lam arg: {ty: Ast_Type, expr: [Char]}.
    { ty = arg.ty,
      expr = arg.expr,
      libraries = [ "sundialsml" ],
      cLibraries = "" }
in
let tykinsolresf =
  tyarrows_
    [ otybaarrayclayoutfloat_ 1,
      otybaarrayclayoutfloat_ 1,
      otyunit_ ]
in
let kinsolExtMap =
  mapFromSeq
    cmpString
    [ ("kinsolDlsDense", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  otyopaque_,
                  otyopaque_ ],
            expr = "Kinsol.Dls.dense" } ]),
      ("kinsolDlsSolver", [ impl
          { ty = tyarrows_ [ otyopaque_,
                  otyopaque_ ],
            expr = "Kinsol.Dls.solver" } ]),
      ("cvodeSSTolerances", [ impl
          { ty =
              tyarrows_
                [ tyfloat_,
                  tyfloat_,
                  otyopaque_ ],
            expr = "fun rtol atol -> Cvode.SStolerances (rtol, atol)" } ]),
      ("kinsolNewton", [ impl { ty = otyopaque_, expr = "Kinsol.Newton" } ]),
      ("kinsolLineSearch", [ impl { ty = otyopaque_, expr = "Kinsol.LineSearch" } ]),
      ("kinsolPicard", [ impl { ty = otyopaque_, expr = "Kinsol.Picard" } ]),
      ("kinsolFixedPoint", [ impl { ty = otyopaque_, expr = "Kinsol.FixedPoint" } ]),
      ("kinsolInit", [ impl
          { ty =
              tyarrows_
                [ otylabel_ "lsolver" otyopaque_,
                  tykinsolresf,
                  otyopaque_,
                  otyopaque_ ],
            expr = "Kinsol.init" } ]),
      ("kinsolSolve", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  otyopaque_,
                  tyint_,
                  otyopaque_,
                  otyopaque_,
                  otytuple_ [ tyint_,
                      tyint_ ] ],
            expr =
              "\nbegin\n  let solve s u strategy u_scale f_scale =\n    try\n      match Kinsol.solve s u strategy u_scale f_scale with\n      | Kinsol.Success -> (0, -1)\n      | Kinsol.InitialGuessOK -> (1, -1)\n      | Kinsol.StoppedOnStepTol -> (2, -1)\n    with\n    | Kinsol.MissingLinearSolver -> (3, 0)  \n    | Kinsol.IllInput -> (3, 1) \n    | Kinsol.LineSearchNonConvergence -> (3, 2) \n    | Kinsol.MaxIterationsReached -> (3, 3) \n    | Kinsol.MaxNewtonStepExceeded -> (3, 4) \n    | Kinsol.LineSearchBetaConditionFailure -> (3, 5)\n    | Kinsol.LinearSolverNoRecovery -> (3, 6)\n    | Kinsol.LinearSolverInitFailure -> (3, 7)\n    | Kinsol.LinearSetupFailure _ -> (3, 8) \n    | Kinsol.LinearSolveFailure _ -> (3, 9) \n    | Kinsol.SystemFunctionFailure -> (3, 10) \n    | Kinsol.FirstSystemFunctionFailure -> (3, 11) \n    | Kinsol.RepeatedSystemFunctionFailure -> (3, 12)     \n  in solve\nend\n" } ]) ]
in
let impl =
  lam arg: {ty: Ast_Type, expr: [Char]}.
    { ty = arg.ty, expr = arg.expr, libraries = "", cLibraries = "" }
in
let tyaref_ = lam #var"".
    tyunknown_ in
let tygeneric_ = lam #var"".
    tyunknown_ in
let atomicExtMap =
  mapFromSeq
    cmpString
    [ ("externalAtomicMake", [ impl
          { ty = tyarrow_ (tygeneric_ "a") (tyaref_ "a"),
            expr = "Atomic.make" } ]),
      ("externalAtomicGet", [ impl
          { ty = tyarrow_ (tyaref_ "a") (tygeneric_ "a"),
            expr = "Atomic.get" } ]),
      ("externalAtomicExchange", [ impl
          { ty =
              tyarrows_
                [ tyaref_ "a",
                  tygeneric_ "a",
                  tygeneric_ "a" ],
            expr = "Atomic.exchange" } ]),
      ("externalAtomicCAS", [ impl
          { ty =
              tyarrows_
                [ tyaref_ "a",
                  tygeneric_ "a",
                  tygeneric_ "a",
                  tybool_ ],
            expr = "Atomic.compare_and_set" } ]),
      ("externalAtomicFetchAndAdd", [ impl
          { ty =
              tyarrows_
                [ tyaref_ "Int",
                  tyint_,
                  tyint_ ],
            expr = "Atomic.fetch_and_add" } ]) ]
in
let impl =
  lam arg: {ty: Ast_Type, expr: [Char]}.
    { ty = arg.ty, expr = arg.expr, libraries = "", cLibraries = "" }
in
let tyathread_ = lam #var"".
    tyunknown_ in
let tygeneric_ = lam #var"".
    tyunknown_ in
let threadExtMap =
  mapFromSeq
    cmpString
    [ ("externalThreadSpawn", [ impl
          { ty =
              tyarrow_ (tyarrow_ otyunit_ (tygeneric_ "a")) (tyathread_ "a"),
            expr = "Domain.spawn" } ]),
      ("externalThreadJoin", [ impl
          { ty = tyarrow_ (tyathread_ "a") (tygeneric_ "a"),
            expr = "Domain.join" } ]),
      ("externalThreadGetID", [ impl
          { ty = tyarrow_ (tyathread_ "a") tyint_, expr = "Domain.get_id" } ]),
      ("externalThreadSelf", [ impl { ty = tyarrow_ otyunit_ tyint_, expr = "Domain.self" } ]),
      ("externalThreadCPURelax", [ impl
          { ty = tyarrow_ otyunit_ otyunit_, expr = "Domain.cpu_relax" } ]) ]
in
let impl =
  lam arg: {ty: Ast_Type, expr: [Char]}.
    { ty = arg.ty, expr = arg.expr, libraries = "", cLibraries = "" }
in
let tymutex_ = tyunknown_ in
let mutexExtMap =
  mapFromSeq
    cmpString
    [ ("externalMutexCreate", [ impl { ty = tyarrow_ otyunit_ tymutex_, expr = "Mutex.create" } ]),
      ("externalMutexLock", [ impl { ty = tyarrow_ tymutex_ otyunit_, expr = "Mutex.lock" } ]),
      ("externalMutexRelease", [ impl { ty = tyarrow_ tymutex_ otyunit_, expr = "Mutex.unlock" } ]),
      ("externalMutexTryLock", [ impl { ty = tyarrow_ tymutex_ tybool_, expr = "Mutex.try_lock" } ]) ]
in
let impl =
  lam arg: {ty: Ast_Type, expr: [Char]}.
    { ty = arg.ty, expr = arg.expr, libraries = "", cLibraries = "" }
in
let tycond_ = tyunknown_ in
let condExtMap =
  mapFromSeq
    cmpString
    [ ("externalCondCreate", [ impl
          { ty = tyarrow_ otyunit_ tycond_, expr = "Condition.create" } ]),
      ("externalCondWait", [ impl
          { ty =
              tyarrows_
                [ tycond_,
                  tyunknown_,
                  otyunit_ ],
            expr = "Condition.wait" } ]),
      ("externalCondSignal", [ impl
          { ty = tyarrow_ tycond_ otyunit_, expr = "Condition.signal" } ]),
      ("externalCondBroadcast", [ impl
          { ty = tyarrow_ tycond_ otyunit_, expr = "Condition.broadcast" } ]) ]
in
let impl =
  lam arg: {ty: Ast_Type, expr: [Char]}.
    { ty = arg.ty,
      expr = arg.expr,
      libraries = [ "ipoptml" ],
      cLibraries = [ "ipopt" ] }
in
let tyvec = otybaarrayclayoutfloat_ 1 in
let tyevalf = tyarrow_ tyvec tyfloat_ in
let tyevalgradf = tyarrows_ [ tyvec,
      tyvec,
      otyunit_ ]
in
let tyevalg = tyarrows_ [ tyvec,
      tyvec,
      otyunit_ ]
in
let tystructure = otyarray_ (otytuple_ [ tyint_,
         tyint_ ])
in
let tyevaljacg = tyarrows_ [ tyvec,
      tyvec,
      otyunit_ ]
in
let tyevalh =
  tyarrows_
    [ otylabel_ "sigma" tyfloat_,
      otylabel_ "x" tyvec,
      otylabel_ "lambda" tyvec,
      otylabel_ "h" tyvec,
      otyunit_ ]
in
let ipoptExtMap =
  mapFromSeq
    cmpString
    [ ("externalIpoptApplicationReturnStatusRetcode", [ impl
          { ty = tyarrow_ otyopaque_ tyint_,
            expr = "Ipoptml.application_return_status_retcode" } ]),
      ("externalIpoptCreateNLP", [ impl
          { ty =
              tyarrows_
                [ otylabel_ "eval_f" tyevalf,
                  otylabel_ "eval_grad_f" tyevalgradf,
                  otylabel_ "eval_g" tyevalg,
                  otylabel_ "jac_g_structure" tystructure,
                  otylabel_ "eval_jac_g" tyevaljacg,
                  otylabel_ "h_structure" tystructure,
                  otylabel_ "eval_h" tyevalh,
                  otylabel_ "lb" tyvec,
                  otylabel_ "ub" tyvec,
                  otylabel_ "constraints_lb" tyvec,
                  otylabel_ "constraints_ub" tyvec,
                  otyopaque_ ],
            expr = "Ipoptml.create_nlp" } ]),
      ("externalIpoptAddStrOption", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  otystring_,
                  otystring_,
                  otyunit_ ],
            expr = "Ipoptml.add_str_option" } ]),
      ("externalIpoptAddNumOption", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  otystring_,
                  tyfloat_,
                  otyunit_ ],
            expr = "Ipoptml.add_num_option" } ]),
      ("externalIpoptAddIntOption", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  otystring_,
                  tyint_,
                  otyunit_ ],
            expr = "Ipoptml.add_int_option" } ]),
      ("externalIpoptSolve", [ impl
          { ty =
              tyarrows_
                [ otyopaque_,
                  tyvec,
                  otyopaque_ ],
            expr = "Ipoptml.solve" } ]) ]
in
type ExternalImpl =
  {ty: Ast_Type, expr: [Char], libraries: [[Char]], cLibraries: [[Char]]}
in
let globalExternalImplsMap: Map [Char] [ExternalImpl] =
  foldl1
    mapUnion
    [ extTestMap,
      mathExtMap,
      arrayExtMap,
      sundialsExtMap,
      idaExtMap,
      cvodeExtMap,
      kinsolExtMap,
      atomicExtMap,
      threadExtMap,
      mutexExtMap,
      condExtMap,
      distExtMap,
      matrixExtMap,
      fileExtMap,
      ipoptExtMap,
      tomlExtMap,
      asyncExtMap,
      rtpplExtMap ]
in
let externalListOcamlPackages: () -> [([Char], [Char])] =
  lam #var"".
    let res = mapEmpty cmpString in
    match
      sysCommandExists "dune"
    with
      true
    then
      let tempDir = sysTempDirMake {} in
      match
        sysRunCommand
          [ "dune",
            "installed-libraries",
            "--root",
            tempDir ]
          ""
          "."
      with
        {stdout = stdout, returncode = returncode}
      in
      (sysTempDirDelete tempDir {})
        ; match
          eqi 0 returncode
        with
          true
        then
          let pkgs = map (strSplit "(version: ") (strSplit "\n" stdout)
          in
          let pkgs =
            map
              (lam x.
                 (strTrim (head x), init (join (tail x))))
              pkgs
          in
          pkgs
        else
          (printError
               (join
                  [ "externalListOcamlPackages: failed to run `dune installed-libraries`",
                    " cannot automatically find ocaml packages available the system\n" ]))
          ; flushStderr
          ; ""
    else
      (printError
           (join
              [ "externalListOcamlPackages: dune not in PATH, cannot",
                " automatically find ocaml packages available the system\n" ]))
      ; flushStderr
      ; ""
in
let externalGetSupportedExternalImpls: () -> Map [Char] [ExternalImpl] =
  lam #var"".
    let syslibs =
      setOfSeq
        cmpString
        (map
           (lam x: ([Char], [Char]).
              x.0)
           (externalListOcamlPackages {}))
    in
    match
      setIsEmpty syslibs
    with
      true
    then
      globalExternalImplsMap
    else
      let newMap = mapEmpty cmpString in
      mapFoldWithKey
        (lam acc: Map [Char] [ExternalImpl].
           lam x: [Char].
             lam impls: [ExternalImpl].
               let impls =
                 filter
                   (lam impl: ExternalImpl.
                      forAll
                        (lam lib.
                           setMem lib syslibs)
                        impl.libraries)
                   impls
               in
               match
                 impls
               with
                 ""
               then
                 acc
               else
                 mapInsert x impls acc)
        (mapEmpty cmpString)
        globalExternalImplsMap
in
type GenerateEnv =
  {exts: Map Name [ExternalImpl], constrs: Map Name Ast_Type, records: Map (Map SID Ast_Type) Name}
in
let emptyGenerateEnv =
  { constrs = mapEmpty nameCmp,
    exts = mapEmpty nameCmp,
    records = mapEmpty (mapCmp vMExprCmp_cmpType) }
in
type ParseOptions =
  {keywords: [[Char]], keepUtests: Bool, eliminateDeadCode: Bool, pruneExternalUtests: Bool, findExternalsExclude: Bool, pruneExternalUtestsWarning: Bool}
in
let parseParseMCoreFile: ParseOptions -> [Char] -> Ast_Expr =
  lam opt.
    lam file.
      match
        opt.pruneExternalUtests
      with
        true
      then
        let externalsExclude =
          match
            opt.findExternalsExclude
          with
            true
          then
            mapKeys (externalGetSupportedExternalImpls {})
          else
            ""
        in
        vBootParser_parseMCoreFile
          { defaultBootParserParseMCoreFileArg
            with
            keepUtests = opt.keepUtests,
              pruneExternalUtests = true,
              externalsExclude = externalsExclude,
              pruneExternalUtestsWarning = opt.pruneExternalUtestsWarning,
              eliminateDeadCode = opt.eliminateDeadCode,
              keywords = opt.keywords }
          file
      else
        vBootParser_parseMCoreFile
          { defaultBootParserParseMCoreFileArg
            with
            keepUtests = opt.keepUtests,
              pruneExternalUtests = false,
              externalsExclude = "",
              pruneExternalUtestsWarning = false,
              eliminateDeadCode = opt.eliminateDeadCode,
              keywords = opt.keywords }
          file
in
type ProfileEnv =
  Map Name (Int, Info)
in
let _profilerReportCodeRef = ref (None
       {}) in
con VariantNameTypeAst_TyVariantName: {info: Info, ident: Name} -> Ast_Type in
type TypeLiftBase_TypeLiftEnv =
  {seqs: Map Ast_Type Name, records: Map (Map SID Ast_Type) Name, tensors: Map Ast_Type Name, typeEnv: AssocSeq Name Ast_Type, variants: Map Name (Map Name Ast_Type)}
in
let stdlibCwd = join [ sysGetCwd {},
      "/stdlib" ] in
let stdlibLocUnix =
  match
    sysGetEnv "HOME"
  with
    Some path
  then
    join [ path,
        "/.local/lib/mcore/stdlib" ]
  else
    stdlibCwd
in
let stdlibLoc =
  optionGetOrElse
    (lam #var"".
       match
         sysFileExists stdlibLocUnix
       with
         true
       then
         stdlibLocUnix
       else
         stdlibCwd)
    (optionBind
       (sysGetEnv "MCORE_LIBS")
       (lam path.
          let libs = strSplit ":" path in
          findMap
            (lam str.
               match
                 splitAt str 7
               with
                 ("stdlib=", loc)
               then
                 Some
                   loc
               else
                 None
                   {})
            libs))
in
type MExprEliminateDuplicateCode_Definition =
  (Info, [Char])
in
type MExprEliminateDuplicateCode_DuplicateCodeEnv =
  {defIds: Map MExprEliminateDuplicateCode_Definition Name, replace: Map Name Name}
in
let _utestRuntimeCode = ref (None
       {}) in
let _utestRuntimeIds = ref (None
       {}) in
let _pprintId = ref 0 in
let _eqId = ref 0 in
let _ppSeqName = nameSym "ppSeq" in
let _ppSeqTyVarName = nameSym "a" in
let _eqSeqName = nameSym "eqSeq" in
let _eqSeqTyVarName = nameSym "a" in
let _ppTensorName = nameSym "ppTensor" in
let _ppTensorTyVarName = nameSym "a" in
let _eqTensorName = nameSym "eqTensor" in
let _eqTensorTyVarName = nameSym "a" in
let _builtinTypes = map (lam s.
       nameNoSym s.0) builtinTypes
in
let _utestInfo = let pos = initPos "utest-generated" in
  makeInfo pos pos
in
let _recordTy =
  lam fields.
    let fields =
      map
        (lam f.
           match
             f
           with
             (s, ty)
           in
           (stringToSid s, ty))
        fields
    in
    RecordTypeAst_TyRecord
      { info = _utestInfo, fields = mapFromSeq cmpSID fields }
in
let _unitTy = _recordTy "" in
let _record =
  lam binds.
    lam ty.
      let binds =
        map
          (lam b.
             match
               b
             with
               (s, e)
             in
             (stringToSid s, e))
          binds
      in
      RecordAst_TmRecord
        { ty = ty, info = _utestInfo, bindings = mapFromSeq cmpSID binds }
in
let _unit = _record "" _unitTy in
type UtestBase_UtestEnv =
  {eq: Map Ast_Type Name, eqDef: Set Ast_Type, pprint: Map Ast_Type Name, variants: Map Name (Map Name Ast_Type), pprintDef: Set Ast_Type}
in
type LogLevel =
  Int
in
let logLevel = { info = 3, off = 0, debug = 4, error = 1, warning = 2 }
in
let _logLevel = ref logLevel.info in
type DigraphEdge v l =
  (v, v, l)
in
type Digraph v l =
  {adj: Map v [(v, l)], eql: l -> l -> Bool}
in
type Successors v =
  {i: Int, comps: [[v]], stack: [v], number: Map v Int, lowlink: Map v Int}
in
type SideEffectEnv =
  {arityId: Map Name Int, sideEffectId: Set Name}
in
type PEvalCtx_PEvalCtx =
  {env: Eval_EvalEnv, freeVar: Set Name, recFlag: Bool, effectEnv: SideEffectEnv, maxRecDepth: Int}
in
con ClosPAst_TmClosP: {cls: {env: ClosAst_Lazy Eval_EvalEnv, body: Ast_Expr, info: Info, ident: Name}, ident: Option Name, isRecursive: Bool} -> Ast_Expr in
type PEvalLetInlineOrRemove
in
con PEvalLetInline: () -> PEvalLetInlineOrRemove in
con PEvalLetRemove: () -> PEvalLetInlineOrRemove in
con SpecializeAst_TmSpecialize: {e: Ast_Expr, info: Info} -> Ast_Expr in
let compileFunc =
  lam uri.
    lam content.
      (eprintln "Parsing Mcore program")
      ; (eprintln uri)
      ; let strippedUri = stripUriProtocol uri in
      let ast =
        parseParseMCoreFile
          { keywords = "",
            keepUtests = false,
            eliminateDeadCode = false,
            pruneExternalUtests = true,
            pruneExternalUtestsWarning = true,
            findExternalsExclude = false }
          strippedUri
      in
      let implementations: LSPImplementations = { hover = "" } in
      Right
        (ast, implementations)
in
let environment: LSPEnvironment = { files = mapEmpty cmpString }
in
(eprintln "Miking Probtime LSP started")
; (readJsonRPC compileFunc environment)
; eprintln "Miking Probtime LSP ended"
