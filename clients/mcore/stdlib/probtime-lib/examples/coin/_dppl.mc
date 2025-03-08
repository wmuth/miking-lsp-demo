Task cf:
let and: Bool -> Bool -> Bool =
  lam a24: Bool.
    lam b8: Bool.
      match
        a24
      with
        true
      then
        b8
      else
        false
in
type Option a
in
con Some: all a1. a1 -> Option a1 in
con None: all a2. () -> Option a2 in
type These a3 b
in
type Either a4 b1
in
con Left: all a5. all b2. a5 -> Either a5 b2 in
con Right: all a6. all b3. b3 -> Either a6 b3 in
recursive
  let work1: all b7. all a23. (a23 -> b7 -> Bool) -> [a23] -> [b7] -> Bool =
    lam eq1.
      lam s114.
        lam s214.
          match
            (s114, s214)
          with
            ([ h12 ] ++ t13, [ h22 ] ++ t22)
          then
            match
              eq1 h12 h22
            with
              true
            then
              work1 eq1 t13 t22
            else
              false
          else
            true
in
let eqSeq: all a22. all b6. (a22 -> b6 -> Bool) -> [a22] -> [b6] -> Bool =
  lam eq: a22 -> b6 -> Bool.
    lam s113: [a22].
      lam s213: [b6].
        let n12 = length s113 in
        let n22 = length s213 in
        let ndiff1 = subi n12 n22 in
        match
          eqi ndiff1 0
        with
          true
        then
          work1 eq s113 s213
        else
          false
in
let join: all a21. [[a21]] -> [a21] = lam seqs: [[a21]].
    foldl concat "" seqs
in
recursive
  let find: all a20. (a20 -> Bool) -> [a20] -> Option a20 =
    lam p1: a20 -> Bool.
      lam seq: [a20].
        match
          null seq
        with
          true
        then
          None
            {}
        else match
          p1 (head seq)
        with
          true
        then
          Some
            (head seq)
        else
          find p1 (tail seq)
in
recursive
  let work: all a19. (a19 -> a19 -> Int) -> [a19] -> [a19] -> Int =
    lam cmp4.
      lam s112.
        lam s212.
          match
            (s112, s212)
          with
            ([ h11 ] ++ t12, [ h21 ] ++ t21)
          then
            let c6 = cmp4 h11 h21 in
            match
              eqi c6 0
            with
              true
            then
              work cmp4 t12 t21
            else
              c6
          else
            0
in
let seqCmp: all a18. (a18 -> a18 -> Int) -> [a18] -> [a18] -> Int =
  lam cmp3: a18 -> a18 -> Int.
    lam s111: [a18].
      lam s211: [a18].
        let n11 = length s111 in
        let n21 = length s211 in
        let ndiff = subi n11 n21 in
        match
          eqi ndiff 0
        with
          true
        then
          work cmp3 s111 s211
        else
          ndiff
in
recursive
  let seqJoin: all a17. [a17] -> [[a17]] -> [a17] =
    lam delim: [a17].
      lam ss: [[a17]].
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
let printLn =
  lam s45.
    let #var"43" = print (concat s45 "\n") in
    flushStdout {}
in
let eqChar = lam c14.
    lam c24.
      eqc c14 c24 in
let leqChar =
  lam c13.
    lam c23.
      leqi (char2int c13) (char2int c23)
in
let geqChar =
  lam c12.
    lam c22.
      geqi (char2int c12) (char2int c22)
in
let cmpChar =
  lam c11.
    lam c21.
      subi (char2int c11) (char2int c21)
in
let eqString = lam s110.
    lam s210.
      eqSeq eqc s110 s210
in
let cmpString: [Char] -> [Char] -> Int = seqCmp cmpChar in
recursive
  let string2int_rechelper =
    lam s44.
      lam acc17.
        match
          null s44
        with
          true
        then
          acc17
        else
          let fsd = subi (char2int (head s44)) (char2int '0') in
          string2int_rechelper (tail s44) (addi (muli 10 acc17) fsd)
in
let string2int =
  lam s43.
    match
      s43
    with
      ""
    then
      0
    else match
      eqChar '-' (head s43)
    with
      true
    then
      negi (string2int_rechelper (tail s43) 0)
    else
      string2int_rechelper s43 0
in
let digit2char = lam d10.
    int2char (addi d10 (char2int '0'))
in
recursive
  let int2string_rechelper =
    lam n2.
      lam acc16.
        match
          lti n2 10
        with
          true
        then
          cons (digit2char n2) acc16
        else
          int2string_rechelper (divi n2 10) (cons (digit2char (modi n2 10)) acc16)
in
let int2string =
  lam n1.
    match
      lti n1 0
    with
      true
    then
      cons '-' (int2string_rechelper (negi n1) "")
    else
      int2string_rechelper n1 ""
in
let strJoin: [Char] -> [[Char]] -> [Char] = seqJoin in
type AVLTreeImpl_AuxTree a7 a8
in
type AVLTreeImpl_AVL a9 a10
in
con AVLTreeImpl_Node: all k. all v. {h: Int, l: AVLTreeImpl_AVL k v, r: AVLTreeImpl_AVL k v, key: k, value: v} -> AVLTreeImpl_AVL k v in
con AVLTreeImpl_Leaf: all k1. all v1. () -> AVLTreeImpl_AVL k1 v1 in
recursive
  let vAVLTreeImpl_avlJoin: all k8. all v9. k8 -> v9 -> AVLTreeImpl_AVL k8 v9 -> AVLTreeImpl_AVL k8 v9 -> AVLTreeImpl_AVL k8 v9 =
    lam k9: k8.
      lam v10: v9.
        lam l: AVLTreeImpl_AVL k8 v9.
          lam __sem_target: AVLTreeImpl_AVL k8 v9.
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
                vAVLTreeImpl_avlJoinRight k9 v10 r l
              else match
                gti rh (addi lh 1)
              with
                true
              then
                vAVLTreeImpl_avlJoinLeft k9 v10 l r
              else
                vAVLTreeImpl_avlCreate k9 v10 l r
  let vAVLTreeImpl_avlEmpty: all k10. all v11. () -> AVLTreeImpl_AVL k10 v11 =
    lam __sem_target1: ().
      match
        __sem_target1
      with
        {}
      in
      AVLTreeImpl_Leaf
          {}
  let vAVLTreeImpl_avlCreate: all k11. all v12. k11 -> v12 -> AVLTreeImpl_AVL k11 v12 -> AVLTreeImpl_AVL k11 v12 -> AVLTreeImpl_AVL k11 v12 =
    lam k12: k11.
      lam v13: v12.
        lam l1: AVLTreeImpl_AVL k11 v12.
          lam __sem_target2: AVLTreeImpl_AVL k11 v12.
            match
              __sem_target2
            with
              r1
            in
            let lh1 = vAVLTreeImpl_avlHeight l1 in
              let rh1 = vAVLTreeImpl_avlHeight r1 in
              let h =
                addi
                  (match
                     geqi lh1 rh1
                   with
                     true
                   then
                     lh1
                   else
                     rh1)
                  1
              in
              AVLTreeImpl_Node
                { r = r1, key = k12, value = v13, h = h, l = l1 }
  let vAVLTreeImpl_avlHeight: all k13. all v14. AVLTreeImpl_AVL k13 v14 -> Int =
    lam __sem_target3: AVLTreeImpl_AVL k13 v14.
      match
        __sem_target3
      with
        AVLTreeImpl_Leaf _
      then
        0
      else match
        __sem_target3
      with
        AVLTreeImpl_Node {h = h4}
      in
      h4
  let vAVLTreeImpl_avlInsert: all k14. all v15. (k14 -> k14 -> Int) -> k14 -> v15 -> AVLTreeImpl_AVL k14 v15 -> AVLTreeImpl_AVL k14 v15 =
    lam cmp1: k14 -> k14 -> Int.
      lam k15: k14.
        lam v16: v15.
          lam __sem_target4: AVLTreeImpl_AVL k14 v15.
            match
              __sem_target4
            with
              AVLTreeImpl_Leaf _
            then
              AVLTreeImpl_Node
                { r = AVLTreeImpl_Leaf
                      {},
                  key = k15,
                  value = v16,
                  h = 1,
                  l = AVLTreeImpl_Leaf
                      {} }
            else match
              __sem_target4
            with
              AVLTreeImpl_Node t8
            in
            let d8 = cmp1 k15 t8.key in
              match
                lti d8 0
              with
                true
              then
                vAVLTreeImpl_avlJoin
                  t8.key
                  t8.value
                  (vAVLTreeImpl_avlInsert cmp1 k15 v16 t8.l)
                  t8.r
              else match
                gti d8 0
              with
                true
              then
                vAVLTreeImpl_avlJoin
                  t8.key
                  t8.value
                  t8.l
                  (vAVLTreeImpl_avlInsert cmp1 k15 v16 t8.r)
              else
                AVLTreeImpl_Node
                  { t8 with value = v16 }
  let vAVLTreeImpl_avlLookup: all k16. all v17. (k16 -> k16 -> Int) -> k16 -> AVLTreeImpl_AVL k16 v17 -> Option v17 =
    lam cmp2: k16 -> k16 -> Int.
      lam k17: k16.
        lam __sem_target5: AVLTreeImpl_AVL k16 v17.
          match
            __sem_target5
          with
            AVLTreeImpl_Leaf _
          then
            None
              {}
          else match
            __sem_target5
          with
            AVLTreeImpl_Node t9
          in
          let d9 = cmp2 k17 t9.key in
            match
              lti d9 0
            with
              true
            then
              vAVLTreeImpl_avlLookup cmp2 k17 t9.l
            else match
              gti d9 0
            with
              true
            then
              vAVLTreeImpl_avlLookup cmp2 k17 t9.r
            else
              Some
                t9.value
  let vAVLTreeImpl_avlJoinLeft: all k18. all v18. k18 -> v18 -> AVLTreeImpl_AVL k18 v18 -> AVLTreeImpl_AVL k18 v18 -> AVLTreeImpl_AVL k18 v18 =
    lam k19: k18.
      lam v19: v18.
        lam l2: AVLTreeImpl_AVL k18 v18.
          lam __sem_target6: AVLTreeImpl_AVL k18 v18.
            match
              __sem_target6
            with
              AVLTreeImpl_Node tr
            then
              match
                leqi
                  (vAVLTreeImpl_avlHeight tr.l)
                  (addi (vAVLTreeImpl_avlHeight l2) 1)
              with
                true
              then
                let t10 = vAVLTreeImpl_avlCreate k19 v19 l2 tr.l in
                match
                  leqi
                    (vAVLTreeImpl_avlHeight t10)
                    (addi (vAVLTreeImpl_avlHeight tr.r) 1)
                with
                  true
                then
                  vAVLTreeImpl_avlCreate tr.key tr.value t10 tr.r
                else
                  vAVLTreeImpl_avlRotateRight
                    tr.key
                    tr.value
                    tr.r
                    (vAVLTreeImpl_avlRotateLeft k19 v19 l2 tr.l)
              else
                let tx = vAVLTreeImpl_avlJoinLeft k19 v19 l2 tr.l in
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
              __sem_target6
            with
              AVLTreeImpl_Leaf _
            in
            error "avlJoinLeft: empty tree"
  let vAVLTreeImpl_avlJoinRight: all k20. all v20. k20 -> v20 -> AVLTreeImpl_AVL k20 v20 -> AVLTreeImpl_AVL k20 v20 -> AVLTreeImpl_AVL k20 v20 =
    lam k21: k20.
      lam v21: v20.
        lam r2: AVLTreeImpl_AVL k20 v20.
          lam __sem_target7: AVLTreeImpl_AVL k20 v20.
            match
              __sem_target7
            with
              AVLTreeImpl_Node tl
            then
              match
                leqi
                  (vAVLTreeImpl_avlHeight tl.r)
                  (addi (vAVLTreeImpl_avlHeight r2) 1)
              with
                true
              then
                let t11 = vAVLTreeImpl_avlCreate k21 v21 tl.r r2 in
                match
                  leqi
                    (vAVLTreeImpl_avlHeight t11)
                    (addi (vAVLTreeImpl_avlHeight tl.l) 1)
                with
                  true
                then
                  vAVLTreeImpl_avlCreate tl.key tl.value tl.l t11
                else
                  vAVLTreeImpl_avlRotateLeft
                    tl.key
                    tl.value
                    tl.l
                    (vAVLTreeImpl_avlRotateRight k21 v21 r2 tl.r)
              else
                let tx1 = vAVLTreeImpl_avlJoinRight k21 v21 r2 tl.r in
                match
                  leqi
                    (vAVLTreeImpl_avlHeight tx1)
                    (addi (vAVLTreeImpl_avlHeight tl.l) 1)
                with
                  true
                then
                  vAVLTreeImpl_avlCreate tl.key tl.value tl.l tx1
                else
                  vAVLTreeImpl_avlRotateLeft tl.key tl.value tl.l tx1
            else match
              __sem_target7
            with
              AVLTreeImpl_Leaf _
            in
            error "avlJoinRight: empty tree"
  let vAVLTreeImpl_avlRotateLeft: all k22. all v22. k22 -> v22 -> AVLTreeImpl_AVL k22 v22 -> AVLTreeImpl_AVL k22 v22 -> AVLTreeImpl_AVL k22 v22 =
    lam k23: k22.
      lam v23: v22.
        lam l3: AVLTreeImpl_AVL k22 v22.
          lam __sem_target8: AVLTreeImpl_AVL k22 v22.
            match
              __sem_target8
            with
              AVLTreeImpl_Node (rt & {r = rr, l = rl})
            then
              vAVLTreeImpl_avlCreate rt.key rt.value (vAVLTreeImpl_avlCreate k23 v23 l3 rl) rr
            else match
              __sem_target8
            with
              AVLTreeImpl_Leaf _
            in
            error "avlRotateLeft: empty tree"
  let vAVLTreeImpl_avlRotateRight: all k24. all v24. k24 -> v24 -> AVLTreeImpl_AVL k24 v24 -> AVLTreeImpl_AVL k24 v24 -> AVLTreeImpl_AVL k24 v24 =
    lam k25: k24.
      lam v25: v24.
        lam r3: AVLTreeImpl_AVL k24 v24.
          lam __sem_target9: AVLTreeImpl_AVL k24 v24.
            match
              __sem_target9
            with
              AVLTreeImpl_Node (lt2 & {r = lr, l = ll})
            then
              vAVLTreeImpl_avlCreate lt2.key lt2.value ll (vAVLTreeImpl_avlCreate k25 v25 lr r3)
            else match
              __sem_target9
            with
              AVLTreeImpl_Leaf _
            in
            error "avlRotateRight: empty tree"
in
type Map k2 v2 =
  {cmp: k2 -> k2 -> Int, root: AVLTreeImpl_AVL k2 v2}
in
let mapEmpty: all k7. all v8. (k7 -> k7 -> Int) -> Map k7 v8 =
  lam cmp: k7 -> k7 -> Int.
    { cmp = cmp, root = vAVLTreeImpl_avlEmpty {} }
in
let mapLookup: all k5. all v7. k5 -> Map k5 v7 -> Option v7 =
  lam k6: k5.
    lam m1: Map k5 v7.
      vAVLTreeImpl_avlLookup m1.cmp k6 m1.root
in
let mapInsert: all k3. all v5. k3 -> v5 -> Map k3 v5 -> Map k3 v5 =
  lam k4: k3.
    lam v6: v5.
      lam m: Map k3 v5.
        { m with root = vAVLTreeImpl_avlInsert m.cmp k4 v6 m.root }
in
external externalExp : Float -> Float
in
let exp = lam x1: Float.
    externalExp x1 in
external externalSqrt : Float -> Float
in
let sqrt: Float -> Float = lam x: Float.
    externalSqrt x in
let maxi =
  lam a16.
    lam b5.
      match
        gti a16 b5
      with
        true
      then
        a16
      else
        b5
in
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
    lam pos2: Int.
      lam msg1: [Char].
        join
          [ "Error at position ",
            int2string pos2,
            ": ",
            msg1 ]
  let _jsonError: Int -> [Char] -> Either (JsonValue, [Char], Int) [Char] =
    lam pos3: Int.
      lam msg2: [Char].
        Right
          (_jsonErrorString pos3 msg2)
  let _jsonEatWhitespace: [Char] -> Int -> ([Char], Int) =
    lam s17: [Char].
      lam pos4: Int.
        match
          s17
        with
          [ ' ' | '\n' | '\r' | '\t' ] ++ ws
        then
          _jsonEatWhitespace ws (addi pos4 1)
        else
          (s17, pos4)
  let _jsonEatInt: [Char] -> [Char] -> Int -> ([Char], [Char], Int) =
    lam acc8: [Char].
      lam s18: [Char].
        lam pos5: Int.
          match
            s18
          with
            [ c1 ] ++ ws1
          then
            match
              and (geqChar c1 '0') (leqChar c1 '9')
            with
              true
            then
              _jsonEatInt (snoc acc8 c1) ws1 (addi pos5 1)
            else
              (acc8, s18, pos5)
          else
            (acc8, s18, pos5)
  let _jsonParse: ([Char] -> [Char] -> Int) -> [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam cmpString7.
      lam s19: [Char].
        lam pos6: Int.
          match
            _jsonEatWhitespace s19 pos6
          with
            (s20, pos7)
          in
          let #var"X2" = s20 in
            match
              #var"X2"
            with
              "{" ++ ws2
            then
              _jsonParseObject cmpString7 ws2 (addi pos7 1)
            else match
              #var"X2"
            with
              "[" ++ ws3
            then
              _jsonParseArray cmpString7 ws3 (addi pos7 1)
            else match
              #var"X2"
            with
              "\"" ++ ws4
            then
              _jsonParseString "" ws4 (addi pos7 1)
            else match
              #var"X2"
            with
              "-" ++ ws5
            then
              _jsonParseNegativeNumber ws5 (addi pos7 1)
            else match
              #var"X2"
            with
              [ '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ] ++ _
            then
              _jsonParseNumber s20 pos7
            else match
              #var"X2"
            with
              "true" ++ ws6
            then
              Left
                (JsonBool
                  true, ws6, addi pos7 4)
            else match
              #var"X2"
            with
              "false" ++ ws7
            then
              Left
                (JsonBool
                  false, ws7, addi pos7 5)
            else match
              #var"X2"
            with
              "null" ++ ws8
            then
              Left
                (JsonNull
                  {}, ws8, addi pos7 4)
            else match
              #var"X2"
            with
              _
            in
            _jsonError pos7 "Invalid start to a JSON value."
  let _jsonParseObject: ([Char] -> [Char] -> Int) -> [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam cmpString8.
      lam s21: [Char].
        lam pos8: Int.
          match
            _jsonEatWhitespace s21 pos8
          with
            (s22, pos9)
          in
          let acc9 = mapEmpty cmpString8 in
            match
              s22
            with
              "}" ++ ws9
            then
              Left
                (JsonObject
                  acc9, ws9, addi pos9 1)
            else
              _jsonParseObjectContents cmpString8 acc9 s22 pos9
  let _jsonParseObjectContents: ([Char] -> [Char] -> Int) -> Map [Char] JsonValue -> [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam cmpString9.
      lam acc10: Map [Char] JsonValue.
        lam s23: [Char].
          lam pos10: Int.
            match
              _jsonEatWhitespace s23 pos10
            with
              (s24, pos11)
            in
            let #var"X3" = _jsonParse cmpString9 s24 pos11 in
              match
                #var"X3"
              with
                Left (JsonString key, s25, pos12)
              then
                match
                  _jsonEatWhitespace s25 pos12
                with
                  (s26, pos13)
                in
                match
                    s26
                  with
                    ":" ++ ws10
                  then
                    match
                      _jsonEatWhitespace ws10 (addi pos13 1)
                    with
                      (s27, pos14)
                    in
                    let #var"X4" = _jsonParse cmpString9 s27 pos14 in
                      match
                        #var"X4"
                      with
                        Left (value4, s28, pos15)
                      then
                        let acc11 = mapInsert key value4 acc10 in
                        match
                          _jsonEatWhitespace s28 pos15
                        with
                          (s29, pos16)
                        in
                        match
                            s29
                          with
                            "," ++ ws11
                          then
                            _jsonParseObjectContents cmpString9 acc11 ws11 (addi pos16 1)
                          else match
                            s29
                          with
                            "}" ++ ws12
                          then
                            Left
                              (JsonObject
                                acc11, ws12, addi pos16 1)
                          else
                            _jsonError pos16 "Expected comma or closing bracket for JSON object."
                      else match
                        #var"X4"
                      with
                        Right err1
                      in
                      Right
                          err1
                  else
                    _jsonError pos13 "Expected colon after property key."
              else match
                #var"X3"
              with
                Left _
              then
                _jsonError pos11 "Expected string as property key."
              else match
                #var"X3"
              with
                Right err2
              in
              Right
                  err2
  let _jsonParseArray: ([Char] -> [Char] -> Int) -> [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam cmpString10.
      lam s30: [Char].
        lam pos17: Int.
          match
            _jsonEatWhitespace s30 pos17
          with
            (s31, pos18)
          in
          match
              s31
            with
              "]" ++ ws13
            then
              Left
                (JsonArray
                  "", ws13, addi pos18 1)
            else
              _jsonParseArrayContents cmpString10 "" s31 pos18
  let _jsonParseArrayContents: ([Char] -> [Char] -> Int) -> [JsonValue] -> [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam cmpString11.
      lam acc12: [JsonValue].
        lam s32: [Char].
          lam pos19: Int.
            match
              _jsonEatWhitespace s32 pos19
            with
              (s33, pos20)
            in
            let result2 = _jsonParse cmpString11 s33 pos20 in
              let #var"X5" = result2 in
              match
                #var"X5"
              with
                Left (value5, s34, pos21)
              then
                let acc13 = snoc acc12 value5 in
                match
                  _jsonEatWhitespace s34 pos21
                with
                  (s35, pos22)
                in
                match
                    s35
                  with
                    "," ++ ws14
                  then
                    _jsonParseArrayContents cmpString11 acc13 ws14 (addi pos22 1)
                  else match
                    s35
                  with
                    "]" ++ ws15
                  then
                    Left
                      (JsonArray
                        acc13, ws15, addi pos22 1)
                  else
                    _jsonError pos22 "Expected comma or closing bracket of JSON array."
              else match
                #var"X5"
              with
                Right err3
              in
              Right
                  err3
  let hex2int: Char -> Option Int =
    lam hc: Char.
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
  let t7: Option Int -> Char -> Option Int =
    lam acc14: Option Int.
      lam hc1.
        match
          acc14
        with
          Some accv
        then
          match
            hex2int hc1
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
            {}
  let _jsonParseString: [Char] -> [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam acc15: [Char].
      lam s36: [Char].
        lam pos23: Int.
          match
            s36
          with
            [ '\\',
              c2 ] ++ ws16
          then
            let #var"X6" = c2 in
            match
              #var"X6"
            with
              '\"'
            then
              _jsonParseString (snoc acc15 '\"') ws16 (addi pos23 2)
            else match
              #var"X6"
            with
              '\\'
            then
              _jsonParseString (snoc acc15 '\\') ws16 (addi pos23 2)
            else match
              #var"X6"
            with
              '/'
            then
              _jsonParseString (snoc acc15 '/') ws16 (addi pos23 2)
            else match
              #var"X6"
            with
              'b'
            then
              _jsonParseString (snoc acc15 (int2char 8)) ws16 (addi pos23 2)
            else match
              #var"X6"
            with
              'f'
            then
              _jsonParseString (snoc acc15 (int2char 12)) ws16 (addi pos23 2)
            else match
              #var"X6"
            with
              'n'
            then
              _jsonParseString (snoc acc15 '\n') ws16 (addi pos23 2)
            else match
              #var"X6"
            with
              'r'
            then
              _jsonParseString (snoc acc15 '\r') ws16 (addi pos23 2)
            else match
              #var"X6"
            with
              't'
            then
              _jsonParseString (snoc acc15 '\t') ws16 (addi pos23 2)
            else match
              #var"X6"
            with
              'u'
            then
              match
                ws16
              with
                [ h3,
                  h2,
                  h1,
                  h0 ] ++ ws17
              then
                let conv =
                  foldl
                    t7
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
                  Some v4
                then
                  _jsonParseString (snoc acc15 (int2char v4)) ws17 (addi pos23 6)
                else
                  _jsonError (addi pos23 2) "Expected 4 hexadecimal characters"
              else
                _jsonError (addi pos23 2) "Expected 4 hexadecimal characters"
            else match
              #var"X6"
            with
              _
            in
            _jsonError
                (addi pos23 1)
                (join
                   [ "Invalid escape char \'",
                     [ c2 ],
                     "\' (decimal value: ",
                     int2string (char2int c2),
                     ")" ])
          else match
            s36
          with
            "\"" ++ ws18
          then
            Left
              (JsonString
                acc15, ws18, addi pos23 1)
          else match
            s36
          with
            [ c3 ] ++ ws19
          then
            _jsonParseString (snoc acc15 c3) ws19 (addi pos23 1)
          else
            _jsonError pos23 "Non-terminated string."
  let _jsonParseNegativeNumber: [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam s37: [Char].
      lam pos24: Int.
        let num = _jsonParseNumber s37 pos24 in
        let #var"X7" = num in
        match
          #var"X7"
        with
          Left (JsonInt i3, s38, pos25)
        then
          Left
            (JsonInt
              (negi i3), s38, pos25)
        else match
          #var"X7"
        with
          Left (JsonFloat f1, s39, pos26)
        then
          Left
            (JsonFloat
              (negf f1), s39, pos26)
        else match
          #var"X7"
        with
          Left _
        then
          _jsonError pos24 "Internal error, did not get a number back."
        else match
          #var"X7"
        with
          Right err4
        in
        Right
            err4
  let _jsonParseNumber: [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam s40: [Char].
      lam pos27: Int.
        match
          s40
        with
          "0" ++ ws20
        then
          _jsonParseNumberDecimals "0" ws20 (addi pos27 1)
        else match
          s40
        with
          [ c4 & ('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') ] ++ ws21
        then
          match
            _jsonEatInt [ c4 ] ws21 (addi pos27 1)
          with
            (intStr, ws22, pos28)
          in
          _jsonParseNumberDecimals intStr ws22 pos28
        else
          _jsonError pos27 "Expected a number."
  let _jsonParseNumberDecimals: [Char] -> [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam intStr1: [Char].
      lam s41: [Char].
        lam pos29: Int.
          match
            s41
          with
            "." ++ ws23
          then
            match
              _jsonEatInt "" ws23 (addi pos29 1)
            with
              (decimals, ws24, pos30)
            in
            match
                null decimals
              with
                true
              then
                _jsonError pos30 "Expected decimals after dot in a number."
              else
                _jsonParseNumberExponent intStr1 decimals ws24 pos30
          else match
            s41
          with
            [ 'e' | 'E' ] ++ _
          then
            _jsonParseNumberExponent intStr1 "0" s41 pos29
          else
            Left
              (JsonInt
                (string2int intStr1), s41, pos29)
  let _jsonParseNumberExponent: [Char] -> [Char] -> [Char] -> Int -> Either (JsonValue, [Char], Int) [Char] =
    lam intStr2: [Char].
      lam decimals1: [Char].
        lam s42: [Char].
          lam pos31: Int.
            match
              s42
            with
              [ 'e' | 'E' ] ++ ws25
            then
              match
                ws25
              with
                [ c5 & ('+' | '-') ] ++ ws26
              then
                match
                  _jsonEatInt "" ws26 (addi pos31 2)
                with
                  (exponent, ws27, pos32)
                in
                match
                    null exponent
                  with
                    true
                  then
                    _jsonError pos32 "Expected exponent digits."
                  else
                    let floatStr =
                      join
                        [ intStr2,
                          ".",
                          decimals1,
                          "e",
                          [ c5 ],
                          exponent ]
                    in
                    Left
                      (JsonFloat
                        (string2float floatStr), ws27, pos32)
              else match
                _jsonEatInt "" ws25 (addi pos31 1)
              with
                (exponent1, ws28, pos33)
              in
              match
                  null exponent1
                with
                  true
                then
                  _jsonError pos33 "Expected exponent digits."
                else
                  let floatStr1 =
                    join
                      [ intStr2,
                        ".",
                        decimals1,
                        "e",
                        exponent1 ]
                  in
                  Left
                    (JsonFloat
                      (string2float floatStr1), ws28, pos33)
            else
              let floatStr2 =
                join
                  [ intStr2,
                    ".",
                    decimals1 ]
              in
              Left
                (JsonFloat
                  (string2float floatStr2), s42, pos31)
in
let jsonParse: ([Char] -> [Char] -> Int) -> [Char] -> Either JsonValue [Char] =
  lam cmpString6.
    lam s14: [Char].
      let result1 = _jsonParse cmpString6 s14 0 in
      let #var"X1" = result1 in
      match
        #var"X1"
      with
        Left (value3, s15, pos)
      then
        match
          _jsonEatWhitespace s15 pos
        with
          (s16, pos1)
        in
        match
            null s16
          with
            true
          then
            Left
              value3
          else
            Right
              (_jsonErrorString pos1 "Trailing JSON content.")
      else match
        #var"X1"
      with
        Right err
      in
      Right
          err
in
let jsonParseExn: ([Char] -> [Char] -> Int) -> [Char] -> JsonValue =
  lam cmpString5.
    lam s13: [Char].
      let result = jsonParse cmpString5 s13 in
      let #var"X" = result in
      match
        #var"X"
      with
        Left value2
      then
        value2
      else match
        #var"X"
      with
        Right errmsg
      in
      error errmsg
in
type WriteChannel
in
type ReadChannel
in
external fileExists! : [Char] -> Bool
in
type Signal =
  Int
in
external setSigintHandler : (Signal -> ()) -> ()
in
type Timespec =
  (Int, Int)
in
external getMonotonicTime : () -> Timespec
in
external getWallClockTime : () -> Timespec
in
external getProcessCpuTime : () -> Timespec
in
external clockNanosleep : Timespec -> ()
in
external rtpplSetMaxPriority : () -> Int
in
external rtpplSetPriority : Int -> Int
in
external rtpplOpenFileDescriptor : [Char] -> Int -> Int
in
external rtpplCloseFileDescriptor : Int -> ()
in
type Opaque
in
external rtpplReadFloat : Int -> [(Timespec, Float)]
in
external rtpplWriteFloatRecord : Int -> Int -> (Timespec, Opaque) -> ()
in
let nanosPerSec = 1000000000 in
let nanosToTimespec: Int -> Int -> Timespec =
  lam nanosPerSec14.
    lam nanos: Int.
      let s12 = divi nanos nanosPerSec14 in
      let ns3 = modi nanos nanosPerSec14 in
      (s12, ns3)
in
let timespecToNanos: Int -> Timespec -> Int =
  lam nanosPerSec13.
    lam ts: Timespec.
      match
        ts
      with
        (s11, ns2)
      in
      addi (muli s11 nanosPerSec13) ns2
in
let addTimespec: Int -> Timespec -> Timespec -> Timespec =
  lam nanosPerSec12.
    lam lhs2: Timespec.
      lam rhs2: Timespec.
        match
          (lhs2, rhs2)
        with
          ((ls2, lns2), (rs2, rns2))
        in
        let s10 = addi ls2 rs2 in
          let ns1 = addi lns2 rns2 in
          match
            geqi ns1 nanosPerSec12
          with
            true
          then
            (addi s10 1, subi ns1 nanosPerSec12)
          else
            (s10, ns1)
in
let diffTimespec: Int -> Timespec -> Timespec -> Timespec =
  lam nanosPerSec11.
    lam lhs1: Timespec.
      lam rhs1: Timespec.
        match
          (lhs1, rhs1)
        with
          ((ls1, lns1), (rs1, rns1))
        in
        let s9 = subi ls1 rs1 in
          let ns = subi lns1 rns1 in
          match
            lti ns 0
          with
            true
          then
            (subi s9 1, addi ns nanosPerSec11)
          else
            (s9, ns)
in
let cmpTimespec: Timespec -> Timespec -> Int =
  lam lhs: Timespec.
    lam rhs: Timespec.
      match
        (lhs, rhs)
      with
        ((ls, lns), (rs, rns))
      in
      match
          gti ls rs
        with
          true
        then
          1
        else match
          lti ls rs
        with
          true
        then
          negi 1
        else match
          gti lns rns
        with
          true
        then
          1
        else match
          lti lns rns
        with
          true
        then
          negi 1
        else
          0
in
let monoLogicalTime: Ref Timespec = ref (0, 0) in
let wallLogicalTime: Ref Timespec = ref (0, 0) in
let cpuExecutionTime: Ref Timespec = ref (0, 0) in
let particleCount: Ref Int = ref 0 in
let taskBudget: Ref Int = ref 0 in
let taskExecTimes: Ref [Int] = ref "" in
let slowdown: Ref Int = ref 1 in
let delayBy: Int -> Ref Timespec -> Ref Timespec -> Ref Int -> Int -> Int =
  lam nanosPerSec10.
    lam monoLogicalTime3.
      lam wallLogicalTime10.
        lam slowdown7.
          lam delayNs1: Int.
            let oldPriority = rtpplSetMaxPriority {} in
            let logicalIntervalTime = nanosToTimespec nanosPerSec10 delayNs1
            in
            let adjustedDelay = muli delayNs1 (deref slowdown7) in
            let intervalTime = nanosToTimespec nanosPerSec10 adjustedDelay
            in
            let endTime = getMonotonicTime {} in
            let elapsedTime = diffTimespec nanosPerSec10 endTime (deref monoLogicalTime3)
            in
            let waitTime =
              addTimespec nanosPerSec10 (deref monoLogicalTime3) intervalTime
            in
            let overrun1 =
              let c = cmpTimespec intervalTime elapsedTime in
              match
                gti c 0
              with
                true
              then
                let #var"42" = clockNanosleep waitTime in
                0
              else match
                lti c 0
              with
                true
              then
                let elapsedTime1 = diffTimespec nanosPerSec10 endTime waitTime
                in
                timespecToNanos nanosPerSec10 elapsedTime1
              else
                0
            in
            let #var"39" = modref monoLogicalTime3 waitTime in
            let #var"40" =
              modref
                wallLogicalTime10
                (addTimespec nanosPerSec10 (deref wallLogicalTime10) logicalIntervalTime)
            in
            let #var"41" = rtpplSetPriority oldPriority in
            overrun1
in
type TSV a11 =
  (Timespec, a11)
in
let timestamp: all a15. Int -> Ref Timespec -> TSV a15 -> Int =
  lam nanosPerSec9.
    lam wallLogicalTime9.
      lam tsv2: TSV a15.
        let lt1 = deref wallLogicalTime9 in
        timespecToNanos nanosPerSec9 (diffTimespec nanosPerSec9 tsv2.0 lt1)
in
let value: all a14. TSV a14 -> a14 = lam tsv1: TSV a14.
    tsv1.1
in
let tsv: all a13. Int -> Ref Timespec -> Int -> a13 -> TSV a13 =
  lam nanosPerSec8.
    lam wallLogicalTime8.
      lam offset: Int.
        lam value1: a13.
          let lt = deref wallLogicalTime8 in
          (addTimespec nanosPerSec8 lt (nanosToTimespec nanosPerSec8 offset), value1)
in
let writeCollectionMessage =
  lam nanosPerSec7.
    lam cpuExecutionTime3.
      lam taskExecTimes5.
        lam #var"37".
          let cpu = getProcessCpuTime {} in
          let execTime =
            timespecToNanos
              nanosPerSec7
              (diffTimespec nanosPerSec7 cpu (deref cpuExecutionTime3))
          in
          let #var"38" = modref cpuExecutionTime3 cpu in
          modref taskExecTimes5 (snoc (deref taskExecTimes5) execTime)
in
let sdelay =
  lam nanosPerSec6.
    lam monoLogicalTime2.
      lam wallLogicalTime7.
        lam cpuExecutionTime2.
          lam taskExecTimes4.
            lam slowdown6.
              lam flushOutputs1: () -> ().
                lam updateInputs1: () -> ().
                  lam delayNs: Int.
                    let #var"34" = flushOutputs1 {} in
                    let #var"35" =
                      writeCollectionMessage nanosPerSec6 cpuExecutionTime2 taskExecTimes4 {}
                    in
                    let overrun =
                      delayBy
                        nanosPerSec6
                        monoLogicalTime2
                        wallLogicalTime7
                        slowdown6
                        delayNs
                    in
                    let #var"36" = updateInputs1 {} in
                    overrun
in
let rtpplFixedInferRunner = lam pc.
    lam inferModel1.
      inferModel1 pc
in
let openFileDescriptor: [Char] -> Int -> Int =
  lam file: [Char].
    lam bufsz: Int.
      rtpplOpenFileDescriptor file bufsz
in
let closeFileDescriptor: Int -> () = lam fd2: Int.
    rtpplCloseFileDescriptor fd2
in
let t: Int -> Int -> (Timespec, Opaque) -> () =
  lam fd1.
    lam nfields1.
      lam msg.
        rtpplWriteFloatRecord fd1 nfields1 msg
in
let rtpplWriteFloatRecords =
  lam fd.
    lam nfields.
      lam msgs.
        iter (t fd nfields) msgs
in
let storeCollectedResults =
  lam strJoin3.
    lam taskBudget4.
      lam taskExecTimes3.
        lam slowdown5.
          lam taskId7.
            let collectionFile = concat taskId7 ".collect" in
            let execTimes = deref taskExecTimes3 in
            let wcet = foldl maxi 0 execTimes in
            let overran =
              let b4 = deref taskBudget4 in
              match
                lti b4 0
              with
                true
              then
                0
              else match
                gti wcet (muli (deref taskBudget4) (deref slowdown5))
              with
                true
              then
                1
              else
                0
            in
            let data = map int2string (snoc execTimes overran) in
            writeFile collectionFile (strJoin3 "\n" data)
in
let getJsonValueExn =
  lam obj3.
    lam id2.
      match
        obj3
      with
        JsonObject vals1
      then
        match
          mapLookup id2 vals1
        with
          Some v3
        then
          v3
        else
          error (concat "Could not find JSON field " id2)
      else
        error
          "Attempted to access field of JSON value of non-object type"
in
let getJsonStringExn =
  lam obj2.
    lam id1.
      match
        getJsonValueExn obj2 id1
      with
        JsonString s8
      then
        s8
      else
        error
          (join
             [ "Expected field ",
               id1,
               " to be a string value" ])
in
let getJsonIntExn =
  lam obj1.
    lam id.
      match
        getJsonValueExn obj1 id
      with
        JsonInt n
      then
        n
      else
        error
          (join
             [ "Expected field ",
               id,
               " to be an integer value" ])
in
let isTaskObj =
  lam taskId6.
    lam taskObj.
      eqString (getJsonStringExn taskObj "id") taskId6
in
let findTask =
  lam obj.
    lam taskId5.
      match
        getJsonValueExn obj "tasks"
      with
        JsonArray vals
      then
        match
          find (isTaskObj taskId5) vals
        with
          Some taskValue
        then
          taskValue
        else
          error (concat "Failed to find task " taskId5)
      else
        error "Could not find tasks list in JSON configuration"
in
let readJsonConfig =
  lam cmpString4.
    lam configFile1.
      lam taskId4.
        let config = jsonParseExn cmpString4 (readFile configFile1) in
        let jsonTask = findTask config taskId4 in
        let numParticles = getJsonIntExn jsonTask "particles" in
        let budget1 = getJsonIntExn jsonTask "budget" in
        let slowdown4 = getJsonIntExn (getJsonValueExn config "config") "slowdown"
        in
        (numParticles, budget1, slowdown4)
in
let rtpplReadConfigurationFile =
  lam cmpString3.
    lam taskId3.
      let configFile = "system.json" in
      match
        fileExists configFile
      with
        true
      then
        readJsonConfig cmpString3 configFile taskId3
      else
        error
          (join
             [ "Failed to read system configuration in \'",
               configFile,
               "\'" ])
in
let rtpplLoadConfiguration =
  lam cmpString2.
    lam particleCount2.
      lam taskBudget3.
        lam slowdown3.
          lam taskId2.
            match
              rtpplReadConfigurationFile cmpString2 taskId2
            with
              (nparticles, budget, slowd)
            in
            let #var"32" = modref particleCount2 nparticles in
              let #var"33" = modref taskBudget3 budget in
              modref slowdown3 slowd
in
let t1: ([Char] -> [[Char]] -> [Char]) -> Ref Int -> Ref [Int] -> Ref Int -> (() -> ()) -> [Char] -> Signal -> () =
  lam strJoin2.
    lam taskBudget2.
      lam taskExecTimes2.
        lam slowdown2.
          lam closeFileDescriptors2.
            lam taskId1.
              lam #var"29".
                let #var"30" = closeFileDescriptors2 {} in
                let #var"31" =
                  storeCollectedResults strJoin2 taskBudget2 taskExecTimes2 slowdown2 taskId1
                in
                exit 0
in
let rtpplRuntimeInit: all a12. ([Char] -> [Char] -> Int) -> ([Char] -> [[Char]] -> [Char]) -> Ref Timespec -> Ref Timespec -> Ref Timespec -> Ref Int -> Ref Int -> Ref [Int] -> Ref Int -> (() -> ()) -> (() -> ()) -> [Char] -> (() -> a12) -> () =
  lam cmpString1.
    lam strJoin1.
      lam monoLogicalTime1.
        lam wallLogicalTime6.
          lam cpuExecutionTime1.
            lam particleCount1.
              lam taskBudget1.
                lam taskExecTimes1.
                  lam slowdown1.
                    lam updateInputSequences: () -> ().
                      lam closeFileDescriptors1: () -> ().
                        lam taskId: [Char].
                          lam cont: () -> a12.
                            let #var"22" =
                              setSigintHandler
                                (t1
                                   strJoin1
                                   taskBudget1
                                   taskExecTimes1
                                   slowdown1
                                   closeFileDescriptors1
                                   taskId)
                            in
                            let #var"23" =
                              rtpplLoadConfiguration cmpString1 particleCount1 taskBudget1 slowdown1 taskId
                            in
                            let #var"24" = modref monoLogicalTime1 (getMonotonicTime {}) in
                            let #var"25" = modref wallLogicalTime6 (getWallClockTime {}) in
                            let #var"26" = modref cpuExecutionTime1 (getProcessCpuTime {})
                            in
                            let #var"27" = updateInputSequences {} in
                            let #var"28" = cont {} in
                            {}
in
let fileDescriptors =
  { bias = openFileDescriptor "bias" 4194304,
    in1 = openFileDescriptor "cf-in1" 4194304 }
in
let closeFileDescriptors =
  lam #var"21".
    let close_bias = closeFileDescriptor fileDescriptors.bias in
    let close_in1 = closeFileDescriptor fileDescriptors.in1 in
    {}
in
let inputSeqs = ref { in1 = "" } in
let outputSeqs = ref { out1 = "" } in
let updateInputs =
  lam #var"20".
    modref inputSeqs { in1 = rtpplReadFloat fileDescriptors.in1 }
in
let flushOutputs =
  lam #var"18".
    let w_cf_bias =
      rtpplWriteFloatRecords
        fileDescriptors.bias 2 (unsafeCoerce (deref outputSeqs).out1)
    in
    let #var"19" = modref outputSeqs { out1 = "" } in
    {}
in
let print: [Char] -> () = lam s7: [Char].
    print s7 in
let printLine: [Char] -> () = lam s6: [Char].
    printLn s6 in
let floatToString: Float -> [Char] = lam f: Float.
    float2string f
in
let intToString: Int -> [Char] = lam i2: Int.
    int2string i2
in
recursive
  let range: Int -> Int -> [Int] =
    lam lo: Int.
      lam hi: Int.
        match
          lti lo hi
        with
          true
        then
          cons lo (range (addi lo 1) hi)
        else
          ""
in
let t2: Float -> () -> TSV Float -> () =
  lam bias1.
    lam #var"14".
      lam obs2.
        let #var"15" =
          match
            eqf (value obs2) 0.
          with
            true
          then
            let #var"16": () =
              observe
                false (Bernoulli
                   bias1)
            in
            {}
          else
            let #var"17": () =
              observe
                true (Bernoulli
                   bias1)
            in
            {}
        in
        {}
in
let coinFlipModel: Dist(Float) -> [TSV Float] -> Float =
  lam prior: Dist(Float).
    lam observations1: [TSV Float].
      let bias = assume
          prior in
      let #var"13" = foldl (t2 bias) {} observations1 in
      bias
in
let printObs: Int -> Ref Timespec -> TSV Float -> () =
  lam nanosPerSec5.
    lam wallLogicalTime5.
      lam obs1: TSV Float.
        let #var"10" =
          print
            (intToString (timestamp nanosPerSec5 wallLogicalTime5 obs1))
        in
        let #var"11" = print " " in
        let #var"12" = printLine (floatToString (value obs1)) in
        {}
in
let t3: Int -> Ref Timespec -> () -> TSV Float -> () =
  lam nanosPerSec4.
    lam wallLogicalTime4.
      lam #var"8".
        lam obs.
          let #var"9" = printObs nanosPerSec4 wallLogicalTime4 obs in
          {}
in
let printInput: Int -> Ref Timespec -> [TSV Float] -> () =
  lam nanosPerSec3.
    lam wallLogicalTime3.
      lam observations: [TSV Float].
        let #var"6" = printLine "Received observations:" in
        let #var"7" = foldl (t3 nanosPerSec3 wallLogicalTime3) {} observations
        in
        {}
in
let t4: {s: [Float], w: [Float]} -> Float -> Int -> Float =
  lam s5.
    lam acc6.
      lam i1.
        let acc7 = addf acc6 (mulf (get s5.s i1) (exp (get s5.w i1)))
        in
        acc7
in
let expectedValue: Dist(Float) -> Float =
  lam d7: Dist(Float).
    let acc4 = 0. in
    let s3 =
      match
        distEmpiricalSamples d7
      with
        (s4, w1)
      in
      { s = s4, w = w1 }
    in
    let acc5 = foldl (t4 s3) acc4 (range 0 (length s3.s)) in
    acc5
in
let t5: Float -> {s: [Float], w: [Float]} -> Float -> Int -> Float =
  lam mu2.
    lam s2.
      lam acc2.
        lam i.
          let acc3 =
            addf
              acc2
              (mulf
                 (mulf (subf (get s2.s i) mu2) (subf (get s2.s i) mu2))
                 (exp (get s2.w i)))
          in
          acc3
in
let stddev: Dist(Float) -> Float -> Float =
  lam d6: Dist(Float).
    lam mu1: Float.
      let acc = 0. in
      let s =
        match
          distEmpiricalSamples d6
        with
          (s1, w)
        in
        { s = s1, w = w }
      in
      let acc1 = foldl (t5 mu1 s) acc (range 0 (length s.s)) in
      sqrt acc1
in
recursive
  let t6: Dist(Float) -> [((Int, Int), Float)] -> () -> Float =
    lam d2.
      lam indata.
        lam #var"2".
          coinFlipModel d2 indata
  let inferModel =
    lam d3.
      lam indata1.
        lam p.
          infer
            (BPF {particles = p})
            (t6 d3 indata1)
  let loopFn =
    lam nanosPerSec2.
      lam wallLogicalTime2.
        lam d4.
          match
            true
          with
            true
          then
            let #var"3" =
              sdelay
                nanosPerSec
                monoLogicalTime
                wallLogicalTime
                cpuExecutionTime
                taskExecTimes
                slowdown
                flushOutputs
                updateInputs
                1000000000
            in
            let indata2: [((Int, Int), Float)] = unsafeCoerce (deref inputSeqs).in1
            in
            let #var"4" = printInput nanosPerSec2 wallLogicalTime2 indata2
            in
            let d5: Dist(Unknown) = rtpplFixedInferRunner 100 (inferModel d4 indata2)
            in
            let mu = expectedValue d5 in
            let sigma = stddev d5 mu in
            let #var"5" =
              let out = deref outputSeqs in
              modref
                outputSeqs
                { out
                  with
                  out1 =
                    cons
                      (tsv nanosPerSec wallLogicalTime 0 { mu = mu, sigma = sigma })
                      out.out1 }
            in
            loopFn nanosPerSec2 wallLogicalTime2 d5
          else
            d4
in
let #var"RTPPL_CoinFlip": Int -> Ref Timespec -> () -> () =
  lam nanosPerSec1.
    lam wallLogicalTime1.
      lam #var"1": ().
        let d = Uniform
            0. 1. in
        let d1 = loopFn nanosPerSec1 wallLogicalTime1 d in
        {}
in
rtpplRuntimeInit
  cmpString
  strJoin
  monoLogicalTime
  wallLogicalTime
  cpuExecutionTime
  particleCount
  taskBudget
  taskExecTimes
  slowdown
  updateInputs
  closeFileDescriptors
  "cf"
  (lam #var"".
     #var"RTPPL_CoinFlip" nanosPerSec wallLogicalTime #var"")
