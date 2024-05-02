include "seq.mc"
include "parser/ll1.mc"
include "parser/breakable.mc"
lang CalcBaseAst
  syn Expr =
  syn File =
  sem smapAccumL_File_File : all a. (a -> File -> (a, File)) -> a -> File -> (a, File)
sem smapAccumL_File_File f acc =
  | x ->
    (acc, x)
  sem smap_File_File : (File -> File) -> File -> File
sem smap_File_File f =
  | x ->
    (smapAccumL_File_File
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_File_File : all a. (a -> File -> a) -> a -> File -> a
sem sfold_File_File f acc =
  | x ->
    (smapAccumL_File_File
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_File_Expr : all a. (a -> Expr -> (a, Expr)) -> a -> File -> (a, File)
sem smapAccumL_File_Expr f acc =
  | x ->
    (acc, x)
  sem smap_File_Expr : (Expr -> Expr) -> File -> File
sem smap_File_Expr f =
  | x ->
    (smapAccumL_File_Expr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_File_Expr : all a. (a -> Expr -> a) -> a -> File -> a
sem sfold_File_Expr f acc =
  | x ->
    (smapAccumL_File_Expr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_Expr_File : all a. (a -> File -> (a, File)) -> a -> Expr -> (a, Expr)
sem smapAccumL_Expr_File f acc =
  | x ->
    (acc, x)
  sem smap_Expr_File : (File -> File) -> Expr -> Expr
sem smap_Expr_File f =
  | x ->
    (smapAccumL_Expr_File
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_Expr_File : all a. (a -> File -> a) -> a -> Expr -> a
sem sfold_Expr_File f acc =
  | x ->
    (smapAccumL_Expr_File
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_Expr_Expr : all a. (a -> Expr -> (a, Expr)) -> a -> Expr -> (a, Expr)
sem smapAccumL_Expr_Expr f acc =
  | x ->
    (acc, x)
  sem smap_Expr_Expr : (Expr -> Expr) -> Expr -> Expr
sem smap_Expr_Expr f =
  | x ->
    (smapAccumL_Expr_Expr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_Expr_Expr : all a. (a -> Expr -> a) -> a -> Expr -> a
sem sfold_Expr_Expr f acc =
  | x ->
    (smapAccumL_Expr_Expr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem get_File_info : File -> Info
  sem set_File_info : Info -> File -> File
sem set_File_info val =
  sem mapAccum_File_info : all a. (a -> Info -> (a, Info)) -> a -> File -> (a, File)
sem mapAccum_File_info f acc =
  | target ->
    match
      f
        acc
        (get_File_info
           target)
    with
      (acc, val)
    in
    (acc, set_File_info
        val
        target)
  sem map_File_info : (Info -> Info) -> File -> File
sem map_File_info f =
  | target ->
    set_File_info
      (f
         (get_File_info
            target))
      target
  sem get_Expr_info : Expr -> Info
  sem set_Expr_info : Info -> Expr -> Expr
sem set_Expr_info val =
  sem mapAccum_Expr_info : all a. (a -> Info -> (a, Info)) -> a -> Expr -> (a, Expr)
sem mapAccum_Expr_info f acc =
  | target ->
    match
      f
        acc
        (get_Expr_info
           target)
    with
      (acc, val)
    in
    (acc, set_Expr_info
        val
        target)
  sem map_Expr_info : (Info -> Info) -> Expr -> Expr
sem map_Expr_info f =
  | target ->
    set_Expr_info
      (f
         (get_Expr_info
            target))
      target
end
lang FileAst =
  CalcBaseAst
  type FileRecord =
    {e: Expr, info: Info}
  syn File =
  | File1 FileRecord
  sem smapAccumL_File_Expr f acc =
  | File1 x ->
    match
      match
        let e =
          x.e
        in
        f
          acc
          e
      with
        (acc, e)
      in
      (acc, { x
          with
          e =
            e })
    with
      (acc, x)
    in
    (acc, File1
        x)
  sem get_File_info =
  | File1 target ->
    target.info
  sem set_File_info val =
  | File1 target ->
    File1
      { target
        with
        info =
          val }
end
lang NumExprAst =
  CalcBaseAst
  type NumExprRecord =
    {val: {i: Info, v: Float}, info: Info}
  syn Expr =
  | NumExpr NumExprRecord
  sem get_Expr_info =
  | NumExpr target ->
    target.info
  sem set_Expr_info val =
  | NumExpr target ->
    NumExpr
      { target
        with
        info =
          val }
end
lang AddExprAst =
  CalcBaseAst
  type AddExprRecord =
    {info: Info, left: Expr, right: Expr}
  syn Expr =
  | AddExpr AddExprRecord
  sem smapAccumL_Expr_Expr f acc =
  | AddExpr x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, AddExpr
        x)
  sem get_Expr_info =
  | AddExpr target ->
    target.info
  sem set_Expr_info val =
  | AddExpr target ->
    AddExpr
      { target
        with
        info =
          val }
end
lang SubExprAst =
  CalcBaseAst
  type SubExprRecord =
    {info: Info, left: Expr, right: Expr}
  syn Expr =
  | SubExpr SubExprRecord
  sem smapAccumL_Expr_Expr f acc =
  | SubExpr x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, SubExpr
        x)
  sem get_Expr_info =
  | SubExpr target ->
    target.info
  sem set_Expr_info val =
  | SubExpr target ->
    SubExpr
      { target
        with
        info =
          val }
end
lang MulExprAst =
  CalcBaseAst
  type MulExprRecord =
    {info: Info, left: Expr, right: Expr}
  syn Expr =
  | MulExpr MulExprRecord
  sem smapAccumL_Expr_Expr f acc =
  | MulExpr x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, MulExpr
        x)
  sem get_Expr_info =
  | MulExpr target ->
    target.info
  sem set_Expr_info val =
  | MulExpr target ->
    MulExpr
      { target
        with
        info =
          val }
end
lang DivExprAst =
  CalcBaseAst
  type DivExprRecord =
    {info: Info, left: Expr, right: Expr}
  syn Expr =
  | DivExpr DivExprRecord
  sem smapAccumL_Expr_Expr f acc =
  | DivExpr x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, DivExpr
        x)
  sem get_Expr_info =
  | DivExpr target ->
    target.info
  sem set_Expr_info val =
  | DivExpr target ->
    DivExpr
      { target
        with
        info =
          val }
end
lang BadFileAst =
  CalcBaseAst
  type BadFileRecord =
    {info: Info}
  syn File =
  | BadFile BadFileRecord
  sem get_File_info =
  | BadFile target ->
    target.info
  sem set_File_info val =
  | BadFile target ->
    BadFile
      { target
        with
        info =
          val }
end
lang BadExprAst =
  CalcBaseAst
  type BadExprRecord =
    {info: Info}
  syn Expr =
  | BadExpr BadExprRecord
  sem get_Expr_info =
  | BadExpr target ->
    target.info
  sem set_Expr_info val =
  | BadExpr target ->
    BadExpr
      { target
        with
        info =
          val }
end
lang CalcAst =
  FileAst
  + NumExprAst
  + AddExprAst
  + SubExprAst
  + MulExprAst
  + DivExprAst
  + BadFileAst
  + BadExprAst
end
lang FileOpBase =
  CalcAst
  syn FileOp lstyle rstyle =
  sem topAllowed_FileOp : all lstyle. all rstyle. FileOp lstyle rstyle -> Bool
sem topAllowed_FileOp =
  | _ ->
    true
  sem leftAllowed_FileOp : all lstyle. all style. all rstyle. {child: FileOp lstyle rstyle, parent: FileOp LOpen style} -> Bool
sem leftAllowed_FileOp =
  | _ ->
    true
  sem rightAllowed_FileOp : all style. all lstyle. all rstyle. {child: FileOp lstyle rstyle, parent: FileOp style ROpen} -> Bool
sem rightAllowed_FileOp =
  | _ ->
    true
  sem groupingsAllowed_FileOp : all lstyle. all rstyle. (FileOp lstyle ROpen, FileOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_FileOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_FileOp : all lstyle. all rstyle. FileOp lstyle rstyle -> AllowedDirection
sem parenAllowed_FileOp =
  | _ ->
    GEither
      {}
  sem getInfo_FileOp : all lstyle. all rstyle. FileOp lstyle rstyle -> Info
  sem getTerms_FileOp : all lstyle. all rstyle. FileOp lstyle rstyle -> [Info]
  sem unsplit_FileOp : PermanentNode FileOp -> (Info, File)
end
lang ExprOpBase =
  CalcAst
  syn ExprOp lstyle rstyle =
  sem topAllowed_ExprOp : all lstyle. all rstyle. ExprOp lstyle rstyle -> Bool
sem topAllowed_ExprOp =
  | _ ->
    true
  sem leftAllowed_ExprOp : all lstyle. all style. all rstyle. {child: ExprOp lstyle rstyle, parent: ExprOp LOpen style} -> Bool
sem leftAllowed_ExprOp =
  | _ ->
    true
  sem rightAllowed_ExprOp : all style. all lstyle. all rstyle. {child: ExprOp lstyle rstyle, parent: ExprOp style ROpen} -> Bool
sem rightAllowed_ExprOp =
  | _ ->
    true
  sem groupingsAllowed_ExprOp : all lstyle. all rstyle. (ExprOp lstyle ROpen, ExprOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_ExprOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_ExprOp : all lstyle. all rstyle. ExprOp lstyle rstyle -> AllowedDirection
sem parenAllowed_ExprOp =
  | _ ->
    GEither
      {}
  sem getInfo_ExprOp : all lstyle. all rstyle. ExprOp lstyle rstyle -> Info
  sem getTerms_ExprOp : all lstyle. all rstyle. ExprOp lstyle rstyle -> [Info]
  sem unsplit_ExprOp : PermanentNode ExprOp -> (Info, Expr)
end
lang FileOp1 =
  FileOpBase
  + FileAst
  syn FileOp lstyle rstyle =
  | FileOp1 {e: [Expr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_FileOp =
  | FileOp1 x ->
    x.__br_info
  sem getTerms_FileOp =
  | FileOp1 x ->
    x.__br_terms
  sem unsplit_FileOp =
  | AtomP {self = FileOp1 x} ->
    (x.__br_info, File1
      { e =
          match
            x.e
          with
            [ x1 ] ++ _ ++ ""
          in
          x1,
        info =
          x.__br_info })
end
lang NumExprOp =
  ExprOpBase
  + NumExprAst
  syn ExprOp lstyle rstyle =
  | NumExprOp {val: [{i: Info, v: Float}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_ExprOp =
  | NumExprOp x ->
    x.__br_info
  sem getTerms_ExprOp =
  | NumExprOp x ->
    x.__br_terms
  sem unsplit_ExprOp =
  | AtomP {self = NumExprOp x} ->
    (x.__br_info, NumExpr
      { info =
          x.__br_info,
        val =
          match
            x.val
          with
            [ x1 ] ++ _ ++ ""
          in
          x1 })
end
lang AddExprOp =
  ExprOpBase
  + AddExprAst
  syn ExprOp lstyle rstyle =
  | AddExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_ExprOp =
  | AddExprOp x ->
    x.__br_info
  sem getTerms_ExprOp =
  | AddExprOp x ->
    x.__br_terms
  sem unsplit_ExprOp =
  | InfixP {self = AddExprOp x, leftChildAlts = [ l ] ++ _ ++ "", rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      (unsplit_ExprOp
        l, unsplit_ExprOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, AddExpr
        { info =
            info,
          left =
            match
              [ l ]
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          right =
            match
              [ r ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2 })
end
lang SubExprOp =
  ExprOpBase
  + SubExprAst
  syn ExprOp lstyle rstyle =
  | SubExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_ExprOp =
  | SubExprOp x ->
    x.__br_info
  sem getTerms_ExprOp =
  | SubExprOp x ->
    x.__br_terms
  sem unsplit_ExprOp =
  | InfixP {self = SubExprOp x, leftChildAlts = [ l ] ++ _ ++ "", rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      (unsplit_ExprOp
        l, unsplit_ExprOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, SubExpr
        { info =
            info,
          left =
            match
              [ l ]
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          right =
            match
              [ r ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2 })
end
lang MulExprOp =
  ExprOpBase
  + MulExprAst
  syn ExprOp lstyle rstyle =
  | MulExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_ExprOp =
  | MulExprOp x ->
    x.__br_info
  sem getTerms_ExprOp =
  | MulExprOp x ->
    x.__br_terms
  sem unsplit_ExprOp =
  | InfixP {self = MulExprOp x, leftChildAlts = [ l ] ++ _ ++ "", rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      (unsplit_ExprOp
        l, unsplit_ExprOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, MulExpr
        { info =
            info,
          left =
            match
              [ l ]
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          right =
            match
              [ r ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2 })
end
lang DivExprOp =
  ExprOpBase
  + DivExprAst
  syn ExprOp lstyle rstyle =
  | DivExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_ExprOp =
  | DivExprOp x ->
    x.__br_info
  sem getTerms_ExprOp =
  | DivExprOp x ->
    x.__br_terms
  sem unsplit_ExprOp =
  | InfixP {self = DivExprOp x, leftChildAlts = [ l ] ++ _ ++ "", rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      (unsplit_ExprOp
        l, unsplit_ExprOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, DivExpr
        { info =
            info,
          left =
            match
              [ l ]
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          right =
            match
              [ r ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2 })
end
lang ExprGrouping =
  ExprOpBase
  syn ExprOp lstyle rstyle =
  | ExprGrouping {inner: Expr, __br_info: Info, __br_terms: [Info]}
  sem getInfo_ExprOp =
  | ExprGrouping x ->
    x.__br_info
  sem getTerms_ExprOp =
  | ExprGrouping x ->
    x.__br_terms
  sem unsplit_ExprOp =
  | AtomP {self = ExprGrouping x} ->
    (x.__br_info, x.inner)
end
lang ParseCalc =
  FileOp1
  + NumExprOp
  + AddExprOp
  + SubExprOp
  + MulExprOp
  + DivExprOp
  + ExprGrouping
  + BadFileAst
  + BadExprAst
  + LL1Parser
  + SemiTokenParser
  + UIntTokenParser
  + CommaTokenParser
  + WhitespaceParser
  + LIdentTokenParser
  + LineCommentParser
  + StringTokenParser
  + UFloatTokenParser
  + UIdentTokenParser
  + BracketTokenParser
  + OperatorTokenParser
  + MultilineCommentParser
  
  sem groupingsAllowed_ExprOp =
  | (AddExprOp _, MulExprOp _) ->
    GRight
      {}
  | (MulExprOp _, AddExprOp _) ->
    GLeft
      {}
  | (AddExprOp _, DivExprOp _) ->
    GRight
      {}
  | (DivExprOp _, AddExprOp _) ->
    GLeft
      {}
  | (SubExprOp _, MulExprOp _) ->
    GRight
      {}
  | (MulExprOp _, SubExprOp _) ->
    GLeft
      {}
  | (SubExprOp _, DivExprOp _) ->
    GRight
      {}
  | (DivExprOp _, SubExprOp _) ->
    GLeft
      {}
end
let _table =
  use ParseCalc
  in
  let target =
    genParsingTable
      (let #var"File" =
         nameSym
           "File"
       in
       let #var"Expr" =
         nameSym
           "Expr"
       in
       let #var"FilePostfix" =
         nameSym
           "FilePostfix"
       in
       let #var"FilePrefix" =
         nameSym
           "FilePrefix"
       in
       let #var"FileInfix" =
         nameSym
           "FileInfix"
       in
       let #var"FileAtom" =
         nameSym
           "FileAtom"
       in
       let #var"ExprPostfix" =
         nameSym
           "ExprPostfix"
       in
       let #var"ExprPrefix" =
         nameSym
           "ExprPrefix"
       in
       let #var"ExprInfix" =
         nameSym
           "ExprInfix"
       in
       let #var"ExprAtom" =
         nameSym
           "ExprAtom"
       in
       let #var"File_lclosed" =
         nameSym
           "File_lclosed"
       in
       let #var"File_lopen" =
         nameSym
           "File_lopen"
       in
       let #var"Expr_lclosed" =
         nameSym
           "Expr_lclosed"
       in
       let #var"Expr_lopen" =
         nameSym
           "Expr_lopen"
       in
       { start =
           #var"File",
         productions =
           let config =
             { parenAllowed =
                 #frozen"parenAllowed_FileOp",
               topAllowed =
                 #frozen"topAllowed_FileOp",
               leftAllowed =
                 #frozen"leftAllowed_FileOp",
               rightAllowed =
                 #frozen"rightAllowed_FileOp",
               groupingsAllowed =
                 #frozen"groupingsAllowed_FileOp" }
           in
           let reportConfig =
             { parenAllowed =
                 #frozen"parenAllowed_FileOp",
               topAllowed =
                 #frozen"topAllowed_FileOp",
               terminalInfos =
                 #frozen"getTerms_FileOp",
               getInfo =
                 #frozen"getInfo_FileOp",
               lpar =
                 "(",
               rpar =
                 ")" }
           in
           let addFileOpAtom =
             lam #var"".
               lam x1.
                 lam st.
                   optionMap
                     (breakableAddAtom
                        config
                        x1)
                     st
           in
           let addFileOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x1.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddInfix
                         config
                         x1
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_FileOp
                                 x1, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addFileOpPrefix =
             lam #var"".
               lam x1.
                 lam st.
                   optionMap
                     (breakableAddPrefix
                        config
                        x1)
                     st
           in
           let addFileOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x1.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddPostfix
                         config
                         x1
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_FileOp
                                 x1, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeFileOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res6 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse
                            config
                            st
                        with
                          Some (tops & ([ top ] ++ _ ++ ""))
                        then
                          let errs =
                            breakableDefaultHighlight
                              reportConfig
                              p.content
                              tops
                          in
                          let res6 =
                            unsplit_FileOp
                              top
                          in
                          match
                            null
                              errs
                          with
                            true
                          then
                            Some
                              res6
                          else
                            (modref
                                 p.errors
                                 (concat
                                    (deref
                                       p.errors)
                                    errs))
                            ; Some
                              (res6.0, BadFile
                                { info =
                                    res6.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref
                                     p.errors)
                                  (NoInfo
                                    {}, "Unfinished File")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadFile
                     { info =
                         NoInfo
                           {} })
                   res6
           in
           let config1 =
             { parenAllowed =
                 #frozen"parenAllowed_ExprOp",
               topAllowed =
                 #frozen"topAllowed_ExprOp",
               leftAllowed =
                 #frozen"leftAllowed_ExprOp",
               rightAllowed =
                 #frozen"rightAllowed_ExprOp",
               groupingsAllowed =
                 #frozen"groupingsAllowed_ExprOp" }
           in
           let reportConfig1 =
             { parenAllowed =
                 #frozen"parenAllowed_ExprOp",
               topAllowed =
                 #frozen"topAllowed_ExprOp",
               terminalInfos =
                 #frozen"getTerms_ExprOp",
               getInfo =
                 #frozen"getInfo_ExprOp",
               lpar =
                 "(",
               rpar =
                 ")" }
           in
           let addExprOpAtom =
             lam #var"".
               lam x1.
                 lam st.
                   optionMap
                     (breakableAddAtom
                        config1
                        x1)
                     st
           in
           let addExprOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x1.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddInfix
                         config1
                         x1
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_ExprOp
                                 x1, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addExprOpPrefix =
             lam #var"".
               lam x1.
                 lam st.
                   optionMap
                     (breakableAddPrefix
                        config1
                        x1)
                     st
           in
           let addExprOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x1.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddPostfix
                         config1
                         x1
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_ExprOp
                                 x1, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeExprOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res6 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse
                            config1
                            st
                        with
                          Some (tops & ([ top ] ++ _ ++ ""))
                        then
                          let errs =
                            breakableDefaultHighlight
                              reportConfig1
                              p.content
                              tops
                          in
                          let res6 =
                            unsplit_ExprOp
                              top
                          in
                          match
                            null
                              errs
                          with
                            true
                          then
                            Some
                              res6
                          else
                            (modref
                                 p.errors
                                 (concat
                                    (deref
                                       p.errors)
                                    errs))
                            ; Some
                              (res6.0, BadExpr
                                { info =
                                    res6.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref
                                     p.errors)
                                  (NoInfo
                                    {}, "Unfinished Expr")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadExpr
                     { info =
                         NoInfo
                           {} })
                   res6
           in
           [ { nt =
                 #var"FileAtom",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"Expr" ],
               action =
                 lam state: {errors: Ref [(Info, [Char])], content: String}.
                   lam res.
                     match
                       res
                     with
                       [ UserSym ntVal ]
                     in
                     let ntVal: (Info, Expr) =
                         fromDyn
                           ntVal
                       in
                       asDyn
                         (FileOp1
                            { e =
                                [ ntVal.1 ],
                              __br_info =
                                ntVal.0,
                              __br_terms =
                                "" }) },
             { nt =
                 #var"ExprAtom",
               label =
                 {},
               rhs =
                 [ tokSym
                     (FloatRepr
                        {}) ],
               action =
                 lam state1: {errors: Ref [(Info, [Char])], content: String}.
                   lam res1.
                     match
                       res1
                     with
                       [ TokParsed (FloatTok x) ]
                     in
                     asDyn
                         (NumExprOp
                            { __br_info =
                                x.info,
                              __br_terms =
                                [ x.info ],
                              val =
                                [ { v =
                                      x.val,
                                    i =
                                      x.info } ] }) },
             { nt =
                 #var"ExprInfix",
               label =
                 {},
               rhs =
                 [ litSym
                     "+" ],
               action =
                 lam state2: {errors: Ref [(Info, [Char])], content: String}.
                   lam res2.
                     match
                       res2
                     with
                       [ LitParsed l ]
                     in
                     asDyn
                         (AddExprOp
                            { __br_info =
                                l.info,
                              __br_terms =
                                [ l.info ] }) },
             { nt =
                 #var"ExprInfix",
               label =
                 {},
               rhs =
                 [ litSym
                     "-" ],
               action =
                 lam state3: {errors: Ref [(Info, [Char])], content: String}.
                   lam res3.
                     match
                       res3
                     with
                       [ LitParsed l1 ]
                     in
                     asDyn
                         (SubExprOp
                            { __br_info =
                                l1.info,
                              __br_terms =
                                [ l1.info ] }) },
             { nt =
                 #var"ExprInfix",
               label =
                 {},
               rhs =
                 [ litSym
                     "*" ],
               action =
                 lam state4: {errors: Ref [(Info, [Char])], content: String}.
                   lam res4.
                     match
                       res4
                     with
                       [ LitParsed l2 ]
                     in
                     asDyn
                         (MulExprOp
                            { __br_info =
                                l2.info,
                              __br_terms =
                                [ l2.info ] }) },
             { nt =
                 #var"ExprInfix",
               label =
                 {},
               rhs =
                 [ litSym
                     "/" ],
               action =
                 lam state5: {errors: Ref [(Info, [Char])], content: String}.
                   lam res5.
                     match
                       res5
                     with
                       [ LitParsed l3 ]
                     in
                     asDyn
                         (DivExprOp
                            { __br_info =
                                l3.info,
                              __br_terms =
                                [ l3.info ] }) },
             { nt =
                 #var"ExprAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "(",
                   ntSym
                     #var"Expr",
                   litSym
                     ")" ],
               action =
                 lam #var"".
                   lam seq.
                     match
                       seq
                     with
                       [ LitParsed l4,
                         UserSym ntVal1,
                         LitParsed l5 ]
                     in
                     let ntVal1: (Info, Expr) =
                         fromDyn
                           ntVal1
                       in
                       asDyn
                         (ExprGrouping
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l4.info
                                  [ ntVal1.0,
                                    l5.info ],
                              __br_terms =
                                [ l4.info,
                                  l5.info ],
                              inner =
                                match
                                  [ ntVal1.1 ]
                                with
                                  [ x1 ]
                                in
                                x1 }) },
             { nt =
                 #var"File",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"File_lclosed" ],
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
                            (breakableInitState
                               {})) },
             { nt =
                 #var"File_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"FileAtom",
                   ntSym
                     #var"File_lopen" ],
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x1,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addFileOpAtom
                                 p
                                 (fromDyn
                                    x1)
                                 st)) },
             { nt =
                 #var"File_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"FileInfix",
                   ntSym
                     #var"File_lclosed" ],
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x1,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addFileOpInfix
                                 p
                                 (fromDyn
                                    x1)
                                 st)) },
             { nt =
                 #var"File_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"FilePrefix",
                   ntSym
                     #var"File_lclosed" ],
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x1,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addFileOpPrefix
                                 p
                                 (fromDyn
                                    x1)
                                 st)) },
             { nt =
                 #var"File_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"FilePostfix",
                   ntSym
                     #var"File_lopen" ],
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x1,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addFileOpPostfix
                                 p
                                 (fromDyn
                                    x1)
                                 st)) },
             { nt =
                 #var"File_lopen",
               label =
                 {},
               rhs =
                 "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeFileOp
                            p
                            st) },
             { nt =
                 #var"Expr",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"Expr_lclosed" ],
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
                            (breakableInitState
                               {})) },
             { nt =
                 #var"Expr_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"ExprAtom",
                   ntSym
                     #var"Expr_lopen" ],
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x1,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addExprOpAtom
                                 p
                                 (fromDyn
                                    x1)
                                 st)) },
             { nt =
                 #var"Expr_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"ExprInfix",
                   ntSym
                     #var"Expr_lclosed" ],
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x1,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addExprOpInfix
                                 p
                                 (fromDyn
                                    x1)
                                 st)) },
             { nt =
                 #var"Expr_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"ExprPrefix",
                   ntSym
                     #var"Expr_lclosed" ],
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x1,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addExprOpPrefix
                                 p
                                 (fromDyn
                                    x1)
                                 st)) },
             { nt =
                 #var"Expr_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"ExprPostfix",
                   ntSym
                     #var"Expr_lopen" ],
               action =
                 lam p.
                   lam seq1.
                     match
                       seq1
                     with
                       [ UserSym x1,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addExprOpPostfix
                                 p
                                 (fromDyn
                                    x1)
                                 st)) },
             { nt =
                 #var"Expr_lopen",
               label =
                 {},
               rhs =
                 "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeExprOp
                            p
                            st) } ] })
  in
  match
    target
  with
    Right table
  in
  table
let parseCalc =
  lam filename.
    lam content.
      use ParseCalc
      in
      let config2 =
        { errors =
            ref
              "",
          content =
            content }
      in
      let res6 =
        parseWithTable
          _table
          filename
          config2
          content
      in
      let #var"X" =
        (res6, deref
          config2.errors)
      in
      match
        #var"X"
      with
        (Right dyn, "")
      then
        match
          fromDyn
            dyn
        with
          (_, res6)
        in
        Right
            res6
      else
        match
          #var"X"
        with
          (Left err, errors)
        then
          let err =
            ll1DefaultHighlight
              content
              (ll1ToErrorHighlightSpec
                 err)
          in
          Left
            (snoc
               errors
               err)
        else
          match
            #var"X"
          with
            (_, errors)
          in
          Left
              errors
let parseCalcExn =
  lam filename.
    lam content.
      let #var"X" =
        parseCalc
          filename
          content
      in
      match
        #var"X"
      with
        Left errors
      then
        (for_
             errors
             (lam x1.
                match
                  x1
                with
                  (info, msg)
                in
                printLn
                    (infoErrorString
                       info
                       msg)))
        ; exit
          1
      else
        match
          #var"X"
        with
          Right file
        in
        file
mexpr
{}