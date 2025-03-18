include "seq.mc"
include "parser/ll1.mc"
include "parser/breakable.mc"
include "lexer.mc"
lang RtpplBaseAst
  syn RtpplPortSpec =
  syn RtpplConnection =
  syn RtpplTask =
  syn RtpplExt =
  syn RtpplMain =
  syn RtpplPort =
  syn RtpplConst =
  syn RtpplTypeNoIdent =
  syn RtpplType =
  syn RtpplExprNoIdent =
  syn RtpplExpr =
  syn RtpplStmtNoIdent =
  syn RtpplStmt =
  syn RtpplTopParams =
  syn RtpplTop =
  syn RtpplProgram =
  sem smapAccumL_RtpplProgram_RtpplProgram : all a. (a -> RtpplProgram -> (a, RtpplProgram)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem smapAccumL_RtpplProgram_RtpplProgram f acc =
  | x ->
    (acc, x)
  sem smap_RtpplProgram_RtpplProgram : (RtpplProgram -> RtpplProgram) -> RtpplProgram -> RtpplProgram
sem smap_RtpplProgram_RtpplProgram f =
  | x ->
    (smapAccumL_RtpplProgram_RtpplProgram (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplProgram_RtpplProgram : all a. (a -> RtpplProgram -> a) -> a -> RtpplProgram -> a
sem sfold_RtpplProgram_RtpplProgram f acc =
  | x ->
    (smapAccumL_RtpplProgram_RtpplProgram (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplProgram_RtpplTop : all a. (a -> RtpplTop -> (a, RtpplTop)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem smapAccumL_RtpplProgram_RtpplTop f acc =
  | x ->
    (acc, x)
  sem smap_RtpplProgram_RtpplTop : (RtpplTop -> RtpplTop) -> RtpplProgram -> RtpplProgram
sem smap_RtpplProgram_RtpplTop f =
  | x ->
    (smapAccumL_RtpplProgram_RtpplTop (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplProgram_RtpplTop : all a. (a -> RtpplTop -> a) -> a -> RtpplProgram -> a
sem sfold_RtpplProgram_RtpplTop f acc =
  | x ->
    (smapAccumL_RtpplProgram_RtpplTop (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplProgram_RtpplTopParams : all a. (a -> RtpplTopParams -> (a, RtpplTopParams)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem smapAccumL_RtpplProgram_RtpplTopParams f acc =
  | x ->
    (acc, x)
  sem smap_RtpplProgram_RtpplTopParams : (RtpplTopParams -> RtpplTopParams) -> RtpplProgram -> RtpplProgram
sem smap_RtpplProgram_RtpplTopParams f =
  | x ->
    (smapAccumL_RtpplProgram_RtpplTopParams (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplProgram_RtpplTopParams : all a. (a -> RtpplTopParams -> a) -> a -> RtpplProgram -> a
sem sfold_RtpplProgram_RtpplTopParams f acc =
  | x ->
    (smapAccumL_RtpplProgram_RtpplTopParams (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplProgram_RtpplStmt : all a. (a -> RtpplStmt -> (a, RtpplStmt)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem smapAccumL_RtpplProgram_RtpplStmt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplProgram_RtpplStmt : (RtpplStmt -> RtpplStmt) -> RtpplProgram -> RtpplProgram
sem smap_RtpplProgram_RtpplStmt f =
  | x ->
    (smapAccumL_RtpplProgram_RtpplStmt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplProgram_RtpplStmt : all a. (a -> RtpplStmt -> a) -> a -> RtpplProgram -> a
sem sfold_RtpplProgram_RtpplStmt f acc =
  | x ->
    (smapAccumL_RtpplProgram_RtpplStmt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplProgram_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem smapAccumL_RtpplProgram_RtpplStmtNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplProgram_RtpplStmtNoIdent : (RtpplStmtNoIdent -> RtpplStmtNoIdent) -> RtpplProgram -> RtpplProgram
sem smap_RtpplProgram_RtpplStmtNoIdent f =
  | x ->
    (smapAccumL_RtpplProgram_RtpplStmtNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplProgram_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> a) -> a -> RtpplProgram -> a
sem sfold_RtpplProgram_RtpplStmtNoIdent f acc =
  | x ->
    (smapAccumL_RtpplProgram_RtpplStmtNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplProgram_RtpplExpr : all a. (a -> RtpplExpr -> (a, RtpplExpr)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem smapAccumL_RtpplProgram_RtpplExpr f acc =
  | x ->
    (acc, x)
  sem smap_RtpplProgram_RtpplExpr : (RtpplExpr -> RtpplExpr) -> RtpplProgram -> RtpplProgram
sem smap_RtpplProgram_RtpplExpr f =
  | x ->
    (smapAccumL_RtpplProgram_RtpplExpr (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplProgram_RtpplExpr : all a. (a -> RtpplExpr -> a) -> a -> RtpplProgram -> a
sem sfold_RtpplProgram_RtpplExpr f acc =
  | x ->
    (smapAccumL_RtpplProgram_RtpplExpr (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplProgram_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem smapAccumL_RtpplProgram_RtpplExprNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplProgram_RtpplExprNoIdent : (RtpplExprNoIdent -> RtpplExprNoIdent) -> RtpplProgram -> RtpplProgram
sem smap_RtpplProgram_RtpplExprNoIdent f =
  | x ->
    (smapAccumL_RtpplProgram_RtpplExprNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplProgram_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> a) -> a -> RtpplProgram -> a
sem sfold_RtpplProgram_RtpplExprNoIdent f acc =
  | x ->
    (smapAccumL_RtpplProgram_RtpplExprNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplProgram_RtpplType : all a. (a -> RtpplType -> (a, RtpplType)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem smapAccumL_RtpplProgram_RtpplType f acc =
  | x ->
    (acc, x)
  sem smap_RtpplProgram_RtpplType : (RtpplType -> RtpplType) -> RtpplProgram -> RtpplProgram
sem smap_RtpplProgram_RtpplType f =
  | x ->
    (smapAccumL_RtpplProgram_RtpplType (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplProgram_RtpplType : all a. (a -> RtpplType -> a) -> a -> RtpplProgram -> a
sem sfold_RtpplProgram_RtpplType f acc =
  | x ->
    (smapAccumL_RtpplProgram_RtpplType (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplProgram_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem smapAccumL_RtpplProgram_RtpplTypeNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplProgram_RtpplTypeNoIdent : (RtpplTypeNoIdent -> RtpplTypeNoIdent) -> RtpplProgram -> RtpplProgram
sem smap_RtpplProgram_RtpplTypeNoIdent f =
  | x ->
    (smapAccumL_RtpplProgram_RtpplTypeNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplProgram_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> a) -> a -> RtpplProgram -> a
sem sfold_RtpplProgram_RtpplTypeNoIdent f acc =
  | x ->
    (smapAccumL_RtpplProgram_RtpplTypeNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplProgram_RtpplConst : all a. (a -> RtpplConst -> (a, RtpplConst)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem smapAccumL_RtpplProgram_RtpplConst f acc =
  | x ->
    (acc, x)
  sem smap_RtpplProgram_RtpplConst : (RtpplConst -> RtpplConst) -> RtpplProgram -> RtpplProgram
sem smap_RtpplProgram_RtpplConst f =
  | x ->
    (smapAccumL_RtpplProgram_RtpplConst (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplProgram_RtpplConst : all a. (a -> RtpplConst -> a) -> a -> RtpplProgram -> a
sem sfold_RtpplProgram_RtpplConst f acc =
  | x ->
    (smapAccumL_RtpplProgram_RtpplConst (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplProgram_RtpplPort : all a. (a -> RtpplPort -> (a, RtpplPort)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem smapAccumL_RtpplProgram_RtpplPort f acc =
  | x ->
    (acc, x)
  sem smap_RtpplProgram_RtpplPort : (RtpplPort -> RtpplPort) -> RtpplProgram -> RtpplProgram
sem smap_RtpplProgram_RtpplPort f =
  | x ->
    (smapAccumL_RtpplProgram_RtpplPort (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplProgram_RtpplPort : all a. (a -> RtpplPort -> a) -> a -> RtpplProgram -> a
sem sfold_RtpplProgram_RtpplPort f acc =
  | x ->
    (smapAccumL_RtpplProgram_RtpplPort (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplProgram_RtpplMain : all a. (a -> RtpplMain -> (a, RtpplMain)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem smapAccumL_RtpplProgram_RtpplMain f acc =
  | x ->
    (acc, x)
  sem smap_RtpplProgram_RtpplMain : (RtpplMain -> RtpplMain) -> RtpplProgram -> RtpplProgram
sem smap_RtpplProgram_RtpplMain f =
  | x ->
    (smapAccumL_RtpplProgram_RtpplMain (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplProgram_RtpplMain : all a. (a -> RtpplMain -> a) -> a -> RtpplProgram -> a
sem sfold_RtpplProgram_RtpplMain f acc =
  | x ->
    (smapAccumL_RtpplProgram_RtpplMain (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplProgram_RtpplExt : all a. (a -> RtpplExt -> (a, RtpplExt)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem smapAccumL_RtpplProgram_RtpplExt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplProgram_RtpplExt : (RtpplExt -> RtpplExt) -> RtpplProgram -> RtpplProgram
sem smap_RtpplProgram_RtpplExt f =
  | x ->
    (smapAccumL_RtpplProgram_RtpplExt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplProgram_RtpplExt : all a. (a -> RtpplExt -> a) -> a -> RtpplProgram -> a
sem sfold_RtpplProgram_RtpplExt f acc =
  | x ->
    (smapAccumL_RtpplProgram_RtpplExt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplProgram_RtpplTask : all a. (a -> RtpplTask -> (a, RtpplTask)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem smapAccumL_RtpplProgram_RtpplTask f acc =
  | x ->
    (acc, x)
  sem smap_RtpplProgram_RtpplTask : (RtpplTask -> RtpplTask) -> RtpplProgram -> RtpplProgram
sem smap_RtpplProgram_RtpplTask f =
  | x ->
    (smapAccumL_RtpplProgram_RtpplTask (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplProgram_RtpplTask : all a. (a -> RtpplTask -> a) -> a -> RtpplProgram -> a
sem sfold_RtpplProgram_RtpplTask f acc =
  | x ->
    (smapAccumL_RtpplProgram_RtpplTask (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplProgram_RtpplConnection : all a. (a -> RtpplConnection -> (a, RtpplConnection)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem smapAccumL_RtpplProgram_RtpplConnection f acc =
  | x ->
    (acc, x)
  sem smap_RtpplProgram_RtpplConnection : (RtpplConnection -> RtpplConnection) -> RtpplProgram -> RtpplProgram
sem smap_RtpplProgram_RtpplConnection f =
  | x ->
    (smapAccumL_RtpplProgram_RtpplConnection (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplProgram_RtpplConnection : all a. (a -> RtpplConnection -> a) -> a -> RtpplProgram -> a
sem sfold_RtpplProgram_RtpplConnection f acc =
  | x ->
    (smapAccumL_RtpplProgram_RtpplConnection (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplProgram_RtpplPortSpec : all a. (a -> RtpplPortSpec -> (a, RtpplPortSpec)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem smapAccumL_RtpplProgram_RtpplPortSpec f acc =
  | x ->
    (acc, x)
  sem smap_RtpplProgram_RtpplPortSpec : (RtpplPortSpec -> RtpplPortSpec) -> RtpplProgram -> RtpplProgram
sem smap_RtpplProgram_RtpplPortSpec f =
  | x ->
    (smapAccumL_RtpplProgram_RtpplPortSpec (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplProgram_RtpplPortSpec : all a. (a -> RtpplPortSpec -> a) -> a -> RtpplProgram -> a
sem sfold_RtpplProgram_RtpplPortSpec f acc =
  | x ->
    (smapAccumL_RtpplProgram_RtpplPortSpec (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTop_RtpplProgram : all a. (a -> RtpplProgram -> (a, RtpplProgram)) -> a -> RtpplTop -> (a, RtpplTop)
sem smapAccumL_RtpplTop_RtpplProgram f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTop_RtpplProgram : (RtpplProgram -> RtpplProgram) -> RtpplTop -> RtpplTop
sem smap_RtpplTop_RtpplProgram f =
  | x ->
    (smapAccumL_RtpplTop_RtpplProgram (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTop_RtpplProgram : all a. (a -> RtpplProgram -> a) -> a -> RtpplTop -> a
sem sfold_RtpplTop_RtpplProgram f acc =
  | x ->
    (smapAccumL_RtpplTop_RtpplProgram (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTop_RtpplTop : all a. (a -> RtpplTop -> (a, RtpplTop)) -> a -> RtpplTop -> (a, RtpplTop)
sem smapAccumL_RtpplTop_RtpplTop f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTop_RtpplTop : (RtpplTop -> RtpplTop) -> RtpplTop -> RtpplTop
sem smap_RtpplTop_RtpplTop f =
  | x ->
    (smapAccumL_RtpplTop_RtpplTop (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTop_RtpplTop : all a. (a -> RtpplTop -> a) -> a -> RtpplTop -> a
sem sfold_RtpplTop_RtpplTop f acc =
  | x ->
    (smapAccumL_RtpplTop_RtpplTop (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTop_RtpplTopParams : all a. (a -> RtpplTopParams -> (a, RtpplTopParams)) -> a -> RtpplTop -> (a, RtpplTop)
sem smapAccumL_RtpplTop_RtpplTopParams f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTop_RtpplTopParams : (RtpplTopParams -> RtpplTopParams) -> RtpplTop -> RtpplTop
sem smap_RtpplTop_RtpplTopParams f =
  | x ->
    (smapAccumL_RtpplTop_RtpplTopParams (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTop_RtpplTopParams : all a. (a -> RtpplTopParams -> a) -> a -> RtpplTop -> a
sem sfold_RtpplTop_RtpplTopParams f acc =
  | x ->
    (smapAccumL_RtpplTop_RtpplTopParams (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTop_RtpplStmt : all a. (a -> RtpplStmt -> (a, RtpplStmt)) -> a -> RtpplTop -> (a, RtpplTop)
sem smapAccumL_RtpplTop_RtpplStmt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTop_RtpplStmt : (RtpplStmt -> RtpplStmt) -> RtpplTop -> RtpplTop
sem smap_RtpplTop_RtpplStmt f =
  | x ->
    (smapAccumL_RtpplTop_RtpplStmt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTop_RtpplStmt : all a. (a -> RtpplStmt -> a) -> a -> RtpplTop -> a
sem sfold_RtpplTop_RtpplStmt f acc =
  | x ->
    (smapAccumL_RtpplTop_RtpplStmt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTop_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)) -> a -> RtpplTop -> (a, RtpplTop)
sem smapAccumL_RtpplTop_RtpplStmtNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTop_RtpplStmtNoIdent : (RtpplStmtNoIdent -> RtpplStmtNoIdent) -> RtpplTop -> RtpplTop
sem smap_RtpplTop_RtpplStmtNoIdent f =
  | x ->
    (smapAccumL_RtpplTop_RtpplStmtNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTop_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> a) -> a -> RtpplTop -> a
sem sfold_RtpplTop_RtpplStmtNoIdent f acc =
  | x ->
    (smapAccumL_RtpplTop_RtpplStmtNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTop_RtpplExpr : all a. (a -> RtpplExpr -> (a, RtpplExpr)) -> a -> RtpplTop -> (a, RtpplTop)
sem smapAccumL_RtpplTop_RtpplExpr f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTop_RtpplExpr : (RtpplExpr -> RtpplExpr) -> RtpplTop -> RtpplTop
sem smap_RtpplTop_RtpplExpr f =
  | x ->
    (smapAccumL_RtpplTop_RtpplExpr (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTop_RtpplExpr : all a. (a -> RtpplExpr -> a) -> a -> RtpplTop -> a
sem sfold_RtpplTop_RtpplExpr f acc =
  | x ->
    (smapAccumL_RtpplTop_RtpplExpr (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTop_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)) -> a -> RtpplTop -> (a, RtpplTop)
sem smapAccumL_RtpplTop_RtpplExprNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTop_RtpplExprNoIdent : (RtpplExprNoIdent -> RtpplExprNoIdent) -> RtpplTop -> RtpplTop
sem smap_RtpplTop_RtpplExprNoIdent f =
  | x ->
    (smapAccumL_RtpplTop_RtpplExprNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTop_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> a) -> a -> RtpplTop -> a
sem sfold_RtpplTop_RtpplExprNoIdent f acc =
  | x ->
    (smapAccumL_RtpplTop_RtpplExprNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTop_RtpplType : all a. (a -> RtpplType -> (a, RtpplType)) -> a -> RtpplTop -> (a, RtpplTop)
sem smapAccumL_RtpplTop_RtpplType f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTop_RtpplType : (RtpplType -> RtpplType) -> RtpplTop -> RtpplTop
sem smap_RtpplTop_RtpplType f =
  | x ->
    (smapAccumL_RtpplTop_RtpplType (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTop_RtpplType : all a. (a -> RtpplType -> a) -> a -> RtpplTop -> a
sem sfold_RtpplTop_RtpplType f acc =
  | x ->
    (smapAccumL_RtpplTop_RtpplType (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTop_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)) -> a -> RtpplTop -> (a, RtpplTop)
sem smapAccumL_RtpplTop_RtpplTypeNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTop_RtpplTypeNoIdent : (RtpplTypeNoIdent -> RtpplTypeNoIdent) -> RtpplTop -> RtpplTop
sem smap_RtpplTop_RtpplTypeNoIdent f =
  | x ->
    (smapAccumL_RtpplTop_RtpplTypeNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTop_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> a) -> a -> RtpplTop -> a
sem sfold_RtpplTop_RtpplTypeNoIdent f acc =
  | x ->
    (smapAccumL_RtpplTop_RtpplTypeNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTop_RtpplConst : all a. (a -> RtpplConst -> (a, RtpplConst)) -> a -> RtpplTop -> (a, RtpplTop)
sem smapAccumL_RtpplTop_RtpplConst f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTop_RtpplConst : (RtpplConst -> RtpplConst) -> RtpplTop -> RtpplTop
sem smap_RtpplTop_RtpplConst f =
  | x ->
    (smapAccumL_RtpplTop_RtpplConst (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTop_RtpplConst : all a. (a -> RtpplConst -> a) -> a -> RtpplTop -> a
sem sfold_RtpplTop_RtpplConst f acc =
  | x ->
    (smapAccumL_RtpplTop_RtpplConst (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTop_RtpplPort : all a. (a -> RtpplPort -> (a, RtpplPort)) -> a -> RtpplTop -> (a, RtpplTop)
sem smapAccumL_RtpplTop_RtpplPort f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTop_RtpplPort : (RtpplPort -> RtpplPort) -> RtpplTop -> RtpplTop
sem smap_RtpplTop_RtpplPort f =
  | x ->
    (smapAccumL_RtpplTop_RtpplPort (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTop_RtpplPort : all a. (a -> RtpplPort -> a) -> a -> RtpplTop -> a
sem sfold_RtpplTop_RtpplPort f acc =
  | x ->
    (smapAccumL_RtpplTop_RtpplPort (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTop_RtpplMain : all a. (a -> RtpplMain -> (a, RtpplMain)) -> a -> RtpplTop -> (a, RtpplTop)
sem smapAccumL_RtpplTop_RtpplMain f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTop_RtpplMain : (RtpplMain -> RtpplMain) -> RtpplTop -> RtpplTop
sem smap_RtpplTop_RtpplMain f =
  | x ->
    (smapAccumL_RtpplTop_RtpplMain (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTop_RtpplMain : all a. (a -> RtpplMain -> a) -> a -> RtpplTop -> a
sem sfold_RtpplTop_RtpplMain f acc =
  | x ->
    (smapAccumL_RtpplTop_RtpplMain (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTop_RtpplExt : all a. (a -> RtpplExt -> (a, RtpplExt)) -> a -> RtpplTop -> (a, RtpplTop)
sem smapAccumL_RtpplTop_RtpplExt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTop_RtpplExt : (RtpplExt -> RtpplExt) -> RtpplTop -> RtpplTop
sem smap_RtpplTop_RtpplExt f =
  | x ->
    (smapAccumL_RtpplTop_RtpplExt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTop_RtpplExt : all a. (a -> RtpplExt -> a) -> a -> RtpplTop -> a
sem sfold_RtpplTop_RtpplExt f acc =
  | x ->
    (smapAccumL_RtpplTop_RtpplExt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTop_RtpplTask : all a. (a -> RtpplTask -> (a, RtpplTask)) -> a -> RtpplTop -> (a, RtpplTop)
sem smapAccumL_RtpplTop_RtpplTask f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTop_RtpplTask : (RtpplTask -> RtpplTask) -> RtpplTop -> RtpplTop
sem smap_RtpplTop_RtpplTask f =
  | x ->
    (smapAccumL_RtpplTop_RtpplTask (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTop_RtpplTask : all a. (a -> RtpplTask -> a) -> a -> RtpplTop -> a
sem sfold_RtpplTop_RtpplTask f acc =
  | x ->
    (smapAccumL_RtpplTop_RtpplTask (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTop_RtpplConnection : all a. (a -> RtpplConnection -> (a, RtpplConnection)) -> a -> RtpplTop -> (a, RtpplTop)
sem smapAccumL_RtpplTop_RtpplConnection f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTop_RtpplConnection : (RtpplConnection -> RtpplConnection) -> RtpplTop -> RtpplTop
sem smap_RtpplTop_RtpplConnection f =
  | x ->
    (smapAccumL_RtpplTop_RtpplConnection (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTop_RtpplConnection : all a. (a -> RtpplConnection -> a) -> a -> RtpplTop -> a
sem sfold_RtpplTop_RtpplConnection f acc =
  | x ->
    (smapAccumL_RtpplTop_RtpplConnection (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTop_RtpplPortSpec : all a. (a -> RtpplPortSpec -> (a, RtpplPortSpec)) -> a -> RtpplTop -> (a, RtpplTop)
sem smapAccumL_RtpplTop_RtpplPortSpec f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTop_RtpplPortSpec : (RtpplPortSpec -> RtpplPortSpec) -> RtpplTop -> RtpplTop
sem smap_RtpplTop_RtpplPortSpec f =
  | x ->
    (smapAccumL_RtpplTop_RtpplPortSpec (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTop_RtpplPortSpec : all a. (a -> RtpplPortSpec -> a) -> a -> RtpplTop -> a
sem sfold_RtpplTop_RtpplPortSpec f acc =
  | x ->
    (smapAccumL_RtpplTop_RtpplPortSpec (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTopParams_RtpplProgram : all a. (a -> RtpplProgram -> (a, RtpplProgram)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem smapAccumL_RtpplTopParams_RtpplProgram f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTopParams_RtpplProgram : (RtpplProgram -> RtpplProgram) -> RtpplTopParams -> RtpplTopParams
sem smap_RtpplTopParams_RtpplProgram f =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplProgram (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTopParams_RtpplProgram : all a. (a -> RtpplProgram -> a) -> a -> RtpplTopParams -> a
sem sfold_RtpplTopParams_RtpplProgram f acc =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplProgram (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTopParams_RtpplTop : all a. (a -> RtpplTop -> (a, RtpplTop)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem smapAccumL_RtpplTopParams_RtpplTop f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTopParams_RtpplTop : (RtpplTop -> RtpplTop) -> RtpplTopParams -> RtpplTopParams
sem smap_RtpplTopParams_RtpplTop f =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplTop (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTopParams_RtpplTop : all a. (a -> RtpplTop -> a) -> a -> RtpplTopParams -> a
sem sfold_RtpplTopParams_RtpplTop f acc =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplTop (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTopParams_RtpplTopParams : all a. (a -> RtpplTopParams -> (a, RtpplTopParams)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem smapAccumL_RtpplTopParams_RtpplTopParams f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTopParams_RtpplTopParams : (RtpplTopParams -> RtpplTopParams) -> RtpplTopParams -> RtpplTopParams
sem smap_RtpplTopParams_RtpplTopParams f =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplTopParams (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTopParams_RtpplTopParams : all a. (a -> RtpplTopParams -> a) -> a -> RtpplTopParams -> a
sem sfold_RtpplTopParams_RtpplTopParams f acc =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplTopParams (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTopParams_RtpplStmt : all a. (a -> RtpplStmt -> (a, RtpplStmt)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem smapAccumL_RtpplTopParams_RtpplStmt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTopParams_RtpplStmt : (RtpplStmt -> RtpplStmt) -> RtpplTopParams -> RtpplTopParams
sem smap_RtpplTopParams_RtpplStmt f =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplStmt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTopParams_RtpplStmt : all a. (a -> RtpplStmt -> a) -> a -> RtpplTopParams -> a
sem sfold_RtpplTopParams_RtpplStmt f acc =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplStmt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTopParams_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem smapAccumL_RtpplTopParams_RtpplStmtNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTopParams_RtpplStmtNoIdent : (RtpplStmtNoIdent -> RtpplStmtNoIdent) -> RtpplTopParams -> RtpplTopParams
sem smap_RtpplTopParams_RtpplStmtNoIdent f =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplStmtNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTopParams_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> a) -> a -> RtpplTopParams -> a
sem sfold_RtpplTopParams_RtpplStmtNoIdent f acc =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplStmtNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTopParams_RtpplExpr : all a. (a -> RtpplExpr -> (a, RtpplExpr)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem smapAccumL_RtpplTopParams_RtpplExpr f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTopParams_RtpplExpr : (RtpplExpr -> RtpplExpr) -> RtpplTopParams -> RtpplTopParams
sem smap_RtpplTopParams_RtpplExpr f =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplExpr (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTopParams_RtpplExpr : all a. (a -> RtpplExpr -> a) -> a -> RtpplTopParams -> a
sem sfold_RtpplTopParams_RtpplExpr f acc =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplExpr (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTopParams_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem smapAccumL_RtpplTopParams_RtpplExprNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTopParams_RtpplExprNoIdent : (RtpplExprNoIdent -> RtpplExprNoIdent) -> RtpplTopParams -> RtpplTopParams
sem smap_RtpplTopParams_RtpplExprNoIdent f =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplExprNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTopParams_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> a) -> a -> RtpplTopParams -> a
sem sfold_RtpplTopParams_RtpplExprNoIdent f acc =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplExprNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTopParams_RtpplType : all a. (a -> RtpplType -> (a, RtpplType)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem smapAccumL_RtpplTopParams_RtpplType f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTopParams_RtpplType : (RtpplType -> RtpplType) -> RtpplTopParams -> RtpplTopParams
sem smap_RtpplTopParams_RtpplType f =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplType (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTopParams_RtpplType : all a. (a -> RtpplType -> a) -> a -> RtpplTopParams -> a
sem sfold_RtpplTopParams_RtpplType f acc =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplType (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTopParams_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem smapAccumL_RtpplTopParams_RtpplTypeNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTopParams_RtpplTypeNoIdent : (RtpplTypeNoIdent -> RtpplTypeNoIdent) -> RtpplTopParams -> RtpplTopParams
sem smap_RtpplTopParams_RtpplTypeNoIdent f =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplTypeNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTopParams_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> a) -> a -> RtpplTopParams -> a
sem sfold_RtpplTopParams_RtpplTypeNoIdent f acc =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplTypeNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTopParams_RtpplConst : all a. (a -> RtpplConst -> (a, RtpplConst)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem smapAccumL_RtpplTopParams_RtpplConst f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTopParams_RtpplConst : (RtpplConst -> RtpplConst) -> RtpplTopParams -> RtpplTopParams
sem smap_RtpplTopParams_RtpplConst f =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplConst (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTopParams_RtpplConst : all a. (a -> RtpplConst -> a) -> a -> RtpplTopParams -> a
sem sfold_RtpplTopParams_RtpplConst f acc =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplConst (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTopParams_RtpplPort : all a. (a -> RtpplPort -> (a, RtpplPort)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem smapAccumL_RtpplTopParams_RtpplPort f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTopParams_RtpplPort : (RtpplPort -> RtpplPort) -> RtpplTopParams -> RtpplTopParams
sem smap_RtpplTopParams_RtpplPort f =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplPort (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTopParams_RtpplPort : all a. (a -> RtpplPort -> a) -> a -> RtpplTopParams -> a
sem sfold_RtpplTopParams_RtpplPort f acc =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplPort (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTopParams_RtpplMain : all a. (a -> RtpplMain -> (a, RtpplMain)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem smapAccumL_RtpplTopParams_RtpplMain f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTopParams_RtpplMain : (RtpplMain -> RtpplMain) -> RtpplTopParams -> RtpplTopParams
sem smap_RtpplTopParams_RtpplMain f =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplMain (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTopParams_RtpplMain : all a. (a -> RtpplMain -> a) -> a -> RtpplTopParams -> a
sem sfold_RtpplTopParams_RtpplMain f acc =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplMain (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTopParams_RtpplExt : all a. (a -> RtpplExt -> (a, RtpplExt)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem smapAccumL_RtpplTopParams_RtpplExt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTopParams_RtpplExt : (RtpplExt -> RtpplExt) -> RtpplTopParams -> RtpplTopParams
sem smap_RtpplTopParams_RtpplExt f =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplExt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTopParams_RtpplExt : all a. (a -> RtpplExt -> a) -> a -> RtpplTopParams -> a
sem sfold_RtpplTopParams_RtpplExt f acc =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplExt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTopParams_RtpplTask : all a. (a -> RtpplTask -> (a, RtpplTask)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem smapAccumL_RtpplTopParams_RtpplTask f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTopParams_RtpplTask : (RtpplTask -> RtpplTask) -> RtpplTopParams -> RtpplTopParams
sem smap_RtpplTopParams_RtpplTask f =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplTask (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTopParams_RtpplTask : all a. (a -> RtpplTask -> a) -> a -> RtpplTopParams -> a
sem sfold_RtpplTopParams_RtpplTask f acc =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplTask (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTopParams_RtpplConnection : all a. (a -> RtpplConnection -> (a, RtpplConnection)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem smapAccumL_RtpplTopParams_RtpplConnection f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTopParams_RtpplConnection : (RtpplConnection -> RtpplConnection) -> RtpplTopParams -> RtpplTopParams
sem smap_RtpplTopParams_RtpplConnection f =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplConnection (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTopParams_RtpplConnection : all a. (a -> RtpplConnection -> a) -> a -> RtpplTopParams -> a
sem sfold_RtpplTopParams_RtpplConnection f acc =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplConnection (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTopParams_RtpplPortSpec : all a. (a -> RtpplPortSpec -> (a, RtpplPortSpec)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem smapAccumL_RtpplTopParams_RtpplPortSpec f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTopParams_RtpplPortSpec : (RtpplPortSpec -> RtpplPortSpec) -> RtpplTopParams -> RtpplTopParams
sem smap_RtpplTopParams_RtpplPortSpec f =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplPortSpec (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTopParams_RtpplPortSpec : all a. (a -> RtpplPortSpec -> a) -> a -> RtpplTopParams -> a
sem sfold_RtpplTopParams_RtpplPortSpec f acc =
  | x ->
    (smapAccumL_RtpplTopParams_RtpplPortSpec (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmt_RtpplProgram : all a. (a -> RtpplProgram -> (a, RtpplProgram)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem smapAccumL_RtpplStmt_RtpplProgram f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmt_RtpplProgram : (RtpplProgram -> RtpplProgram) -> RtpplStmt -> RtpplStmt
sem smap_RtpplStmt_RtpplProgram f =
  | x ->
    (smapAccumL_RtpplStmt_RtpplProgram (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmt_RtpplProgram : all a. (a -> RtpplProgram -> a) -> a -> RtpplStmt -> a
sem sfold_RtpplStmt_RtpplProgram f acc =
  | x ->
    (smapAccumL_RtpplStmt_RtpplProgram (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmt_RtpplTop : all a. (a -> RtpplTop -> (a, RtpplTop)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem smapAccumL_RtpplStmt_RtpplTop f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmt_RtpplTop : (RtpplTop -> RtpplTop) -> RtpplStmt -> RtpplStmt
sem smap_RtpplStmt_RtpplTop f =
  | x ->
    (smapAccumL_RtpplStmt_RtpplTop (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmt_RtpplTop : all a. (a -> RtpplTop -> a) -> a -> RtpplStmt -> a
sem sfold_RtpplStmt_RtpplTop f acc =
  | x ->
    (smapAccumL_RtpplStmt_RtpplTop (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmt_RtpplTopParams : all a. (a -> RtpplTopParams -> (a, RtpplTopParams)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem smapAccumL_RtpplStmt_RtpplTopParams f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmt_RtpplTopParams : (RtpplTopParams -> RtpplTopParams) -> RtpplStmt -> RtpplStmt
sem smap_RtpplStmt_RtpplTopParams f =
  | x ->
    (smapAccumL_RtpplStmt_RtpplTopParams (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmt_RtpplTopParams : all a. (a -> RtpplTopParams -> a) -> a -> RtpplStmt -> a
sem sfold_RtpplStmt_RtpplTopParams f acc =
  | x ->
    (smapAccumL_RtpplStmt_RtpplTopParams (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmt_RtpplStmt : all a. (a -> RtpplStmt -> (a, RtpplStmt)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem smapAccumL_RtpplStmt_RtpplStmt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmt_RtpplStmt : (RtpplStmt -> RtpplStmt) -> RtpplStmt -> RtpplStmt
sem smap_RtpplStmt_RtpplStmt f =
  | x ->
    (smapAccumL_RtpplStmt_RtpplStmt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmt_RtpplStmt : all a. (a -> RtpplStmt -> a) -> a -> RtpplStmt -> a
sem sfold_RtpplStmt_RtpplStmt f acc =
  | x ->
    (smapAccumL_RtpplStmt_RtpplStmt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmt_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem smapAccumL_RtpplStmt_RtpplStmtNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmt_RtpplStmtNoIdent : (RtpplStmtNoIdent -> RtpplStmtNoIdent) -> RtpplStmt -> RtpplStmt
sem smap_RtpplStmt_RtpplStmtNoIdent f =
  | x ->
    (smapAccumL_RtpplStmt_RtpplStmtNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmt_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> a) -> a -> RtpplStmt -> a
sem sfold_RtpplStmt_RtpplStmtNoIdent f acc =
  | x ->
    (smapAccumL_RtpplStmt_RtpplStmtNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmt_RtpplExpr : all a. (a -> RtpplExpr -> (a, RtpplExpr)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem smapAccumL_RtpplStmt_RtpplExpr f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmt_RtpplExpr : (RtpplExpr -> RtpplExpr) -> RtpplStmt -> RtpplStmt
sem smap_RtpplStmt_RtpplExpr f =
  | x ->
    (smapAccumL_RtpplStmt_RtpplExpr (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmt_RtpplExpr : all a. (a -> RtpplExpr -> a) -> a -> RtpplStmt -> a
sem sfold_RtpplStmt_RtpplExpr f acc =
  | x ->
    (smapAccumL_RtpplStmt_RtpplExpr (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmt_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem smapAccumL_RtpplStmt_RtpplExprNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmt_RtpplExprNoIdent : (RtpplExprNoIdent -> RtpplExprNoIdent) -> RtpplStmt -> RtpplStmt
sem smap_RtpplStmt_RtpplExprNoIdent f =
  | x ->
    (smapAccumL_RtpplStmt_RtpplExprNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmt_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> a) -> a -> RtpplStmt -> a
sem sfold_RtpplStmt_RtpplExprNoIdent f acc =
  | x ->
    (smapAccumL_RtpplStmt_RtpplExprNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmt_RtpplType : all a. (a -> RtpplType -> (a, RtpplType)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem smapAccumL_RtpplStmt_RtpplType f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmt_RtpplType : (RtpplType -> RtpplType) -> RtpplStmt -> RtpplStmt
sem smap_RtpplStmt_RtpplType f =
  | x ->
    (smapAccumL_RtpplStmt_RtpplType (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmt_RtpplType : all a. (a -> RtpplType -> a) -> a -> RtpplStmt -> a
sem sfold_RtpplStmt_RtpplType f acc =
  | x ->
    (smapAccumL_RtpplStmt_RtpplType (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmt_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem smapAccumL_RtpplStmt_RtpplTypeNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmt_RtpplTypeNoIdent : (RtpplTypeNoIdent -> RtpplTypeNoIdent) -> RtpplStmt -> RtpplStmt
sem smap_RtpplStmt_RtpplTypeNoIdent f =
  | x ->
    (smapAccumL_RtpplStmt_RtpplTypeNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmt_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> a) -> a -> RtpplStmt -> a
sem sfold_RtpplStmt_RtpplTypeNoIdent f acc =
  | x ->
    (smapAccumL_RtpplStmt_RtpplTypeNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmt_RtpplConst : all a. (a -> RtpplConst -> (a, RtpplConst)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem smapAccumL_RtpplStmt_RtpplConst f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmt_RtpplConst : (RtpplConst -> RtpplConst) -> RtpplStmt -> RtpplStmt
sem smap_RtpplStmt_RtpplConst f =
  | x ->
    (smapAccumL_RtpplStmt_RtpplConst (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmt_RtpplConst : all a. (a -> RtpplConst -> a) -> a -> RtpplStmt -> a
sem sfold_RtpplStmt_RtpplConst f acc =
  | x ->
    (smapAccumL_RtpplStmt_RtpplConst (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmt_RtpplPort : all a. (a -> RtpplPort -> (a, RtpplPort)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem smapAccumL_RtpplStmt_RtpplPort f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmt_RtpplPort : (RtpplPort -> RtpplPort) -> RtpplStmt -> RtpplStmt
sem smap_RtpplStmt_RtpplPort f =
  | x ->
    (smapAccumL_RtpplStmt_RtpplPort (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmt_RtpplPort : all a. (a -> RtpplPort -> a) -> a -> RtpplStmt -> a
sem sfold_RtpplStmt_RtpplPort f acc =
  | x ->
    (smapAccumL_RtpplStmt_RtpplPort (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmt_RtpplMain : all a. (a -> RtpplMain -> (a, RtpplMain)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem smapAccumL_RtpplStmt_RtpplMain f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmt_RtpplMain : (RtpplMain -> RtpplMain) -> RtpplStmt -> RtpplStmt
sem smap_RtpplStmt_RtpplMain f =
  | x ->
    (smapAccumL_RtpplStmt_RtpplMain (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmt_RtpplMain : all a. (a -> RtpplMain -> a) -> a -> RtpplStmt -> a
sem sfold_RtpplStmt_RtpplMain f acc =
  | x ->
    (smapAccumL_RtpplStmt_RtpplMain (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmt_RtpplExt : all a. (a -> RtpplExt -> (a, RtpplExt)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem smapAccumL_RtpplStmt_RtpplExt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmt_RtpplExt : (RtpplExt -> RtpplExt) -> RtpplStmt -> RtpplStmt
sem smap_RtpplStmt_RtpplExt f =
  | x ->
    (smapAccumL_RtpplStmt_RtpplExt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmt_RtpplExt : all a. (a -> RtpplExt -> a) -> a -> RtpplStmt -> a
sem sfold_RtpplStmt_RtpplExt f acc =
  | x ->
    (smapAccumL_RtpplStmt_RtpplExt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmt_RtpplTask : all a. (a -> RtpplTask -> (a, RtpplTask)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem smapAccumL_RtpplStmt_RtpplTask f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmt_RtpplTask : (RtpplTask -> RtpplTask) -> RtpplStmt -> RtpplStmt
sem smap_RtpplStmt_RtpplTask f =
  | x ->
    (smapAccumL_RtpplStmt_RtpplTask (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmt_RtpplTask : all a. (a -> RtpplTask -> a) -> a -> RtpplStmt -> a
sem sfold_RtpplStmt_RtpplTask f acc =
  | x ->
    (smapAccumL_RtpplStmt_RtpplTask (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmt_RtpplConnection : all a. (a -> RtpplConnection -> (a, RtpplConnection)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem smapAccumL_RtpplStmt_RtpplConnection f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmt_RtpplConnection : (RtpplConnection -> RtpplConnection) -> RtpplStmt -> RtpplStmt
sem smap_RtpplStmt_RtpplConnection f =
  | x ->
    (smapAccumL_RtpplStmt_RtpplConnection (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmt_RtpplConnection : all a. (a -> RtpplConnection -> a) -> a -> RtpplStmt -> a
sem sfold_RtpplStmt_RtpplConnection f acc =
  | x ->
    (smapAccumL_RtpplStmt_RtpplConnection (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmt_RtpplPortSpec : all a. (a -> RtpplPortSpec -> (a, RtpplPortSpec)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem smapAccumL_RtpplStmt_RtpplPortSpec f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmt_RtpplPortSpec : (RtpplPortSpec -> RtpplPortSpec) -> RtpplStmt -> RtpplStmt
sem smap_RtpplStmt_RtpplPortSpec f =
  | x ->
    (smapAccumL_RtpplStmt_RtpplPortSpec (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmt_RtpplPortSpec : all a. (a -> RtpplPortSpec -> a) -> a -> RtpplStmt -> a
sem sfold_RtpplStmt_RtpplPortSpec f acc =
  | x ->
    (smapAccumL_RtpplStmt_RtpplPortSpec (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmtNoIdent_RtpplProgram : all a. (a -> RtpplProgram -> (a, RtpplProgram)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem smapAccumL_RtpplStmtNoIdent_RtpplProgram f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmtNoIdent_RtpplProgram : (RtpplProgram -> RtpplProgram) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem smap_RtpplStmtNoIdent_RtpplProgram f =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplProgram (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmtNoIdent_RtpplProgram : all a. (a -> RtpplProgram -> a) -> a -> RtpplStmtNoIdent -> a
sem sfold_RtpplStmtNoIdent_RtpplProgram f acc =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplProgram (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmtNoIdent_RtpplTop : all a. (a -> RtpplTop -> (a, RtpplTop)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem smapAccumL_RtpplStmtNoIdent_RtpplTop f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmtNoIdent_RtpplTop : (RtpplTop -> RtpplTop) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem smap_RtpplStmtNoIdent_RtpplTop f =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplTop (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmtNoIdent_RtpplTop : all a. (a -> RtpplTop -> a) -> a -> RtpplStmtNoIdent -> a
sem sfold_RtpplStmtNoIdent_RtpplTop f acc =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplTop (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmtNoIdent_RtpplTopParams : all a. (a -> RtpplTopParams -> (a, RtpplTopParams)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem smapAccumL_RtpplStmtNoIdent_RtpplTopParams f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmtNoIdent_RtpplTopParams : (RtpplTopParams -> RtpplTopParams) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem smap_RtpplStmtNoIdent_RtpplTopParams f =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplTopParams (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmtNoIdent_RtpplTopParams : all a. (a -> RtpplTopParams -> a) -> a -> RtpplStmtNoIdent -> a
sem sfold_RtpplStmtNoIdent_RtpplTopParams f acc =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplTopParams (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmtNoIdent_RtpplStmt : all a. (a -> RtpplStmt -> (a, RtpplStmt)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem smapAccumL_RtpplStmtNoIdent_RtpplStmt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmtNoIdent_RtpplStmt : (RtpplStmt -> RtpplStmt) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem smap_RtpplStmtNoIdent_RtpplStmt f =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplStmt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmtNoIdent_RtpplStmt : all a. (a -> RtpplStmt -> a) -> a -> RtpplStmtNoIdent -> a
sem sfold_RtpplStmtNoIdent_RtpplStmt f acc =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplStmt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmtNoIdent_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem smapAccumL_RtpplStmtNoIdent_RtpplStmtNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmtNoIdent_RtpplStmtNoIdent : (RtpplStmtNoIdent -> RtpplStmtNoIdent) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem smap_RtpplStmtNoIdent_RtpplStmtNoIdent f =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplStmtNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmtNoIdent_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> a) -> a -> RtpplStmtNoIdent -> a
sem sfold_RtpplStmtNoIdent_RtpplStmtNoIdent f acc =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplStmtNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmtNoIdent_RtpplExpr : all a. (a -> RtpplExpr -> (a, RtpplExpr)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem smapAccumL_RtpplStmtNoIdent_RtpplExpr f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmtNoIdent_RtpplExpr : (RtpplExpr -> RtpplExpr) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem smap_RtpplStmtNoIdent_RtpplExpr f =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplExpr (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmtNoIdent_RtpplExpr : all a. (a -> RtpplExpr -> a) -> a -> RtpplStmtNoIdent -> a
sem sfold_RtpplStmtNoIdent_RtpplExpr f acc =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplExpr (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmtNoIdent_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem smapAccumL_RtpplStmtNoIdent_RtpplExprNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmtNoIdent_RtpplExprNoIdent : (RtpplExprNoIdent -> RtpplExprNoIdent) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem smap_RtpplStmtNoIdent_RtpplExprNoIdent f =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplExprNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmtNoIdent_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> a) -> a -> RtpplStmtNoIdent -> a
sem sfold_RtpplStmtNoIdent_RtpplExprNoIdent f acc =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplExprNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmtNoIdent_RtpplType : all a. (a -> RtpplType -> (a, RtpplType)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem smapAccumL_RtpplStmtNoIdent_RtpplType f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmtNoIdent_RtpplType : (RtpplType -> RtpplType) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem smap_RtpplStmtNoIdent_RtpplType f =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplType (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmtNoIdent_RtpplType : all a. (a -> RtpplType -> a) -> a -> RtpplStmtNoIdent -> a
sem sfold_RtpplStmtNoIdent_RtpplType f acc =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplType (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmtNoIdent_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem smapAccumL_RtpplStmtNoIdent_RtpplTypeNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmtNoIdent_RtpplTypeNoIdent : (RtpplTypeNoIdent -> RtpplTypeNoIdent) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem smap_RtpplStmtNoIdent_RtpplTypeNoIdent f =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplTypeNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmtNoIdent_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> a) -> a -> RtpplStmtNoIdent -> a
sem sfold_RtpplStmtNoIdent_RtpplTypeNoIdent f acc =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplTypeNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmtNoIdent_RtpplConst : all a. (a -> RtpplConst -> (a, RtpplConst)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem smapAccumL_RtpplStmtNoIdent_RtpplConst f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmtNoIdent_RtpplConst : (RtpplConst -> RtpplConst) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem smap_RtpplStmtNoIdent_RtpplConst f =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplConst (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmtNoIdent_RtpplConst : all a. (a -> RtpplConst -> a) -> a -> RtpplStmtNoIdent -> a
sem sfold_RtpplStmtNoIdent_RtpplConst f acc =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplConst (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmtNoIdent_RtpplPort : all a. (a -> RtpplPort -> (a, RtpplPort)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem smapAccumL_RtpplStmtNoIdent_RtpplPort f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmtNoIdent_RtpplPort : (RtpplPort -> RtpplPort) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem smap_RtpplStmtNoIdent_RtpplPort f =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplPort (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmtNoIdent_RtpplPort : all a. (a -> RtpplPort -> a) -> a -> RtpplStmtNoIdent -> a
sem sfold_RtpplStmtNoIdent_RtpplPort f acc =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplPort (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmtNoIdent_RtpplMain : all a. (a -> RtpplMain -> (a, RtpplMain)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem smapAccumL_RtpplStmtNoIdent_RtpplMain f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmtNoIdent_RtpplMain : (RtpplMain -> RtpplMain) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem smap_RtpplStmtNoIdent_RtpplMain f =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplMain (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmtNoIdent_RtpplMain : all a. (a -> RtpplMain -> a) -> a -> RtpplStmtNoIdent -> a
sem sfold_RtpplStmtNoIdent_RtpplMain f acc =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplMain (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmtNoIdent_RtpplExt : all a. (a -> RtpplExt -> (a, RtpplExt)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem smapAccumL_RtpplStmtNoIdent_RtpplExt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmtNoIdent_RtpplExt : (RtpplExt -> RtpplExt) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem smap_RtpplStmtNoIdent_RtpplExt f =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplExt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmtNoIdent_RtpplExt : all a. (a -> RtpplExt -> a) -> a -> RtpplStmtNoIdent -> a
sem sfold_RtpplStmtNoIdent_RtpplExt f acc =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplExt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmtNoIdent_RtpplTask : all a. (a -> RtpplTask -> (a, RtpplTask)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem smapAccumL_RtpplStmtNoIdent_RtpplTask f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmtNoIdent_RtpplTask : (RtpplTask -> RtpplTask) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem smap_RtpplStmtNoIdent_RtpplTask f =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplTask (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmtNoIdent_RtpplTask : all a. (a -> RtpplTask -> a) -> a -> RtpplStmtNoIdent -> a
sem sfold_RtpplStmtNoIdent_RtpplTask f acc =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplTask (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmtNoIdent_RtpplConnection : all a. (a -> RtpplConnection -> (a, RtpplConnection)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem smapAccumL_RtpplStmtNoIdent_RtpplConnection f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmtNoIdent_RtpplConnection : (RtpplConnection -> RtpplConnection) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem smap_RtpplStmtNoIdent_RtpplConnection f =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplConnection (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmtNoIdent_RtpplConnection : all a. (a -> RtpplConnection -> a) -> a -> RtpplStmtNoIdent -> a
sem sfold_RtpplStmtNoIdent_RtpplConnection f acc =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplConnection (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplStmtNoIdent_RtpplPortSpec : all a. (a -> RtpplPortSpec -> (a, RtpplPortSpec)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem smapAccumL_RtpplStmtNoIdent_RtpplPortSpec f acc =
  | x ->
    (acc, x)
  sem smap_RtpplStmtNoIdent_RtpplPortSpec : (RtpplPortSpec -> RtpplPortSpec) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem smap_RtpplStmtNoIdent_RtpplPortSpec f =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplPortSpec (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplStmtNoIdent_RtpplPortSpec : all a. (a -> RtpplPortSpec -> a) -> a -> RtpplStmtNoIdent -> a
sem sfold_RtpplStmtNoIdent_RtpplPortSpec f acc =
  | x ->
    (smapAccumL_RtpplStmtNoIdent_RtpplPortSpec (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExpr_RtpplProgram : all a. (a -> RtpplProgram -> (a, RtpplProgram)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem smapAccumL_RtpplExpr_RtpplProgram f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExpr_RtpplProgram : (RtpplProgram -> RtpplProgram) -> RtpplExpr -> RtpplExpr
sem smap_RtpplExpr_RtpplProgram f =
  | x ->
    (smapAccumL_RtpplExpr_RtpplProgram (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExpr_RtpplProgram : all a. (a -> RtpplProgram -> a) -> a -> RtpplExpr -> a
sem sfold_RtpplExpr_RtpplProgram f acc =
  | x ->
    (smapAccumL_RtpplExpr_RtpplProgram (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExpr_RtpplTop : all a. (a -> RtpplTop -> (a, RtpplTop)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem smapAccumL_RtpplExpr_RtpplTop f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExpr_RtpplTop : (RtpplTop -> RtpplTop) -> RtpplExpr -> RtpplExpr
sem smap_RtpplExpr_RtpplTop f =
  | x ->
    (smapAccumL_RtpplExpr_RtpplTop (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExpr_RtpplTop : all a. (a -> RtpplTop -> a) -> a -> RtpplExpr -> a
sem sfold_RtpplExpr_RtpplTop f acc =
  | x ->
    (smapAccumL_RtpplExpr_RtpplTop (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExpr_RtpplTopParams : all a. (a -> RtpplTopParams -> (a, RtpplTopParams)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem smapAccumL_RtpplExpr_RtpplTopParams f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExpr_RtpplTopParams : (RtpplTopParams -> RtpplTopParams) -> RtpplExpr -> RtpplExpr
sem smap_RtpplExpr_RtpplTopParams f =
  | x ->
    (smapAccumL_RtpplExpr_RtpplTopParams (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExpr_RtpplTopParams : all a. (a -> RtpplTopParams -> a) -> a -> RtpplExpr -> a
sem sfold_RtpplExpr_RtpplTopParams f acc =
  | x ->
    (smapAccumL_RtpplExpr_RtpplTopParams (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExpr_RtpplStmt : all a. (a -> RtpplStmt -> (a, RtpplStmt)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem smapAccumL_RtpplExpr_RtpplStmt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExpr_RtpplStmt : (RtpplStmt -> RtpplStmt) -> RtpplExpr -> RtpplExpr
sem smap_RtpplExpr_RtpplStmt f =
  | x ->
    (smapAccumL_RtpplExpr_RtpplStmt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExpr_RtpplStmt : all a. (a -> RtpplStmt -> a) -> a -> RtpplExpr -> a
sem sfold_RtpplExpr_RtpplStmt f acc =
  | x ->
    (smapAccumL_RtpplExpr_RtpplStmt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExpr_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem smapAccumL_RtpplExpr_RtpplStmtNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExpr_RtpplStmtNoIdent : (RtpplStmtNoIdent -> RtpplStmtNoIdent) -> RtpplExpr -> RtpplExpr
sem smap_RtpplExpr_RtpplStmtNoIdent f =
  | x ->
    (smapAccumL_RtpplExpr_RtpplStmtNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExpr_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> a) -> a -> RtpplExpr -> a
sem sfold_RtpplExpr_RtpplStmtNoIdent f acc =
  | x ->
    (smapAccumL_RtpplExpr_RtpplStmtNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExpr_RtpplExpr : all a. (a -> RtpplExpr -> (a, RtpplExpr)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExpr_RtpplExpr : (RtpplExpr -> RtpplExpr) -> RtpplExpr -> RtpplExpr
sem smap_RtpplExpr_RtpplExpr f =
  | x ->
    (smapAccumL_RtpplExpr_RtpplExpr (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExpr_RtpplExpr : all a. (a -> RtpplExpr -> a) -> a -> RtpplExpr -> a
sem sfold_RtpplExpr_RtpplExpr f acc =
  | x ->
    (smapAccumL_RtpplExpr_RtpplExpr (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExpr_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem smapAccumL_RtpplExpr_RtpplExprNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExpr_RtpplExprNoIdent : (RtpplExprNoIdent -> RtpplExprNoIdent) -> RtpplExpr -> RtpplExpr
sem smap_RtpplExpr_RtpplExprNoIdent f =
  | x ->
    (smapAccumL_RtpplExpr_RtpplExprNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExpr_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> a) -> a -> RtpplExpr -> a
sem sfold_RtpplExpr_RtpplExprNoIdent f acc =
  | x ->
    (smapAccumL_RtpplExpr_RtpplExprNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExpr_RtpplType : all a. (a -> RtpplType -> (a, RtpplType)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem smapAccumL_RtpplExpr_RtpplType f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExpr_RtpplType : (RtpplType -> RtpplType) -> RtpplExpr -> RtpplExpr
sem smap_RtpplExpr_RtpplType f =
  | x ->
    (smapAccumL_RtpplExpr_RtpplType (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExpr_RtpplType : all a. (a -> RtpplType -> a) -> a -> RtpplExpr -> a
sem sfold_RtpplExpr_RtpplType f acc =
  | x ->
    (smapAccumL_RtpplExpr_RtpplType (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExpr_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem smapAccumL_RtpplExpr_RtpplTypeNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExpr_RtpplTypeNoIdent : (RtpplTypeNoIdent -> RtpplTypeNoIdent) -> RtpplExpr -> RtpplExpr
sem smap_RtpplExpr_RtpplTypeNoIdent f =
  | x ->
    (smapAccumL_RtpplExpr_RtpplTypeNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExpr_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> a) -> a -> RtpplExpr -> a
sem sfold_RtpplExpr_RtpplTypeNoIdent f acc =
  | x ->
    (smapAccumL_RtpplExpr_RtpplTypeNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExpr_RtpplConst : all a. (a -> RtpplConst -> (a, RtpplConst)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem smapAccumL_RtpplExpr_RtpplConst f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExpr_RtpplConst : (RtpplConst -> RtpplConst) -> RtpplExpr -> RtpplExpr
sem smap_RtpplExpr_RtpplConst f =
  | x ->
    (smapAccumL_RtpplExpr_RtpplConst (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExpr_RtpplConst : all a. (a -> RtpplConst -> a) -> a -> RtpplExpr -> a
sem sfold_RtpplExpr_RtpplConst f acc =
  | x ->
    (smapAccumL_RtpplExpr_RtpplConst (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExpr_RtpplPort : all a. (a -> RtpplPort -> (a, RtpplPort)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem smapAccumL_RtpplExpr_RtpplPort f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExpr_RtpplPort : (RtpplPort -> RtpplPort) -> RtpplExpr -> RtpplExpr
sem smap_RtpplExpr_RtpplPort f =
  | x ->
    (smapAccumL_RtpplExpr_RtpplPort (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExpr_RtpplPort : all a. (a -> RtpplPort -> a) -> a -> RtpplExpr -> a
sem sfold_RtpplExpr_RtpplPort f acc =
  | x ->
    (smapAccumL_RtpplExpr_RtpplPort (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExpr_RtpplMain : all a. (a -> RtpplMain -> (a, RtpplMain)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem smapAccumL_RtpplExpr_RtpplMain f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExpr_RtpplMain : (RtpplMain -> RtpplMain) -> RtpplExpr -> RtpplExpr
sem smap_RtpplExpr_RtpplMain f =
  | x ->
    (smapAccumL_RtpplExpr_RtpplMain (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExpr_RtpplMain : all a. (a -> RtpplMain -> a) -> a -> RtpplExpr -> a
sem sfold_RtpplExpr_RtpplMain f acc =
  | x ->
    (smapAccumL_RtpplExpr_RtpplMain (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExpr_RtpplExt : all a. (a -> RtpplExt -> (a, RtpplExt)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem smapAccumL_RtpplExpr_RtpplExt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExpr_RtpplExt : (RtpplExt -> RtpplExt) -> RtpplExpr -> RtpplExpr
sem smap_RtpplExpr_RtpplExt f =
  | x ->
    (smapAccumL_RtpplExpr_RtpplExt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExpr_RtpplExt : all a. (a -> RtpplExt -> a) -> a -> RtpplExpr -> a
sem sfold_RtpplExpr_RtpplExt f acc =
  | x ->
    (smapAccumL_RtpplExpr_RtpplExt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExpr_RtpplTask : all a. (a -> RtpplTask -> (a, RtpplTask)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem smapAccumL_RtpplExpr_RtpplTask f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExpr_RtpplTask : (RtpplTask -> RtpplTask) -> RtpplExpr -> RtpplExpr
sem smap_RtpplExpr_RtpplTask f =
  | x ->
    (smapAccumL_RtpplExpr_RtpplTask (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExpr_RtpplTask : all a. (a -> RtpplTask -> a) -> a -> RtpplExpr -> a
sem sfold_RtpplExpr_RtpplTask f acc =
  | x ->
    (smapAccumL_RtpplExpr_RtpplTask (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExpr_RtpplConnection : all a. (a -> RtpplConnection -> (a, RtpplConnection)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem smapAccumL_RtpplExpr_RtpplConnection f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExpr_RtpplConnection : (RtpplConnection -> RtpplConnection) -> RtpplExpr -> RtpplExpr
sem smap_RtpplExpr_RtpplConnection f =
  | x ->
    (smapAccumL_RtpplExpr_RtpplConnection (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExpr_RtpplConnection : all a. (a -> RtpplConnection -> a) -> a -> RtpplExpr -> a
sem sfold_RtpplExpr_RtpplConnection f acc =
  | x ->
    (smapAccumL_RtpplExpr_RtpplConnection (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExpr_RtpplPortSpec : all a. (a -> RtpplPortSpec -> (a, RtpplPortSpec)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem smapAccumL_RtpplExpr_RtpplPortSpec f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExpr_RtpplPortSpec : (RtpplPortSpec -> RtpplPortSpec) -> RtpplExpr -> RtpplExpr
sem smap_RtpplExpr_RtpplPortSpec f =
  | x ->
    (smapAccumL_RtpplExpr_RtpplPortSpec (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExpr_RtpplPortSpec : all a. (a -> RtpplPortSpec -> a) -> a -> RtpplExpr -> a
sem sfold_RtpplExpr_RtpplPortSpec f acc =
  | x ->
    (smapAccumL_RtpplExpr_RtpplPortSpec (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExprNoIdent_RtpplProgram : all a. (a -> RtpplProgram -> (a, RtpplProgram)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem smapAccumL_RtpplExprNoIdent_RtpplProgram f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExprNoIdent_RtpplProgram : (RtpplProgram -> RtpplProgram) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem smap_RtpplExprNoIdent_RtpplProgram f =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplProgram (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExprNoIdent_RtpplProgram : all a. (a -> RtpplProgram -> a) -> a -> RtpplExprNoIdent -> a
sem sfold_RtpplExprNoIdent_RtpplProgram f acc =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplProgram (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExprNoIdent_RtpplTop : all a. (a -> RtpplTop -> (a, RtpplTop)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem smapAccumL_RtpplExprNoIdent_RtpplTop f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExprNoIdent_RtpplTop : (RtpplTop -> RtpplTop) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem smap_RtpplExprNoIdent_RtpplTop f =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplTop (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExprNoIdent_RtpplTop : all a. (a -> RtpplTop -> a) -> a -> RtpplExprNoIdent -> a
sem sfold_RtpplExprNoIdent_RtpplTop f acc =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplTop (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExprNoIdent_RtpplTopParams : all a. (a -> RtpplTopParams -> (a, RtpplTopParams)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem smapAccumL_RtpplExprNoIdent_RtpplTopParams f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExprNoIdent_RtpplTopParams : (RtpplTopParams -> RtpplTopParams) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem smap_RtpplExprNoIdent_RtpplTopParams f =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplTopParams (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExprNoIdent_RtpplTopParams : all a. (a -> RtpplTopParams -> a) -> a -> RtpplExprNoIdent -> a
sem sfold_RtpplExprNoIdent_RtpplTopParams f acc =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplTopParams (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExprNoIdent_RtpplStmt : all a. (a -> RtpplStmt -> (a, RtpplStmt)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem smapAccumL_RtpplExprNoIdent_RtpplStmt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExprNoIdent_RtpplStmt : (RtpplStmt -> RtpplStmt) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem smap_RtpplExprNoIdent_RtpplStmt f =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplStmt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExprNoIdent_RtpplStmt : all a. (a -> RtpplStmt -> a) -> a -> RtpplExprNoIdent -> a
sem sfold_RtpplExprNoIdent_RtpplStmt f acc =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplStmt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExprNoIdent_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem smapAccumL_RtpplExprNoIdent_RtpplStmtNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExprNoIdent_RtpplStmtNoIdent : (RtpplStmtNoIdent -> RtpplStmtNoIdent) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem smap_RtpplExprNoIdent_RtpplStmtNoIdent f =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplStmtNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExprNoIdent_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> a) -> a -> RtpplExprNoIdent -> a
sem sfold_RtpplExprNoIdent_RtpplStmtNoIdent f acc =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplStmtNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExprNoIdent_RtpplExpr : all a. (a -> RtpplExpr -> (a, RtpplExpr)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem smapAccumL_RtpplExprNoIdent_RtpplExpr f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExprNoIdent_RtpplExpr : (RtpplExpr -> RtpplExpr) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem smap_RtpplExprNoIdent_RtpplExpr f =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplExpr (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExprNoIdent_RtpplExpr : all a. (a -> RtpplExpr -> a) -> a -> RtpplExprNoIdent -> a
sem sfold_RtpplExprNoIdent_RtpplExpr f acc =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplExpr (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExprNoIdent_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem smapAccumL_RtpplExprNoIdent_RtpplExprNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExprNoIdent_RtpplExprNoIdent : (RtpplExprNoIdent -> RtpplExprNoIdent) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem smap_RtpplExprNoIdent_RtpplExprNoIdent f =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplExprNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExprNoIdent_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> a) -> a -> RtpplExprNoIdent -> a
sem sfold_RtpplExprNoIdent_RtpplExprNoIdent f acc =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplExprNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExprNoIdent_RtpplType : all a. (a -> RtpplType -> (a, RtpplType)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem smapAccumL_RtpplExprNoIdent_RtpplType f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExprNoIdent_RtpplType : (RtpplType -> RtpplType) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem smap_RtpplExprNoIdent_RtpplType f =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplType (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExprNoIdent_RtpplType : all a. (a -> RtpplType -> a) -> a -> RtpplExprNoIdent -> a
sem sfold_RtpplExprNoIdent_RtpplType f acc =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplType (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExprNoIdent_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem smapAccumL_RtpplExprNoIdent_RtpplTypeNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExprNoIdent_RtpplTypeNoIdent : (RtpplTypeNoIdent -> RtpplTypeNoIdent) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem smap_RtpplExprNoIdent_RtpplTypeNoIdent f =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplTypeNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExprNoIdent_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> a) -> a -> RtpplExprNoIdent -> a
sem sfold_RtpplExprNoIdent_RtpplTypeNoIdent f acc =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplTypeNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExprNoIdent_RtpplConst : all a. (a -> RtpplConst -> (a, RtpplConst)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem smapAccumL_RtpplExprNoIdent_RtpplConst f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExprNoIdent_RtpplConst : (RtpplConst -> RtpplConst) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem smap_RtpplExprNoIdent_RtpplConst f =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplConst (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExprNoIdent_RtpplConst : all a. (a -> RtpplConst -> a) -> a -> RtpplExprNoIdent -> a
sem sfold_RtpplExprNoIdent_RtpplConst f acc =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplConst (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExprNoIdent_RtpplPort : all a. (a -> RtpplPort -> (a, RtpplPort)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem smapAccumL_RtpplExprNoIdent_RtpplPort f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExprNoIdent_RtpplPort : (RtpplPort -> RtpplPort) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem smap_RtpplExprNoIdent_RtpplPort f =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplPort (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExprNoIdent_RtpplPort : all a. (a -> RtpplPort -> a) -> a -> RtpplExprNoIdent -> a
sem sfold_RtpplExprNoIdent_RtpplPort f acc =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplPort (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExprNoIdent_RtpplMain : all a. (a -> RtpplMain -> (a, RtpplMain)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem smapAccumL_RtpplExprNoIdent_RtpplMain f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExprNoIdent_RtpplMain : (RtpplMain -> RtpplMain) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem smap_RtpplExprNoIdent_RtpplMain f =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplMain (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExprNoIdent_RtpplMain : all a. (a -> RtpplMain -> a) -> a -> RtpplExprNoIdent -> a
sem sfold_RtpplExprNoIdent_RtpplMain f acc =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplMain (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExprNoIdent_RtpplExt : all a. (a -> RtpplExt -> (a, RtpplExt)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem smapAccumL_RtpplExprNoIdent_RtpplExt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExprNoIdent_RtpplExt : (RtpplExt -> RtpplExt) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem smap_RtpplExprNoIdent_RtpplExt f =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplExt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExprNoIdent_RtpplExt : all a. (a -> RtpplExt -> a) -> a -> RtpplExprNoIdent -> a
sem sfold_RtpplExprNoIdent_RtpplExt f acc =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplExt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExprNoIdent_RtpplTask : all a. (a -> RtpplTask -> (a, RtpplTask)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem smapAccumL_RtpplExprNoIdent_RtpplTask f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExprNoIdent_RtpplTask : (RtpplTask -> RtpplTask) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem smap_RtpplExprNoIdent_RtpplTask f =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplTask (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExprNoIdent_RtpplTask : all a. (a -> RtpplTask -> a) -> a -> RtpplExprNoIdent -> a
sem sfold_RtpplExprNoIdent_RtpplTask f acc =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplTask (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExprNoIdent_RtpplConnection : all a. (a -> RtpplConnection -> (a, RtpplConnection)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem smapAccumL_RtpplExprNoIdent_RtpplConnection f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExprNoIdent_RtpplConnection : (RtpplConnection -> RtpplConnection) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem smap_RtpplExprNoIdent_RtpplConnection f =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplConnection (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExprNoIdent_RtpplConnection : all a. (a -> RtpplConnection -> a) -> a -> RtpplExprNoIdent -> a
sem sfold_RtpplExprNoIdent_RtpplConnection f acc =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplConnection (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExprNoIdent_RtpplPortSpec : all a. (a -> RtpplPortSpec -> (a, RtpplPortSpec)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem smapAccumL_RtpplExprNoIdent_RtpplPortSpec f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExprNoIdent_RtpplPortSpec : (RtpplPortSpec -> RtpplPortSpec) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem smap_RtpplExprNoIdent_RtpplPortSpec f =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplPortSpec (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExprNoIdent_RtpplPortSpec : all a. (a -> RtpplPortSpec -> a) -> a -> RtpplExprNoIdent -> a
sem sfold_RtpplExprNoIdent_RtpplPortSpec f acc =
  | x ->
    (smapAccumL_RtpplExprNoIdent_RtpplPortSpec (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplType_RtpplProgram : all a. (a -> RtpplProgram -> (a, RtpplProgram)) -> a -> RtpplType -> (a, RtpplType)
sem smapAccumL_RtpplType_RtpplProgram f acc =
  | x ->
    (acc, x)
  sem smap_RtpplType_RtpplProgram : (RtpplProgram -> RtpplProgram) -> RtpplType -> RtpplType
sem smap_RtpplType_RtpplProgram f =
  | x ->
    (smapAccumL_RtpplType_RtpplProgram (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplType_RtpplProgram : all a. (a -> RtpplProgram -> a) -> a -> RtpplType -> a
sem sfold_RtpplType_RtpplProgram f acc =
  | x ->
    (smapAccumL_RtpplType_RtpplProgram (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplType_RtpplTop : all a. (a -> RtpplTop -> (a, RtpplTop)) -> a -> RtpplType -> (a, RtpplType)
sem smapAccumL_RtpplType_RtpplTop f acc =
  | x ->
    (acc, x)
  sem smap_RtpplType_RtpplTop : (RtpplTop -> RtpplTop) -> RtpplType -> RtpplType
sem smap_RtpplType_RtpplTop f =
  | x ->
    (smapAccumL_RtpplType_RtpplTop (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplType_RtpplTop : all a. (a -> RtpplTop -> a) -> a -> RtpplType -> a
sem sfold_RtpplType_RtpplTop f acc =
  | x ->
    (smapAccumL_RtpplType_RtpplTop (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplType_RtpplTopParams : all a. (a -> RtpplTopParams -> (a, RtpplTopParams)) -> a -> RtpplType -> (a, RtpplType)
sem smapAccumL_RtpplType_RtpplTopParams f acc =
  | x ->
    (acc, x)
  sem smap_RtpplType_RtpplTopParams : (RtpplTopParams -> RtpplTopParams) -> RtpplType -> RtpplType
sem smap_RtpplType_RtpplTopParams f =
  | x ->
    (smapAccumL_RtpplType_RtpplTopParams (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplType_RtpplTopParams : all a. (a -> RtpplTopParams -> a) -> a -> RtpplType -> a
sem sfold_RtpplType_RtpplTopParams f acc =
  | x ->
    (smapAccumL_RtpplType_RtpplTopParams (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplType_RtpplStmt : all a. (a -> RtpplStmt -> (a, RtpplStmt)) -> a -> RtpplType -> (a, RtpplType)
sem smapAccumL_RtpplType_RtpplStmt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplType_RtpplStmt : (RtpplStmt -> RtpplStmt) -> RtpplType -> RtpplType
sem smap_RtpplType_RtpplStmt f =
  | x ->
    (smapAccumL_RtpplType_RtpplStmt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplType_RtpplStmt : all a. (a -> RtpplStmt -> a) -> a -> RtpplType -> a
sem sfold_RtpplType_RtpplStmt f acc =
  | x ->
    (smapAccumL_RtpplType_RtpplStmt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplType_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)) -> a -> RtpplType -> (a, RtpplType)
sem smapAccumL_RtpplType_RtpplStmtNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplType_RtpplStmtNoIdent : (RtpplStmtNoIdent -> RtpplStmtNoIdent) -> RtpplType -> RtpplType
sem smap_RtpplType_RtpplStmtNoIdent f =
  | x ->
    (smapAccumL_RtpplType_RtpplStmtNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplType_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> a) -> a -> RtpplType -> a
sem sfold_RtpplType_RtpplStmtNoIdent f acc =
  | x ->
    (smapAccumL_RtpplType_RtpplStmtNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplType_RtpplExpr : all a. (a -> RtpplExpr -> (a, RtpplExpr)) -> a -> RtpplType -> (a, RtpplType)
sem smapAccumL_RtpplType_RtpplExpr f acc =
  | x ->
    (acc, x)
  sem smap_RtpplType_RtpplExpr : (RtpplExpr -> RtpplExpr) -> RtpplType -> RtpplType
sem smap_RtpplType_RtpplExpr f =
  | x ->
    (smapAccumL_RtpplType_RtpplExpr (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplType_RtpplExpr : all a. (a -> RtpplExpr -> a) -> a -> RtpplType -> a
sem sfold_RtpplType_RtpplExpr f acc =
  | x ->
    (smapAccumL_RtpplType_RtpplExpr (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplType_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)) -> a -> RtpplType -> (a, RtpplType)
sem smapAccumL_RtpplType_RtpplExprNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplType_RtpplExprNoIdent : (RtpplExprNoIdent -> RtpplExprNoIdent) -> RtpplType -> RtpplType
sem smap_RtpplType_RtpplExprNoIdent f =
  | x ->
    (smapAccumL_RtpplType_RtpplExprNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplType_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> a) -> a -> RtpplType -> a
sem sfold_RtpplType_RtpplExprNoIdent f acc =
  | x ->
    (smapAccumL_RtpplType_RtpplExprNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplType_RtpplType : all a. (a -> RtpplType -> (a, RtpplType)) -> a -> RtpplType -> (a, RtpplType)
sem smapAccumL_RtpplType_RtpplType f acc =
  | x ->
    (acc, x)
  sem smap_RtpplType_RtpplType : (RtpplType -> RtpplType) -> RtpplType -> RtpplType
sem smap_RtpplType_RtpplType f =
  | x ->
    (smapAccumL_RtpplType_RtpplType (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplType_RtpplType : all a. (a -> RtpplType -> a) -> a -> RtpplType -> a
sem sfold_RtpplType_RtpplType f acc =
  | x ->
    (smapAccumL_RtpplType_RtpplType (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplType_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)) -> a -> RtpplType -> (a, RtpplType)
sem smapAccumL_RtpplType_RtpplTypeNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplType_RtpplTypeNoIdent : (RtpplTypeNoIdent -> RtpplTypeNoIdent) -> RtpplType -> RtpplType
sem smap_RtpplType_RtpplTypeNoIdent f =
  | x ->
    (smapAccumL_RtpplType_RtpplTypeNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplType_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> a) -> a -> RtpplType -> a
sem sfold_RtpplType_RtpplTypeNoIdent f acc =
  | x ->
    (smapAccumL_RtpplType_RtpplTypeNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplType_RtpplConst : all a. (a -> RtpplConst -> (a, RtpplConst)) -> a -> RtpplType -> (a, RtpplType)
sem smapAccumL_RtpplType_RtpplConst f acc =
  | x ->
    (acc, x)
  sem smap_RtpplType_RtpplConst : (RtpplConst -> RtpplConst) -> RtpplType -> RtpplType
sem smap_RtpplType_RtpplConst f =
  | x ->
    (smapAccumL_RtpplType_RtpplConst (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplType_RtpplConst : all a. (a -> RtpplConst -> a) -> a -> RtpplType -> a
sem sfold_RtpplType_RtpplConst f acc =
  | x ->
    (smapAccumL_RtpplType_RtpplConst (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplType_RtpplPort : all a. (a -> RtpplPort -> (a, RtpplPort)) -> a -> RtpplType -> (a, RtpplType)
sem smapAccumL_RtpplType_RtpplPort f acc =
  | x ->
    (acc, x)
  sem smap_RtpplType_RtpplPort : (RtpplPort -> RtpplPort) -> RtpplType -> RtpplType
sem smap_RtpplType_RtpplPort f =
  | x ->
    (smapAccumL_RtpplType_RtpplPort (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplType_RtpplPort : all a. (a -> RtpplPort -> a) -> a -> RtpplType -> a
sem sfold_RtpplType_RtpplPort f acc =
  | x ->
    (smapAccumL_RtpplType_RtpplPort (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplType_RtpplMain : all a. (a -> RtpplMain -> (a, RtpplMain)) -> a -> RtpplType -> (a, RtpplType)
sem smapAccumL_RtpplType_RtpplMain f acc =
  | x ->
    (acc, x)
  sem smap_RtpplType_RtpplMain : (RtpplMain -> RtpplMain) -> RtpplType -> RtpplType
sem smap_RtpplType_RtpplMain f =
  | x ->
    (smapAccumL_RtpplType_RtpplMain (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplType_RtpplMain : all a. (a -> RtpplMain -> a) -> a -> RtpplType -> a
sem sfold_RtpplType_RtpplMain f acc =
  | x ->
    (smapAccumL_RtpplType_RtpplMain (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplType_RtpplExt : all a. (a -> RtpplExt -> (a, RtpplExt)) -> a -> RtpplType -> (a, RtpplType)
sem smapAccumL_RtpplType_RtpplExt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplType_RtpplExt : (RtpplExt -> RtpplExt) -> RtpplType -> RtpplType
sem smap_RtpplType_RtpplExt f =
  | x ->
    (smapAccumL_RtpplType_RtpplExt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplType_RtpplExt : all a. (a -> RtpplExt -> a) -> a -> RtpplType -> a
sem sfold_RtpplType_RtpplExt f acc =
  | x ->
    (smapAccumL_RtpplType_RtpplExt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplType_RtpplTask : all a. (a -> RtpplTask -> (a, RtpplTask)) -> a -> RtpplType -> (a, RtpplType)
sem smapAccumL_RtpplType_RtpplTask f acc =
  | x ->
    (acc, x)
  sem smap_RtpplType_RtpplTask : (RtpplTask -> RtpplTask) -> RtpplType -> RtpplType
sem smap_RtpplType_RtpplTask f =
  | x ->
    (smapAccumL_RtpplType_RtpplTask (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplType_RtpplTask : all a. (a -> RtpplTask -> a) -> a -> RtpplType -> a
sem sfold_RtpplType_RtpplTask f acc =
  | x ->
    (smapAccumL_RtpplType_RtpplTask (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplType_RtpplConnection : all a. (a -> RtpplConnection -> (a, RtpplConnection)) -> a -> RtpplType -> (a, RtpplType)
sem smapAccumL_RtpplType_RtpplConnection f acc =
  | x ->
    (acc, x)
  sem smap_RtpplType_RtpplConnection : (RtpplConnection -> RtpplConnection) -> RtpplType -> RtpplType
sem smap_RtpplType_RtpplConnection f =
  | x ->
    (smapAccumL_RtpplType_RtpplConnection (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplType_RtpplConnection : all a. (a -> RtpplConnection -> a) -> a -> RtpplType -> a
sem sfold_RtpplType_RtpplConnection f acc =
  | x ->
    (smapAccumL_RtpplType_RtpplConnection (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplType_RtpplPortSpec : all a. (a -> RtpplPortSpec -> (a, RtpplPortSpec)) -> a -> RtpplType -> (a, RtpplType)
sem smapAccumL_RtpplType_RtpplPortSpec f acc =
  | x ->
    (acc, x)
  sem smap_RtpplType_RtpplPortSpec : (RtpplPortSpec -> RtpplPortSpec) -> RtpplType -> RtpplType
sem smap_RtpplType_RtpplPortSpec f =
  | x ->
    (smapAccumL_RtpplType_RtpplPortSpec (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplType_RtpplPortSpec : all a. (a -> RtpplPortSpec -> a) -> a -> RtpplType -> a
sem sfold_RtpplType_RtpplPortSpec f acc =
  | x ->
    (smapAccumL_RtpplType_RtpplPortSpec (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTypeNoIdent_RtpplProgram : all a. (a -> RtpplProgram -> (a, RtpplProgram)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem smapAccumL_RtpplTypeNoIdent_RtpplProgram f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTypeNoIdent_RtpplProgram : (RtpplProgram -> RtpplProgram) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem smap_RtpplTypeNoIdent_RtpplProgram f =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplProgram (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTypeNoIdent_RtpplProgram : all a. (a -> RtpplProgram -> a) -> a -> RtpplTypeNoIdent -> a
sem sfold_RtpplTypeNoIdent_RtpplProgram f acc =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplProgram (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTypeNoIdent_RtpplTop : all a. (a -> RtpplTop -> (a, RtpplTop)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem smapAccumL_RtpplTypeNoIdent_RtpplTop f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTypeNoIdent_RtpplTop : (RtpplTop -> RtpplTop) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem smap_RtpplTypeNoIdent_RtpplTop f =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplTop (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTypeNoIdent_RtpplTop : all a. (a -> RtpplTop -> a) -> a -> RtpplTypeNoIdent -> a
sem sfold_RtpplTypeNoIdent_RtpplTop f acc =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplTop (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTypeNoIdent_RtpplTopParams : all a. (a -> RtpplTopParams -> (a, RtpplTopParams)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem smapAccumL_RtpplTypeNoIdent_RtpplTopParams f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTypeNoIdent_RtpplTopParams : (RtpplTopParams -> RtpplTopParams) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem smap_RtpplTypeNoIdent_RtpplTopParams f =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplTopParams (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTypeNoIdent_RtpplTopParams : all a. (a -> RtpplTopParams -> a) -> a -> RtpplTypeNoIdent -> a
sem sfold_RtpplTypeNoIdent_RtpplTopParams f acc =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplTopParams (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTypeNoIdent_RtpplStmt : all a. (a -> RtpplStmt -> (a, RtpplStmt)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem smapAccumL_RtpplTypeNoIdent_RtpplStmt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTypeNoIdent_RtpplStmt : (RtpplStmt -> RtpplStmt) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem smap_RtpplTypeNoIdent_RtpplStmt f =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplStmt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTypeNoIdent_RtpplStmt : all a. (a -> RtpplStmt -> a) -> a -> RtpplTypeNoIdent -> a
sem sfold_RtpplTypeNoIdent_RtpplStmt f acc =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplStmt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTypeNoIdent_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem smapAccumL_RtpplTypeNoIdent_RtpplStmtNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTypeNoIdent_RtpplStmtNoIdent : (RtpplStmtNoIdent -> RtpplStmtNoIdent) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem smap_RtpplTypeNoIdent_RtpplStmtNoIdent f =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplStmtNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTypeNoIdent_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> a) -> a -> RtpplTypeNoIdent -> a
sem sfold_RtpplTypeNoIdent_RtpplStmtNoIdent f acc =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplStmtNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTypeNoIdent_RtpplExpr : all a. (a -> RtpplExpr -> (a, RtpplExpr)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem smapAccumL_RtpplTypeNoIdent_RtpplExpr f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTypeNoIdent_RtpplExpr : (RtpplExpr -> RtpplExpr) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem smap_RtpplTypeNoIdent_RtpplExpr f =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplExpr (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTypeNoIdent_RtpplExpr : all a. (a -> RtpplExpr -> a) -> a -> RtpplTypeNoIdent -> a
sem sfold_RtpplTypeNoIdent_RtpplExpr f acc =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplExpr (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTypeNoIdent_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem smapAccumL_RtpplTypeNoIdent_RtpplExprNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTypeNoIdent_RtpplExprNoIdent : (RtpplExprNoIdent -> RtpplExprNoIdent) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem smap_RtpplTypeNoIdent_RtpplExprNoIdent f =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplExprNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTypeNoIdent_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> a) -> a -> RtpplTypeNoIdent -> a
sem sfold_RtpplTypeNoIdent_RtpplExprNoIdent f acc =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplExprNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTypeNoIdent_RtpplType : all a. (a -> RtpplType -> (a, RtpplType)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem smapAccumL_RtpplTypeNoIdent_RtpplType f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTypeNoIdent_RtpplType : (RtpplType -> RtpplType) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem smap_RtpplTypeNoIdent_RtpplType f =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplType (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTypeNoIdent_RtpplType : all a. (a -> RtpplType -> a) -> a -> RtpplTypeNoIdent -> a
sem sfold_RtpplTypeNoIdent_RtpplType f acc =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplType (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTypeNoIdent_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem smapAccumL_RtpplTypeNoIdent_RtpplTypeNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTypeNoIdent_RtpplTypeNoIdent : (RtpplTypeNoIdent -> RtpplTypeNoIdent) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem smap_RtpplTypeNoIdent_RtpplTypeNoIdent f =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplTypeNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTypeNoIdent_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> a) -> a -> RtpplTypeNoIdent -> a
sem sfold_RtpplTypeNoIdent_RtpplTypeNoIdent f acc =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplTypeNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTypeNoIdent_RtpplConst : all a. (a -> RtpplConst -> (a, RtpplConst)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem smapAccumL_RtpplTypeNoIdent_RtpplConst f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTypeNoIdent_RtpplConst : (RtpplConst -> RtpplConst) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem smap_RtpplTypeNoIdent_RtpplConst f =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplConst (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTypeNoIdent_RtpplConst : all a. (a -> RtpplConst -> a) -> a -> RtpplTypeNoIdent -> a
sem sfold_RtpplTypeNoIdent_RtpplConst f acc =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplConst (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTypeNoIdent_RtpplPort : all a. (a -> RtpplPort -> (a, RtpplPort)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem smapAccumL_RtpplTypeNoIdent_RtpplPort f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTypeNoIdent_RtpplPort : (RtpplPort -> RtpplPort) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem smap_RtpplTypeNoIdent_RtpplPort f =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplPort (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTypeNoIdent_RtpplPort : all a. (a -> RtpplPort -> a) -> a -> RtpplTypeNoIdent -> a
sem sfold_RtpplTypeNoIdent_RtpplPort f acc =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplPort (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTypeNoIdent_RtpplMain : all a. (a -> RtpplMain -> (a, RtpplMain)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem smapAccumL_RtpplTypeNoIdent_RtpplMain f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTypeNoIdent_RtpplMain : (RtpplMain -> RtpplMain) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem smap_RtpplTypeNoIdent_RtpplMain f =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplMain (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTypeNoIdent_RtpplMain : all a. (a -> RtpplMain -> a) -> a -> RtpplTypeNoIdent -> a
sem sfold_RtpplTypeNoIdent_RtpplMain f acc =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplMain (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTypeNoIdent_RtpplExt : all a. (a -> RtpplExt -> (a, RtpplExt)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem smapAccumL_RtpplTypeNoIdent_RtpplExt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTypeNoIdent_RtpplExt : (RtpplExt -> RtpplExt) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem smap_RtpplTypeNoIdent_RtpplExt f =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplExt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTypeNoIdent_RtpplExt : all a. (a -> RtpplExt -> a) -> a -> RtpplTypeNoIdent -> a
sem sfold_RtpplTypeNoIdent_RtpplExt f acc =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplExt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTypeNoIdent_RtpplTask : all a. (a -> RtpplTask -> (a, RtpplTask)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem smapAccumL_RtpplTypeNoIdent_RtpplTask f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTypeNoIdent_RtpplTask : (RtpplTask -> RtpplTask) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem smap_RtpplTypeNoIdent_RtpplTask f =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplTask (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTypeNoIdent_RtpplTask : all a. (a -> RtpplTask -> a) -> a -> RtpplTypeNoIdent -> a
sem sfold_RtpplTypeNoIdent_RtpplTask f acc =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplTask (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTypeNoIdent_RtpplConnection : all a. (a -> RtpplConnection -> (a, RtpplConnection)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem smapAccumL_RtpplTypeNoIdent_RtpplConnection f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTypeNoIdent_RtpplConnection : (RtpplConnection -> RtpplConnection) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem smap_RtpplTypeNoIdent_RtpplConnection f =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplConnection (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTypeNoIdent_RtpplConnection : all a. (a -> RtpplConnection -> a) -> a -> RtpplTypeNoIdent -> a
sem sfold_RtpplTypeNoIdent_RtpplConnection f acc =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplConnection (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTypeNoIdent_RtpplPortSpec : all a. (a -> RtpplPortSpec -> (a, RtpplPortSpec)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem smapAccumL_RtpplTypeNoIdent_RtpplPortSpec f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTypeNoIdent_RtpplPortSpec : (RtpplPortSpec -> RtpplPortSpec) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem smap_RtpplTypeNoIdent_RtpplPortSpec f =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplPortSpec (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTypeNoIdent_RtpplPortSpec : all a. (a -> RtpplPortSpec -> a) -> a -> RtpplTypeNoIdent -> a
sem sfold_RtpplTypeNoIdent_RtpplPortSpec f acc =
  | x ->
    (smapAccumL_RtpplTypeNoIdent_RtpplPortSpec (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConst_RtpplProgram : all a. (a -> RtpplProgram -> (a, RtpplProgram)) -> a -> RtpplConst -> (a, RtpplConst)
sem smapAccumL_RtpplConst_RtpplProgram f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConst_RtpplProgram : (RtpplProgram -> RtpplProgram) -> RtpplConst -> RtpplConst
sem smap_RtpplConst_RtpplProgram f =
  | x ->
    (smapAccumL_RtpplConst_RtpplProgram (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConst_RtpplProgram : all a. (a -> RtpplProgram -> a) -> a -> RtpplConst -> a
sem sfold_RtpplConst_RtpplProgram f acc =
  | x ->
    (smapAccumL_RtpplConst_RtpplProgram (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConst_RtpplTop : all a. (a -> RtpplTop -> (a, RtpplTop)) -> a -> RtpplConst -> (a, RtpplConst)
sem smapAccumL_RtpplConst_RtpplTop f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConst_RtpplTop : (RtpplTop -> RtpplTop) -> RtpplConst -> RtpplConst
sem smap_RtpplConst_RtpplTop f =
  | x ->
    (smapAccumL_RtpplConst_RtpplTop (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConst_RtpplTop : all a. (a -> RtpplTop -> a) -> a -> RtpplConst -> a
sem sfold_RtpplConst_RtpplTop f acc =
  | x ->
    (smapAccumL_RtpplConst_RtpplTop (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConst_RtpplTopParams : all a. (a -> RtpplTopParams -> (a, RtpplTopParams)) -> a -> RtpplConst -> (a, RtpplConst)
sem smapAccumL_RtpplConst_RtpplTopParams f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConst_RtpplTopParams : (RtpplTopParams -> RtpplTopParams) -> RtpplConst -> RtpplConst
sem smap_RtpplConst_RtpplTopParams f =
  | x ->
    (smapAccumL_RtpplConst_RtpplTopParams (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConst_RtpplTopParams : all a. (a -> RtpplTopParams -> a) -> a -> RtpplConst -> a
sem sfold_RtpplConst_RtpplTopParams f acc =
  | x ->
    (smapAccumL_RtpplConst_RtpplTopParams (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConst_RtpplStmt : all a. (a -> RtpplStmt -> (a, RtpplStmt)) -> a -> RtpplConst -> (a, RtpplConst)
sem smapAccumL_RtpplConst_RtpplStmt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConst_RtpplStmt : (RtpplStmt -> RtpplStmt) -> RtpplConst -> RtpplConst
sem smap_RtpplConst_RtpplStmt f =
  | x ->
    (smapAccumL_RtpplConst_RtpplStmt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConst_RtpplStmt : all a. (a -> RtpplStmt -> a) -> a -> RtpplConst -> a
sem sfold_RtpplConst_RtpplStmt f acc =
  | x ->
    (smapAccumL_RtpplConst_RtpplStmt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConst_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)) -> a -> RtpplConst -> (a, RtpplConst)
sem smapAccumL_RtpplConst_RtpplStmtNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConst_RtpplStmtNoIdent : (RtpplStmtNoIdent -> RtpplStmtNoIdent) -> RtpplConst -> RtpplConst
sem smap_RtpplConst_RtpplStmtNoIdent f =
  | x ->
    (smapAccumL_RtpplConst_RtpplStmtNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConst_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> a) -> a -> RtpplConst -> a
sem sfold_RtpplConst_RtpplStmtNoIdent f acc =
  | x ->
    (smapAccumL_RtpplConst_RtpplStmtNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConst_RtpplExpr : all a. (a -> RtpplExpr -> (a, RtpplExpr)) -> a -> RtpplConst -> (a, RtpplConst)
sem smapAccumL_RtpplConst_RtpplExpr f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConst_RtpplExpr : (RtpplExpr -> RtpplExpr) -> RtpplConst -> RtpplConst
sem smap_RtpplConst_RtpplExpr f =
  | x ->
    (smapAccumL_RtpplConst_RtpplExpr (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConst_RtpplExpr : all a. (a -> RtpplExpr -> a) -> a -> RtpplConst -> a
sem sfold_RtpplConst_RtpplExpr f acc =
  | x ->
    (smapAccumL_RtpplConst_RtpplExpr (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConst_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)) -> a -> RtpplConst -> (a, RtpplConst)
sem smapAccumL_RtpplConst_RtpplExprNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConst_RtpplExprNoIdent : (RtpplExprNoIdent -> RtpplExprNoIdent) -> RtpplConst -> RtpplConst
sem smap_RtpplConst_RtpplExprNoIdent f =
  | x ->
    (smapAccumL_RtpplConst_RtpplExprNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConst_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> a) -> a -> RtpplConst -> a
sem sfold_RtpplConst_RtpplExprNoIdent f acc =
  | x ->
    (smapAccumL_RtpplConst_RtpplExprNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConst_RtpplType : all a. (a -> RtpplType -> (a, RtpplType)) -> a -> RtpplConst -> (a, RtpplConst)
sem smapAccumL_RtpplConst_RtpplType f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConst_RtpplType : (RtpplType -> RtpplType) -> RtpplConst -> RtpplConst
sem smap_RtpplConst_RtpplType f =
  | x ->
    (smapAccumL_RtpplConst_RtpplType (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConst_RtpplType : all a. (a -> RtpplType -> a) -> a -> RtpplConst -> a
sem sfold_RtpplConst_RtpplType f acc =
  | x ->
    (smapAccumL_RtpplConst_RtpplType (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConst_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)) -> a -> RtpplConst -> (a, RtpplConst)
sem smapAccumL_RtpplConst_RtpplTypeNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConst_RtpplTypeNoIdent : (RtpplTypeNoIdent -> RtpplTypeNoIdent) -> RtpplConst -> RtpplConst
sem smap_RtpplConst_RtpplTypeNoIdent f =
  | x ->
    (smapAccumL_RtpplConst_RtpplTypeNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConst_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> a) -> a -> RtpplConst -> a
sem sfold_RtpplConst_RtpplTypeNoIdent f acc =
  | x ->
    (smapAccumL_RtpplConst_RtpplTypeNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConst_RtpplConst : all a. (a -> RtpplConst -> (a, RtpplConst)) -> a -> RtpplConst -> (a, RtpplConst)
sem smapAccumL_RtpplConst_RtpplConst f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConst_RtpplConst : (RtpplConst -> RtpplConst) -> RtpplConst -> RtpplConst
sem smap_RtpplConst_RtpplConst f =
  | x ->
    (smapAccumL_RtpplConst_RtpplConst (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConst_RtpplConst : all a. (a -> RtpplConst -> a) -> a -> RtpplConst -> a
sem sfold_RtpplConst_RtpplConst f acc =
  | x ->
    (smapAccumL_RtpplConst_RtpplConst (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConst_RtpplPort : all a. (a -> RtpplPort -> (a, RtpplPort)) -> a -> RtpplConst -> (a, RtpplConst)
sem smapAccumL_RtpplConst_RtpplPort f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConst_RtpplPort : (RtpplPort -> RtpplPort) -> RtpplConst -> RtpplConst
sem smap_RtpplConst_RtpplPort f =
  | x ->
    (smapAccumL_RtpplConst_RtpplPort (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConst_RtpplPort : all a. (a -> RtpplPort -> a) -> a -> RtpplConst -> a
sem sfold_RtpplConst_RtpplPort f acc =
  | x ->
    (smapAccumL_RtpplConst_RtpplPort (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConst_RtpplMain : all a. (a -> RtpplMain -> (a, RtpplMain)) -> a -> RtpplConst -> (a, RtpplConst)
sem smapAccumL_RtpplConst_RtpplMain f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConst_RtpplMain : (RtpplMain -> RtpplMain) -> RtpplConst -> RtpplConst
sem smap_RtpplConst_RtpplMain f =
  | x ->
    (smapAccumL_RtpplConst_RtpplMain (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConst_RtpplMain : all a. (a -> RtpplMain -> a) -> a -> RtpplConst -> a
sem sfold_RtpplConst_RtpplMain f acc =
  | x ->
    (smapAccumL_RtpplConst_RtpplMain (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConst_RtpplExt : all a. (a -> RtpplExt -> (a, RtpplExt)) -> a -> RtpplConst -> (a, RtpplConst)
sem smapAccumL_RtpplConst_RtpplExt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConst_RtpplExt : (RtpplExt -> RtpplExt) -> RtpplConst -> RtpplConst
sem smap_RtpplConst_RtpplExt f =
  | x ->
    (smapAccumL_RtpplConst_RtpplExt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConst_RtpplExt : all a. (a -> RtpplExt -> a) -> a -> RtpplConst -> a
sem sfold_RtpplConst_RtpplExt f acc =
  | x ->
    (smapAccumL_RtpplConst_RtpplExt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConst_RtpplTask : all a. (a -> RtpplTask -> (a, RtpplTask)) -> a -> RtpplConst -> (a, RtpplConst)
sem smapAccumL_RtpplConst_RtpplTask f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConst_RtpplTask : (RtpplTask -> RtpplTask) -> RtpplConst -> RtpplConst
sem smap_RtpplConst_RtpplTask f =
  | x ->
    (smapAccumL_RtpplConst_RtpplTask (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConst_RtpplTask : all a. (a -> RtpplTask -> a) -> a -> RtpplConst -> a
sem sfold_RtpplConst_RtpplTask f acc =
  | x ->
    (smapAccumL_RtpplConst_RtpplTask (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConst_RtpplConnection : all a. (a -> RtpplConnection -> (a, RtpplConnection)) -> a -> RtpplConst -> (a, RtpplConst)
sem smapAccumL_RtpplConst_RtpplConnection f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConst_RtpplConnection : (RtpplConnection -> RtpplConnection) -> RtpplConst -> RtpplConst
sem smap_RtpplConst_RtpplConnection f =
  | x ->
    (smapAccumL_RtpplConst_RtpplConnection (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConst_RtpplConnection : all a. (a -> RtpplConnection -> a) -> a -> RtpplConst -> a
sem sfold_RtpplConst_RtpplConnection f acc =
  | x ->
    (smapAccumL_RtpplConst_RtpplConnection (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConst_RtpplPortSpec : all a. (a -> RtpplPortSpec -> (a, RtpplPortSpec)) -> a -> RtpplConst -> (a, RtpplConst)
sem smapAccumL_RtpplConst_RtpplPortSpec f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConst_RtpplPortSpec : (RtpplPortSpec -> RtpplPortSpec) -> RtpplConst -> RtpplConst
sem smap_RtpplConst_RtpplPortSpec f =
  | x ->
    (smapAccumL_RtpplConst_RtpplPortSpec (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConst_RtpplPortSpec : all a. (a -> RtpplPortSpec -> a) -> a -> RtpplConst -> a
sem sfold_RtpplConst_RtpplPortSpec f acc =
  | x ->
    (smapAccumL_RtpplConst_RtpplPortSpec (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPort_RtpplProgram : all a. (a -> RtpplProgram -> (a, RtpplProgram)) -> a -> RtpplPort -> (a, RtpplPort)
sem smapAccumL_RtpplPort_RtpplProgram f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPort_RtpplProgram : (RtpplProgram -> RtpplProgram) -> RtpplPort -> RtpplPort
sem smap_RtpplPort_RtpplProgram f =
  | x ->
    (smapAccumL_RtpplPort_RtpplProgram (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPort_RtpplProgram : all a. (a -> RtpplProgram -> a) -> a -> RtpplPort -> a
sem sfold_RtpplPort_RtpplProgram f acc =
  | x ->
    (smapAccumL_RtpplPort_RtpplProgram (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPort_RtpplTop : all a. (a -> RtpplTop -> (a, RtpplTop)) -> a -> RtpplPort -> (a, RtpplPort)
sem smapAccumL_RtpplPort_RtpplTop f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPort_RtpplTop : (RtpplTop -> RtpplTop) -> RtpplPort -> RtpplPort
sem smap_RtpplPort_RtpplTop f =
  | x ->
    (smapAccumL_RtpplPort_RtpplTop (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPort_RtpplTop : all a. (a -> RtpplTop -> a) -> a -> RtpplPort -> a
sem sfold_RtpplPort_RtpplTop f acc =
  | x ->
    (smapAccumL_RtpplPort_RtpplTop (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPort_RtpplTopParams : all a. (a -> RtpplTopParams -> (a, RtpplTopParams)) -> a -> RtpplPort -> (a, RtpplPort)
sem smapAccumL_RtpplPort_RtpplTopParams f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPort_RtpplTopParams : (RtpplTopParams -> RtpplTopParams) -> RtpplPort -> RtpplPort
sem smap_RtpplPort_RtpplTopParams f =
  | x ->
    (smapAccumL_RtpplPort_RtpplTopParams (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPort_RtpplTopParams : all a. (a -> RtpplTopParams -> a) -> a -> RtpplPort -> a
sem sfold_RtpplPort_RtpplTopParams f acc =
  | x ->
    (smapAccumL_RtpplPort_RtpplTopParams (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPort_RtpplStmt : all a. (a -> RtpplStmt -> (a, RtpplStmt)) -> a -> RtpplPort -> (a, RtpplPort)
sem smapAccumL_RtpplPort_RtpplStmt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPort_RtpplStmt : (RtpplStmt -> RtpplStmt) -> RtpplPort -> RtpplPort
sem smap_RtpplPort_RtpplStmt f =
  | x ->
    (smapAccumL_RtpplPort_RtpplStmt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPort_RtpplStmt : all a. (a -> RtpplStmt -> a) -> a -> RtpplPort -> a
sem sfold_RtpplPort_RtpplStmt f acc =
  | x ->
    (smapAccumL_RtpplPort_RtpplStmt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPort_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)) -> a -> RtpplPort -> (a, RtpplPort)
sem smapAccumL_RtpplPort_RtpplStmtNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPort_RtpplStmtNoIdent : (RtpplStmtNoIdent -> RtpplStmtNoIdent) -> RtpplPort -> RtpplPort
sem smap_RtpplPort_RtpplStmtNoIdent f =
  | x ->
    (smapAccumL_RtpplPort_RtpplStmtNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPort_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> a) -> a -> RtpplPort -> a
sem sfold_RtpplPort_RtpplStmtNoIdent f acc =
  | x ->
    (smapAccumL_RtpplPort_RtpplStmtNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPort_RtpplExpr : all a. (a -> RtpplExpr -> (a, RtpplExpr)) -> a -> RtpplPort -> (a, RtpplPort)
sem smapAccumL_RtpplPort_RtpplExpr f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPort_RtpplExpr : (RtpplExpr -> RtpplExpr) -> RtpplPort -> RtpplPort
sem smap_RtpplPort_RtpplExpr f =
  | x ->
    (smapAccumL_RtpplPort_RtpplExpr (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPort_RtpplExpr : all a. (a -> RtpplExpr -> a) -> a -> RtpplPort -> a
sem sfold_RtpplPort_RtpplExpr f acc =
  | x ->
    (smapAccumL_RtpplPort_RtpplExpr (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPort_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)) -> a -> RtpplPort -> (a, RtpplPort)
sem smapAccumL_RtpplPort_RtpplExprNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPort_RtpplExprNoIdent : (RtpplExprNoIdent -> RtpplExprNoIdent) -> RtpplPort -> RtpplPort
sem smap_RtpplPort_RtpplExprNoIdent f =
  | x ->
    (smapAccumL_RtpplPort_RtpplExprNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPort_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> a) -> a -> RtpplPort -> a
sem sfold_RtpplPort_RtpplExprNoIdent f acc =
  | x ->
    (smapAccumL_RtpplPort_RtpplExprNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPort_RtpplType : all a. (a -> RtpplType -> (a, RtpplType)) -> a -> RtpplPort -> (a, RtpplPort)
sem smapAccumL_RtpplPort_RtpplType f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPort_RtpplType : (RtpplType -> RtpplType) -> RtpplPort -> RtpplPort
sem smap_RtpplPort_RtpplType f =
  | x ->
    (smapAccumL_RtpplPort_RtpplType (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPort_RtpplType : all a. (a -> RtpplType -> a) -> a -> RtpplPort -> a
sem sfold_RtpplPort_RtpplType f acc =
  | x ->
    (smapAccumL_RtpplPort_RtpplType (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPort_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)) -> a -> RtpplPort -> (a, RtpplPort)
sem smapAccumL_RtpplPort_RtpplTypeNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPort_RtpplTypeNoIdent : (RtpplTypeNoIdent -> RtpplTypeNoIdent) -> RtpplPort -> RtpplPort
sem smap_RtpplPort_RtpplTypeNoIdent f =
  | x ->
    (smapAccumL_RtpplPort_RtpplTypeNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPort_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> a) -> a -> RtpplPort -> a
sem sfold_RtpplPort_RtpplTypeNoIdent f acc =
  | x ->
    (smapAccumL_RtpplPort_RtpplTypeNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPort_RtpplConst : all a. (a -> RtpplConst -> (a, RtpplConst)) -> a -> RtpplPort -> (a, RtpplPort)
sem smapAccumL_RtpplPort_RtpplConst f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPort_RtpplConst : (RtpplConst -> RtpplConst) -> RtpplPort -> RtpplPort
sem smap_RtpplPort_RtpplConst f =
  | x ->
    (smapAccumL_RtpplPort_RtpplConst (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPort_RtpplConst : all a. (a -> RtpplConst -> a) -> a -> RtpplPort -> a
sem sfold_RtpplPort_RtpplConst f acc =
  | x ->
    (smapAccumL_RtpplPort_RtpplConst (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPort_RtpplPort : all a. (a -> RtpplPort -> (a, RtpplPort)) -> a -> RtpplPort -> (a, RtpplPort)
sem smapAccumL_RtpplPort_RtpplPort f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPort_RtpplPort : (RtpplPort -> RtpplPort) -> RtpplPort -> RtpplPort
sem smap_RtpplPort_RtpplPort f =
  | x ->
    (smapAccumL_RtpplPort_RtpplPort (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPort_RtpplPort : all a. (a -> RtpplPort -> a) -> a -> RtpplPort -> a
sem sfold_RtpplPort_RtpplPort f acc =
  | x ->
    (smapAccumL_RtpplPort_RtpplPort (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPort_RtpplMain : all a. (a -> RtpplMain -> (a, RtpplMain)) -> a -> RtpplPort -> (a, RtpplPort)
sem smapAccumL_RtpplPort_RtpplMain f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPort_RtpplMain : (RtpplMain -> RtpplMain) -> RtpplPort -> RtpplPort
sem smap_RtpplPort_RtpplMain f =
  | x ->
    (smapAccumL_RtpplPort_RtpplMain (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPort_RtpplMain : all a. (a -> RtpplMain -> a) -> a -> RtpplPort -> a
sem sfold_RtpplPort_RtpplMain f acc =
  | x ->
    (smapAccumL_RtpplPort_RtpplMain (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPort_RtpplExt : all a. (a -> RtpplExt -> (a, RtpplExt)) -> a -> RtpplPort -> (a, RtpplPort)
sem smapAccumL_RtpplPort_RtpplExt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPort_RtpplExt : (RtpplExt -> RtpplExt) -> RtpplPort -> RtpplPort
sem smap_RtpplPort_RtpplExt f =
  | x ->
    (smapAccumL_RtpplPort_RtpplExt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPort_RtpplExt : all a. (a -> RtpplExt -> a) -> a -> RtpplPort -> a
sem sfold_RtpplPort_RtpplExt f acc =
  | x ->
    (smapAccumL_RtpplPort_RtpplExt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPort_RtpplTask : all a. (a -> RtpplTask -> (a, RtpplTask)) -> a -> RtpplPort -> (a, RtpplPort)
sem smapAccumL_RtpplPort_RtpplTask f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPort_RtpplTask : (RtpplTask -> RtpplTask) -> RtpplPort -> RtpplPort
sem smap_RtpplPort_RtpplTask f =
  | x ->
    (smapAccumL_RtpplPort_RtpplTask (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPort_RtpplTask : all a. (a -> RtpplTask -> a) -> a -> RtpplPort -> a
sem sfold_RtpplPort_RtpplTask f acc =
  | x ->
    (smapAccumL_RtpplPort_RtpplTask (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPort_RtpplConnection : all a. (a -> RtpplConnection -> (a, RtpplConnection)) -> a -> RtpplPort -> (a, RtpplPort)
sem smapAccumL_RtpplPort_RtpplConnection f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPort_RtpplConnection : (RtpplConnection -> RtpplConnection) -> RtpplPort -> RtpplPort
sem smap_RtpplPort_RtpplConnection f =
  | x ->
    (smapAccumL_RtpplPort_RtpplConnection (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPort_RtpplConnection : all a. (a -> RtpplConnection -> a) -> a -> RtpplPort -> a
sem sfold_RtpplPort_RtpplConnection f acc =
  | x ->
    (smapAccumL_RtpplPort_RtpplConnection (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPort_RtpplPortSpec : all a. (a -> RtpplPortSpec -> (a, RtpplPortSpec)) -> a -> RtpplPort -> (a, RtpplPort)
sem smapAccumL_RtpplPort_RtpplPortSpec f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPort_RtpplPortSpec : (RtpplPortSpec -> RtpplPortSpec) -> RtpplPort -> RtpplPort
sem smap_RtpplPort_RtpplPortSpec f =
  | x ->
    (smapAccumL_RtpplPort_RtpplPortSpec (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPort_RtpplPortSpec : all a. (a -> RtpplPortSpec -> a) -> a -> RtpplPort -> a
sem sfold_RtpplPort_RtpplPortSpec f acc =
  | x ->
    (smapAccumL_RtpplPort_RtpplPortSpec (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplMain_RtpplProgram : all a. (a -> RtpplProgram -> (a, RtpplProgram)) -> a -> RtpplMain -> (a, RtpplMain)
sem smapAccumL_RtpplMain_RtpplProgram f acc =
  | x ->
    (acc, x)
  sem smap_RtpplMain_RtpplProgram : (RtpplProgram -> RtpplProgram) -> RtpplMain -> RtpplMain
sem smap_RtpplMain_RtpplProgram f =
  | x ->
    (smapAccumL_RtpplMain_RtpplProgram (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplMain_RtpplProgram : all a. (a -> RtpplProgram -> a) -> a -> RtpplMain -> a
sem sfold_RtpplMain_RtpplProgram f acc =
  | x ->
    (smapAccumL_RtpplMain_RtpplProgram (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplMain_RtpplTop : all a. (a -> RtpplTop -> (a, RtpplTop)) -> a -> RtpplMain -> (a, RtpplMain)
sem smapAccumL_RtpplMain_RtpplTop f acc =
  | x ->
    (acc, x)
  sem smap_RtpplMain_RtpplTop : (RtpplTop -> RtpplTop) -> RtpplMain -> RtpplMain
sem smap_RtpplMain_RtpplTop f =
  | x ->
    (smapAccumL_RtpplMain_RtpplTop (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplMain_RtpplTop : all a. (a -> RtpplTop -> a) -> a -> RtpplMain -> a
sem sfold_RtpplMain_RtpplTop f acc =
  | x ->
    (smapAccumL_RtpplMain_RtpplTop (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplMain_RtpplTopParams : all a. (a -> RtpplTopParams -> (a, RtpplTopParams)) -> a -> RtpplMain -> (a, RtpplMain)
sem smapAccumL_RtpplMain_RtpplTopParams f acc =
  | x ->
    (acc, x)
  sem smap_RtpplMain_RtpplTopParams : (RtpplTopParams -> RtpplTopParams) -> RtpplMain -> RtpplMain
sem smap_RtpplMain_RtpplTopParams f =
  | x ->
    (smapAccumL_RtpplMain_RtpplTopParams (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplMain_RtpplTopParams : all a. (a -> RtpplTopParams -> a) -> a -> RtpplMain -> a
sem sfold_RtpplMain_RtpplTopParams f acc =
  | x ->
    (smapAccumL_RtpplMain_RtpplTopParams (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplMain_RtpplStmt : all a. (a -> RtpplStmt -> (a, RtpplStmt)) -> a -> RtpplMain -> (a, RtpplMain)
sem smapAccumL_RtpplMain_RtpplStmt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplMain_RtpplStmt : (RtpplStmt -> RtpplStmt) -> RtpplMain -> RtpplMain
sem smap_RtpplMain_RtpplStmt f =
  | x ->
    (smapAccumL_RtpplMain_RtpplStmt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplMain_RtpplStmt : all a. (a -> RtpplStmt -> a) -> a -> RtpplMain -> a
sem sfold_RtpplMain_RtpplStmt f acc =
  | x ->
    (smapAccumL_RtpplMain_RtpplStmt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplMain_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)) -> a -> RtpplMain -> (a, RtpplMain)
sem smapAccumL_RtpplMain_RtpplStmtNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplMain_RtpplStmtNoIdent : (RtpplStmtNoIdent -> RtpplStmtNoIdent) -> RtpplMain -> RtpplMain
sem smap_RtpplMain_RtpplStmtNoIdent f =
  | x ->
    (smapAccumL_RtpplMain_RtpplStmtNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplMain_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> a) -> a -> RtpplMain -> a
sem sfold_RtpplMain_RtpplStmtNoIdent f acc =
  | x ->
    (smapAccumL_RtpplMain_RtpplStmtNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplMain_RtpplExpr : all a. (a -> RtpplExpr -> (a, RtpplExpr)) -> a -> RtpplMain -> (a, RtpplMain)
sem smapAccumL_RtpplMain_RtpplExpr f acc =
  | x ->
    (acc, x)
  sem smap_RtpplMain_RtpplExpr : (RtpplExpr -> RtpplExpr) -> RtpplMain -> RtpplMain
sem smap_RtpplMain_RtpplExpr f =
  | x ->
    (smapAccumL_RtpplMain_RtpplExpr (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplMain_RtpplExpr : all a. (a -> RtpplExpr -> a) -> a -> RtpplMain -> a
sem sfold_RtpplMain_RtpplExpr f acc =
  | x ->
    (smapAccumL_RtpplMain_RtpplExpr (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplMain_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)) -> a -> RtpplMain -> (a, RtpplMain)
sem smapAccumL_RtpplMain_RtpplExprNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplMain_RtpplExprNoIdent : (RtpplExprNoIdent -> RtpplExprNoIdent) -> RtpplMain -> RtpplMain
sem smap_RtpplMain_RtpplExprNoIdent f =
  | x ->
    (smapAccumL_RtpplMain_RtpplExprNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplMain_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> a) -> a -> RtpplMain -> a
sem sfold_RtpplMain_RtpplExprNoIdent f acc =
  | x ->
    (smapAccumL_RtpplMain_RtpplExprNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplMain_RtpplType : all a. (a -> RtpplType -> (a, RtpplType)) -> a -> RtpplMain -> (a, RtpplMain)
sem smapAccumL_RtpplMain_RtpplType f acc =
  | x ->
    (acc, x)
  sem smap_RtpplMain_RtpplType : (RtpplType -> RtpplType) -> RtpplMain -> RtpplMain
sem smap_RtpplMain_RtpplType f =
  | x ->
    (smapAccumL_RtpplMain_RtpplType (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplMain_RtpplType : all a. (a -> RtpplType -> a) -> a -> RtpplMain -> a
sem sfold_RtpplMain_RtpplType f acc =
  | x ->
    (smapAccumL_RtpplMain_RtpplType (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplMain_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)) -> a -> RtpplMain -> (a, RtpplMain)
sem smapAccumL_RtpplMain_RtpplTypeNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplMain_RtpplTypeNoIdent : (RtpplTypeNoIdent -> RtpplTypeNoIdent) -> RtpplMain -> RtpplMain
sem smap_RtpplMain_RtpplTypeNoIdent f =
  | x ->
    (smapAccumL_RtpplMain_RtpplTypeNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplMain_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> a) -> a -> RtpplMain -> a
sem sfold_RtpplMain_RtpplTypeNoIdent f acc =
  | x ->
    (smapAccumL_RtpplMain_RtpplTypeNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplMain_RtpplConst : all a. (a -> RtpplConst -> (a, RtpplConst)) -> a -> RtpplMain -> (a, RtpplMain)
sem smapAccumL_RtpplMain_RtpplConst f acc =
  | x ->
    (acc, x)
  sem smap_RtpplMain_RtpplConst : (RtpplConst -> RtpplConst) -> RtpplMain -> RtpplMain
sem smap_RtpplMain_RtpplConst f =
  | x ->
    (smapAccumL_RtpplMain_RtpplConst (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplMain_RtpplConst : all a. (a -> RtpplConst -> a) -> a -> RtpplMain -> a
sem sfold_RtpplMain_RtpplConst f acc =
  | x ->
    (smapAccumL_RtpplMain_RtpplConst (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplMain_RtpplPort : all a. (a -> RtpplPort -> (a, RtpplPort)) -> a -> RtpplMain -> (a, RtpplMain)
sem smapAccumL_RtpplMain_RtpplPort f acc =
  | x ->
    (acc, x)
  sem smap_RtpplMain_RtpplPort : (RtpplPort -> RtpplPort) -> RtpplMain -> RtpplMain
sem smap_RtpplMain_RtpplPort f =
  | x ->
    (smapAccumL_RtpplMain_RtpplPort (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplMain_RtpplPort : all a. (a -> RtpplPort -> a) -> a -> RtpplMain -> a
sem sfold_RtpplMain_RtpplPort f acc =
  | x ->
    (smapAccumL_RtpplMain_RtpplPort (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplMain_RtpplMain : all a. (a -> RtpplMain -> (a, RtpplMain)) -> a -> RtpplMain -> (a, RtpplMain)
sem smapAccumL_RtpplMain_RtpplMain f acc =
  | x ->
    (acc, x)
  sem smap_RtpplMain_RtpplMain : (RtpplMain -> RtpplMain) -> RtpplMain -> RtpplMain
sem smap_RtpplMain_RtpplMain f =
  | x ->
    (smapAccumL_RtpplMain_RtpplMain (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplMain_RtpplMain : all a. (a -> RtpplMain -> a) -> a -> RtpplMain -> a
sem sfold_RtpplMain_RtpplMain f acc =
  | x ->
    (smapAccumL_RtpplMain_RtpplMain (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplMain_RtpplExt : all a. (a -> RtpplExt -> (a, RtpplExt)) -> a -> RtpplMain -> (a, RtpplMain)
sem smapAccumL_RtpplMain_RtpplExt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplMain_RtpplExt : (RtpplExt -> RtpplExt) -> RtpplMain -> RtpplMain
sem smap_RtpplMain_RtpplExt f =
  | x ->
    (smapAccumL_RtpplMain_RtpplExt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplMain_RtpplExt : all a. (a -> RtpplExt -> a) -> a -> RtpplMain -> a
sem sfold_RtpplMain_RtpplExt f acc =
  | x ->
    (smapAccumL_RtpplMain_RtpplExt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplMain_RtpplTask : all a. (a -> RtpplTask -> (a, RtpplTask)) -> a -> RtpplMain -> (a, RtpplMain)
sem smapAccumL_RtpplMain_RtpplTask f acc =
  | x ->
    (acc, x)
  sem smap_RtpplMain_RtpplTask : (RtpplTask -> RtpplTask) -> RtpplMain -> RtpplMain
sem smap_RtpplMain_RtpplTask f =
  | x ->
    (smapAccumL_RtpplMain_RtpplTask (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplMain_RtpplTask : all a. (a -> RtpplTask -> a) -> a -> RtpplMain -> a
sem sfold_RtpplMain_RtpplTask f acc =
  | x ->
    (smapAccumL_RtpplMain_RtpplTask (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplMain_RtpplConnection : all a. (a -> RtpplConnection -> (a, RtpplConnection)) -> a -> RtpplMain -> (a, RtpplMain)
sem smapAccumL_RtpplMain_RtpplConnection f acc =
  | x ->
    (acc, x)
  sem smap_RtpplMain_RtpplConnection : (RtpplConnection -> RtpplConnection) -> RtpplMain -> RtpplMain
sem smap_RtpplMain_RtpplConnection f =
  | x ->
    (smapAccumL_RtpplMain_RtpplConnection (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplMain_RtpplConnection : all a. (a -> RtpplConnection -> a) -> a -> RtpplMain -> a
sem sfold_RtpplMain_RtpplConnection f acc =
  | x ->
    (smapAccumL_RtpplMain_RtpplConnection (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplMain_RtpplPortSpec : all a. (a -> RtpplPortSpec -> (a, RtpplPortSpec)) -> a -> RtpplMain -> (a, RtpplMain)
sem smapAccumL_RtpplMain_RtpplPortSpec f acc =
  | x ->
    (acc, x)
  sem smap_RtpplMain_RtpplPortSpec : (RtpplPortSpec -> RtpplPortSpec) -> RtpplMain -> RtpplMain
sem smap_RtpplMain_RtpplPortSpec f =
  | x ->
    (smapAccumL_RtpplMain_RtpplPortSpec (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplMain_RtpplPortSpec : all a. (a -> RtpplPortSpec -> a) -> a -> RtpplMain -> a
sem sfold_RtpplMain_RtpplPortSpec f acc =
  | x ->
    (smapAccumL_RtpplMain_RtpplPortSpec (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExt_RtpplProgram : all a. (a -> RtpplProgram -> (a, RtpplProgram)) -> a -> RtpplExt -> (a, RtpplExt)
sem smapAccumL_RtpplExt_RtpplProgram f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExt_RtpplProgram : (RtpplProgram -> RtpplProgram) -> RtpplExt -> RtpplExt
sem smap_RtpplExt_RtpplProgram f =
  | x ->
    (smapAccumL_RtpplExt_RtpplProgram (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExt_RtpplProgram : all a. (a -> RtpplProgram -> a) -> a -> RtpplExt -> a
sem sfold_RtpplExt_RtpplProgram f acc =
  | x ->
    (smapAccumL_RtpplExt_RtpplProgram (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExt_RtpplTop : all a. (a -> RtpplTop -> (a, RtpplTop)) -> a -> RtpplExt -> (a, RtpplExt)
sem smapAccumL_RtpplExt_RtpplTop f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExt_RtpplTop : (RtpplTop -> RtpplTop) -> RtpplExt -> RtpplExt
sem smap_RtpplExt_RtpplTop f =
  | x ->
    (smapAccumL_RtpplExt_RtpplTop (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExt_RtpplTop : all a. (a -> RtpplTop -> a) -> a -> RtpplExt -> a
sem sfold_RtpplExt_RtpplTop f acc =
  | x ->
    (smapAccumL_RtpplExt_RtpplTop (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExt_RtpplTopParams : all a. (a -> RtpplTopParams -> (a, RtpplTopParams)) -> a -> RtpplExt -> (a, RtpplExt)
sem smapAccumL_RtpplExt_RtpplTopParams f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExt_RtpplTopParams : (RtpplTopParams -> RtpplTopParams) -> RtpplExt -> RtpplExt
sem smap_RtpplExt_RtpplTopParams f =
  | x ->
    (smapAccumL_RtpplExt_RtpplTopParams (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExt_RtpplTopParams : all a. (a -> RtpplTopParams -> a) -> a -> RtpplExt -> a
sem sfold_RtpplExt_RtpplTopParams f acc =
  | x ->
    (smapAccumL_RtpplExt_RtpplTopParams (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExt_RtpplStmt : all a. (a -> RtpplStmt -> (a, RtpplStmt)) -> a -> RtpplExt -> (a, RtpplExt)
sem smapAccumL_RtpplExt_RtpplStmt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExt_RtpplStmt : (RtpplStmt -> RtpplStmt) -> RtpplExt -> RtpplExt
sem smap_RtpplExt_RtpplStmt f =
  | x ->
    (smapAccumL_RtpplExt_RtpplStmt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExt_RtpplStmt : all a. (a -> RtpplStmt -> a) -> a -> RtpplExt -> a
sem sfold_RtpplExt_RtpplStmt f acc =
  | x ->
    (smapAccumL_RtpplExt_RtpplStmt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExt_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)) -> a -> RtpplExt -> (a, RtpplExt)
sem smapAccumL_RtpplExt_RtpplStmtNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExt_RtpplStmtNoIdent : (RtpplStmtNoIdent -> RtpplStmtNoIdent) -> RtpplExt -> RtpplExt
sem smap_RtpplExt_RtpplStmtNoIdent f =
  | x ->
    (smapAccumL_RtpplExt_RtpplStmtNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExt_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> a) -> a -> RtpplExt -> a
sem sfold_RtpplExt_RtpplStmtNoIdent f acc =
  | x ->
    (smapAccumL_RtpplExt_RtpplStmtNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExt_RtpplExpr : all a. (a -> RtpplExpr -> (a, RtpplExpr)) -> a -> RtpplExt -> (a, RtpplExt)
sem smapAccumL_RtpplExt_RtpplExpr f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExt_RtpplExpr : (RtpplExpr -> RtpplExpr) -> RtpplExt -> RtpplExt
sem smap_RtpplExt_RtpplExpr f =
  | x ->
    (smapAccumL_RtpplExt_RtpplExpr (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExt_RtpplExpr : all a. (a -> RtpplExpr -> a) -> a -> RtpplExt -> a
sem sfold_RtpplExt_RtpplExpr f acc =
  | x ->
    (smapAccumL_RtpplExt_RtpplExpr (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExt_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)) -> a -> RtpplExt -> (a, RtpplExt)
sem smapAccumL_RtpplExt_RtpplExprNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExt_RtpplExprNoIdent : (RtpplExprNoIdent -> RtpplExprNoIdent) -> RtpplExt -> RtpplExt
sem smap_RtpplExt_RtpplExprNoIdent f =
  | x ->
    (smapAccumL_RtpplExt_RtpplExprNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExt_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> a) -> a -> RtpplExt -> a
sem sfold_RtpplExt_RtpplExprNoIdent f acc =
  | x ->
    (smapAccumL_RtpplExt_RtpplExprNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExt_RtpplType : all a. (a -> RtpplType -> (a, RtpplType)) -> a -> RtpplExt -> (a, RtpplExt)
sem smapAccumL_RtpplExt_RtpplType f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExt_RtpplType : (RtpplType -> RtpplType) -> RtpplExt -> RtpplExt
sem smap_RtpplExt_RtpplType f =
  | x ->
    (smapAccumL_RtpplExt_RtpplType (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExt_RtpplType : all a. (a -> RtpplType -> a) -> a -> RtpplExt -> a
sem sfold_RtpplExt_RtpplType f acc =
  | x ->
    (smapAccumL_RtpplExt_RtpplType (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExt_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)) -> a -> RtpplExt -> (a, RtpplExt)
sem smapAccumL_RtpplExt_RtpplTypeNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExt_RtpplTypeNoIdent : (RtpplTypeNoIdent -> RtpplTypeNoIdent) -> RtpplExt -> RtpplExt
sem smap_RtpplExt_RtpplTypeNoIdent f =
  | x ->
    (smapAccumL_RtpplExt_RtpplTypeNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExt_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> a) -> a -> RtpplExt -> a
sem sfold_RtpplExt_RtpplTypeNoIdent f acc =
  | x ->
    (smapAccumL_RtpplExt_RtpplTypeNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExt_RtpplConst : all a. (a -> RtpplConst -> (a, RtpplConst)) -> a -> RtpplExt -> (a, RtpplExt)
sem smapAccumL_RtpplExt_RtpplConst f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExt_RtpplConst : (RtpplConst -> RtpplConst) -> RtpplExt -> RtpplExt
sem smap_RtpplExt_RtpplConst f =
  | x ->
    (smapAccumL_RtpplExt_RtpplConst (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExt_RtpplConst : all a. (a -> RtpplConst -> a) -> a -> RtpplExt -> a
sem sfold_RtpplExt_RtpplConst f acc =
  | x ->
    (smapAccumL_RtpplExt_RtpplConst (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExt_RtpplPort : all a. (a -> RtpplPort -> (a, RtpplPort)) -> a -> RtpplExt -> (a, RtpplExt)
sem smapAccumL_RtpplExt_RtpplPort f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExt_RtpplPort : (RtpplPort -> RtpplPort) -> RtpplExt -> RtpplExt
sem smap_RtpplExt_RtpplPort f =
  | x ->
    (smapAccumL_RtpplExt_RtpplPort (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExt_RtpplPort : all a. (a -> RtpplPort -> a) -> a -> RtpplExt -> a
sem sfold_RtpplExt_RtpplPort f acc =
  | x ->
    (smapAccumL_RtpplExt_RtpplPort (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExt_RtpplMain : all a. (a -> RtpplMain -> (a, RtpplMain)) -> a -> RtpplExt -> (a, RtpplExt)
sem smapAccumL_RtpplExt_RtpplMain f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExt_RtpplMain : (RtpplMain -> RtpplMain) -> RtpplExt -> RtpplExt
sem smap_RtpplExt_RtpplMain f =
  | x ->
    (smapAccumL_RtpplExt_RtpplMain (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExt_RtpplMain : all a. (a -> RtpplMain -> a) -> a -> RtpplExt -> a
sem sfold_RtpplExt_RtpplMain f acc =
  | x ->
    (smapAccumL_RtpplExt_RtpplMain (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExt_RtpplExt : all a. (a -> RtpplExt -> (a, RtpplExt)) -> a -> RtpplExt -> (a, RtpplExt)
sem smapAccumL_RtpplExt_RtpplExt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExt_RtpplExt : (RtpplExt -> RtpplExt) -> RtpplExt -> RtpplExt
sem smap_RtpplExt_RtpplExt f =
  | x ->
    (smapAccumL_RtpplExt_RtpplExt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExt_RtpplExt : all a. (a -> RtpplExt -> a) -> a -> RtpplExt -> a
sem sfold_RtpplExt_RtpplExt f acc =
  | x ->
    (smapAccumL_RtpplExt_RtpplExt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExt_RtpplTask : all a. (a -> RtpplTask -> (a, RtpplTask)) -> a -> RtpplExt -> (a, RtpplExt)
sem smapAccumL_RtpplExt_RtpplTask f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExt_RtpplTask : (RtpplTask -> RtpplTask) -> RtpplExt -> RtpplExt
sem smap_RtpplExt_RtpplTask f =
  | x ->
    (smapAccumL_RtpplExt_RtpplTask (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExt_RtpplTask : all a. (a -> RtpplTask -> a) -> a -> RtpplExt -> a
sem sfold_RtpplExt_RtpplTask f acc =
  | x ->
    (smapAccumL_RtpplExt_RtpplTask (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExt_RtpplConnection : all a. (a -> RtpplConnection -> (a, RtpplConnection)) -> a -> RtpplExt -> (a, RtpplExt)
sem smapAccumL_RtpplExt_RtpplConnection f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExt_RtpplConnection : (RtpplConnection -> RtpplConnection) -> RtpplExt -> RtpplExt
sem smap_RtpplExt_RtpplConnection f =
  | x ->
    (smapAccumL_RtpplExt_RtpplConnection (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExt_RtpplConnection : all a. (a -> RtpplConnection -> a) -> a -> RtpplExt -> a
sem sfold_RtpplExt_RtpplConnection f acc =
  | x ->
    (smapAccumL_RtpplExt_RtpplConnection (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplExt_RtpplPortSpec : all a. (a -> RtpplPortSpec -> (a, RtpplPortSpec)) -> a -> RtpplExt -> (a, RtpplExt)
sem smapAccumL_RtpplExt_RtpplPortSpec f acc =
  | x ->
    (acc, x)
  sem smap_RtpplExt_RtpplPortSpec : (RtpplPortSpec -> RtpplPortSpec) -> RtpplExt -> RtpplExt
sem smap_RtpplExt_RtpplPortSpec f =
  | x ->
    (smapAccumL_RtpplExt_RtpplPortSpec (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplExt_RtpplPortSpec : all a. (a -> RtpplPortSpec -> a) -> a -> RtpplExt -> a
sem sfold_RtpplExt_RtpplPortSpec f acc =
  | x ->
    (smapAccumL_RtpplExt_RtpplPortSpec (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTask_RtpplProgram : all a. (a -> RtpplProgram -> (a, RtpplProgram)) -> a -> RtpplTask -> (a, RtpplTask)
sem smapAccumL_RtpplTask_RtpplProgram f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTask_RtpplProgram : (RtpplProgram -> RtpplProgram) -> RtpplTask -> RtpplTask
sem smap_RtpplTask_RtpplProgram f =
  | x ->
    (smapAccumL_RtpplTask_RtpplProgram (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTask_RtpplProgram : all a. (a -> RtpplProgram -> a) -> a -> RtpplTask -> a
sem sfold_RtpplTask_RtpplProgram f acc =
  | x ->
    (smapAccumL_RtpplTask_RtpplProgram (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTask_RtpplTop : all a. (a -> RtpplTop -> (a, RtpplTop)) -> a -> RtpplTask -> (a, RtpplTask)
sem smapAccumL_RtpplTask_RtpplTop f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTask_RtpplTop : (RtpplTop -> RtpplTop) -> RtpplTask -> RtpplTask
sem smap_RtpplTask_RtpplTop f =
  | x ->
    (smapAccumL_RtpplTask_RtpplTop (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTask_RtpplTop : all a. (a -> RtpplTop -> a) -> a -> RtpplTask -> a
sem sfold_RtpplTask_RtpplTop f acc =
  | x ->
    (smapAccumL_RtpplTask_RtpplTop (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTask_RtpplTopParams : all a. (a -> RtpplTopParams -> (a, RtpplTopParams)) -> a -> RtpplTask -> (a, RtpplTask)
sem smapAccumL_RtpplTask_RtpplTopParams f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTask_RtpplTopParams : (RtpplTopParams -> RtpplTopParams) -> RtpplTask -> RtpplTask
sem smap_RtpplTask_RtpplTopParams f =
  | x ->
    (smapAccumL_RtpplTask_RtpplTopParams (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTask_RtpplTopParams : all a. (a -> RtpplTopParams -> a) -> a -> RtpplTask -> a
sem sfold_RtpplTask_RtpplTopParams f acc =
  | x ->
    (smapAccumL_RtpplTask_RtpplTopParams (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTask_RtpplStmt : all a. (a -> RtpplStmt -> (a, RtpplStmt)) -> a -> RtpplTask -> (a, RtpplTask)
sem smapAccumL_RtpplTask_RtpplStmt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTask_RtpplStmt : (RtpplStmt -> RtpplStmt) -> RtpplTask -> RtpplTask
sem smap_RtpplTask_RtpplStmt f =
  | x ->
    (smapAccumL_RtpplTask_RtpplStmt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTask_RtpplStmt : all a. (a -> RtpplStmt -> a) -> a -> RtpplTask -> a
sem sfold_RtpplTask_RtpplStmt f acc =
  | x ->
    (smapAccumL_RtpplTask_RtpplStmt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTask_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)) -> a -> RtpplTask -> (a, RtpplTask)
sem smapAccumL_RtpplTask_RtpplStmtNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTask_RtpplStmtNoIdent : (RtpplStmtNoIdent -> RtpplStmtNoIdent) -> RtpplTask -> RtpplTask
sem smap_RtpplTask_RtpplStmtNoIdent f =
  | x ->
    (smapAccumL_RtpplTask_RtpplStmtNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTask_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> a) -> a -> RtpplTask -> a
sem sfold_RtpplTask_RtpplStmtNoIdent f acc =
  | x ->
    (smapAccumL_RtpplTask_RtpplStmtNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTask_RtpplExpr : all a. (a -> RtpplExpr -> (a, RtpplExpr)) -> a -> RtpplTask -> (a, RtpplTask)
sem smapAccumL_RtpplTask_RtpplExpr f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTask_RtpplExpr : (RtpplExpr -> RtpplExpr) -> RtpplTask -> RtpplTask
sem smap_RtpplTask_RtpplExpr f =
  | x ->
    (smapAccumL_RtpplTask_RtpplExpr (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTask_RtpplExpr : all a. (a -> RtpplExpr -> a) -> a -> RtpplTask -> a
sem sfold_RtpplTask_RtpplExpr f acc =
  | x ->
    (smapAccumL_RtpplTask_RtpplExpr (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTask_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)) -> a -> RtpplTask -> (a, RtpplTask)
sem smapAccumL_RtpplTask_RtpplExprNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTask_RtpplExprNoIdent : (RtpplExprNoIdent -> RtpplExprNoIdent) -> RtpplTask -> RtpplTask
sem smap_RtpplTask_RtpplExprNoIdent f =
  | x ->
    (smapAccumL_RtpplTask_RtpplExprNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTask_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> a) -> a -> RtpplTask -> a
sem sfold_RtpplTask_RtpplExprNoIdent f acc =
  | x ->
    (smapAccumL_RtpplTask_RtpplExprNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTask_RtpplType : all a. (a -> RtpplType -> (a, RtpplType)) -> a -> RtpplTask -> (a, RtpplTask)
sem smapAccumL_RtpplTask_RtpplType f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTask_RtpplType : (RtpplType -> RtpplType) -> RtpplTask -> RtpplTask
sem smap_RtpplTask_RtpplType f =
  | x ->
    (smapAccumL_RtpplTask_RtpplType (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTask_RtpplType : all a. (a -> RtpplType -> a) -> a -> RtpplTask -> a
sem sfold_RtpplTask_RtpplType f acc =
  | x ->
    (smapAccumL_RtpplTask_RtpplType (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTask_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)) -> a -> RtpplTask -> (a, RtpplTask)
sem smapAccumL_RtpplTask_RtpplTypeNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTask_RtpplTypeNoIdent : (RtpplTypeNoIdent -> RtpplTypeNoIdent) -> RtpplTask -> RtpplTask
sem smap_RtpplTask_RtpplTypeNoIdent f =
  | x ->
    (smapAccumL_RtpplTask_RtpplTypeNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTask_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> a) -> a -> RtpplTask -> a
sem sfold_RtpplTask_RtpplTypeNoIdent f acc =
  | x ->
    (smapAccumL_RtpplTask_RtpplTypeNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTask_RtpplConst : all a. (a -> RtpplConst -> (a, RtpplConst)) -> a -> RtpplTask -> (a, RtpplTask)
sem smapAccumL_RtpplTask_RtpplConst f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTask_RtpplConst : (RtpplConst -> RtpplConst) -> RtpplTask -> RtpplTask
sem smap_RtpplTask_RtpplConst f =
  | x ->
    (smapAccumL_RtpplTask_RtpplConst (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTask_RtpplConst : all a. (a -> RtpplConst -> a) -> a -> RtpplTask -> a
sem sfold_RtpplTask_RtpplConst f acc =
  | x ->
    (smapAccumL_RtpplTask_RtpplConst (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTask_RtpplPort : all a. (a -> RtpplPort -> (a, RtpplPort)) -> a -> RtpplTask -> (a, RtpplTask)
sem smapAccumL_RtpplTask_RtpplPort f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTask_RtpplPort : (RtpplPort -> RtpplPort) -> RtpplTask -> RtpplTask
sem smap_RtpplTask_RtpplPort f =
  | x ->
    (smapAccumL_RtpplTask_RtpplPort (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTask_RtpplPort : all a. (a -> RtpplPort -> a) -> a -> RtpplTask -> a
sem sfold_RtpplTask_RtpplPort f acc =
  | x ->
    (smapAccumL_RtpplTask_RtpplPort (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTask_RtpplMain : all a. (a -> RtpplMain -> (a, RtpplMain)) -> a -> RtpplTask -> (a, RtpplTask)
sem smapAccumL_RtpplTask_RtpplMain f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTask_RtpplMain : (RtpplMain -> RtpplMain) -> RtpplTask -> RtpplTask
sem smap_RtpplTask_RtpplMain f =
  | x ->
    (smapAccumL_RtpplTask_RtpplMain (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTask_RtpplMain : all a. (a -> RtpplMain -> a) -> a -> RtpplTask -> a
sem sfold_RtpplTask_RtpplMain f acc =
  | x ->
    (smapAccumL_RtpplTask_RtpplMain (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTask_RtpplExt : all a. (a -> RtpplExt -> (a, RtpplExt)) -> a -> RtpplTask -> (a, RtpplTask)
sem smapAccumL_RtpplTask_RtpplExt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTask_RtpplExt : (RtpplExt -> RtpplExt) -> RtpplTask -> RtpplTask
sem smap_RtpplTask_RtpplExt f =
  | x ->
    (smapAccumL_RtpplTask_RtpplExt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTask_RtpplExt : all a. (a -> RtpplExt -> a) -> a -> RtpplTask -> a
sem sfold_RtpplTask_RtpplExt f acc =
  | x ->
    (smapAccumL_RtpplTask_RtpplExt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTask_RtpplTask : all a. (a -> RtpplTask -> (a, RtpplTask)) -> a -> RtpplTask -> (a, RtpplTask)
sem smapAccumL_RtpplTask_RtpplTask f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTask_RtpplTask : (RtpplTask -> RtpplTask) -> RtpplTask -> RtpplTask
sem smap_RtpplTask_RtpplTask f =
  | x ->
    (smapAccumL_RtpplTask_RtpplTask (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTask_RtpplTask : all a. (a -> RtpplTask -> a) -> a -> RtpplTask -> a
sem sfold_RtpplTask_RtpplTask f acc =
  | x ->
    (smapAccumL_RtpplTask_RtpplTask (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTask_RtpplConnection : all a. (a -> RtpplConnection -> (a, RtpplConnection)) -> a -> RtpplTask -> (a, RtpplTask)
sem smapAccumL_RtpplTask_RtpplConnection f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTask_RtpplConnection : (RtpplConnection -> RtpplConnection) -> RtpplTask -> RtpplTask
sem smap_RtpplTask_RtpplConnection f =
  | x ->
    (smapAccumL_RtpplTask_RtpplConnection (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTask_RtpplConnection : all a. (a -> RtpplConnection -> a) -> a -> RtpplTask -> a
sem sfold_RtpplTask_RtpplConnection f acc =
  | x ->
    (smapAccumL_RtpplTask_RtpplConnection (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplTask_RtpplPortSpec : all a. (a -> RtpplPortSpec -> (a, RtpplPortSpec)) -> a -> RtpplTask -> (a, RtpplTask)
sem smapAccumL_RtpplTask_RtpplPortSpec f acc =
  | x ->
    (acc, x)
  sem smap_RtpplTask_RtpplPortSpec : (RtpplPortSpec -> RtpplPortSpec) -> RtpplTask -> RtpplTask
sem smap_RtpplTask_RtpplPortSpec f =
  | x ->
    (smapAccumL_RtpplTask_RtpplPortSpec (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplTask_RtpplPortSpec : all a. (a -> RtpplPortSpec -> a) -> a -> RtpplTask -> a
sem sfold_RtpplTask_RtpplPortSpec f acc =
  | x ->
    (smapAccumL_RtpplTask_RtpplPortSpec (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConnection_RtpplProgram : all a. (a -> RtpplProgram -> (a, RtpplProgram)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem smapAccumL_RtpplConnection_RtpplProgram f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConnection_RtpplProgram : (RtpplProgram -> RtpplProgram) -> RtpplConnection -> RtpplConnection
sem smap_RtpplConnection_RtpplProgram f =
  | x ->
    (smapAccumL_RtpplConnection_RtpplProgram (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConnection_RtpplProgram : all a. (a -> RtpplProgram -> a) -> a -> RtpplConnection -> a
sem sfold_RtpplConnection_RtpplProgram f acc =
  | x ->
    (smapAccumL_RtpplConnection_RtpplProgram (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConnection_RtpplTop : all a. (a -> RtpplTop -> (a, RtpplTop)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem smapAccumL_RtpplConnection_RtpplTop f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConnection_RtpplTop : (RtpplTop -> RtpplTop) -> RtpplConnection -> RtpplConnection
sem smap_RtpplConnection_RtpplTop f =
  | x ->
    (smapAccumL_RtpplConnection_RtpplTop (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConnection_RtpplTop : all a. (a -> RtpplTop -> a) -> a -> RtpplConnection -> a
sem sfold_RtpplConnection_RtpplTop f acc =
  | x ->
    (smapAccumL_RtpplConnection_RtpplTop (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConnection_RtpplTopParams : all a. (a -> RtpplTopParams -> (a, RtpplTopParams)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem smapAccumL_RtpplConnection_RtpplTopParams f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConnection_RtpplTopParams : (RtpplTopParams -> RtpplTopParams) -> RtpplConnection -> RtpplConnection
sem smap_RtpplConnection_RtpplTopParams f =
  | x ->
    (smapAccumL_RtpplConnection_RtpplTopParams (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConnection_RtpplTopParams : all a. (a -> RtpplTopParams -> a) -> a -> RtpplConnection -> a
sem sfold_RtpplConnection_RtpplTopParams f acc =
  | x ->
    (smapAccumL_RtpplConnection_RtpplTopParams (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConnection_RtpplStmt : all a. (a -> RtpplStmt -> (a, RtpplStmt)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem smapAccumL_RtpplConnection_RtpplStmt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConnection_RtpplStmt : (RtpplStmt -> RtpplStmt) -> RtpplConnection -> RtpplConnection
sem smap_RtpplConnection_RtpplStmt f =
  | x ->
    (smapAccumL_RtpplConnection_RtpplStmt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConnection_RtpplStmt : all a. (a -> RtpplStmt -> a) -> a -> RtpplConnection -> a
sem sfold_RtpplConnection_RtpplStmt f acc =
  | x ->
    (smapAccumL_RtpplConnection_RtpplStmt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConnection_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem smapAccumL_RtpplConnection_RtpplStmtNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConnection_RtpplStmtNoIdent : (RtpplStmtNoIdent -> RtpplStmtNoIdent) -> RtpplConnection -> RtpplConnection
sem smap_RtpplConnection_RtpplStmtNoIdent f =
  | x ->
    (smapAccumL_RtpplConnection_RtpplStmtNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConnection_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> a) -> a -> RtpplConnection -> a
sem sfold_RtpplConnection_RtpplStmtNoIdent f acc =
  | x ->
    (smapAccumL_RtpplConnection_RtpplStmtNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConnection_RtpplExpr : all a. (a -> RtpplExpr -> (a, RtpplExpr)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem smapAccumL_RtpplConnection_RtpplExpr f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConnection_RtpplExpr : (RtpplExpr -> RtpplExpr) -> RtpplConnection -> RtpplConnection
sem smap_RtpplConnection_RtpplExpr f =
  | x ->
    (smapAccumL_RtpplConnection_RtpplExpr (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConnection_RtpplExpr : all a. (a -> RtpplExpr -> a) -> a -> RtpplConnection -> a
sem sfold_RtpplConnection_RtpplExpr f acc =
  | x ->
    (smapAccumL_RtpplConnection_RtpplExpr (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConnection_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem smapAccumL_RtpplConnection_RtpplExprNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConnection_RtpplExprNoIdent : (RtpplExprNoIdent -> RtpplExprNoIdent) -> RtpplConnection -> RtpplConnection
sem smap_RtpplConnection_RtpplExprNoIdent f =
  | x ->
    (smapAccumL_RtpplConnection_RtpplExprNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConnection_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> a) -> a -> RtpplConnection -> a
sem sfold_RtpplConnection_RtpplExprNoIdent f acc =
  | x ->
    (smapAccumL_RtpplConnection_RtpplExprNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConnection_RtpplType : all a. (a -> RtpplType -> (a, RtpplType)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem smapAccumL_RtpplConnection_RtpplType f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConnection_RtpplType : (RtpplType -> RtpplType) -> RtpplConnection -> RtpplConnection
sem smap_RtpplConnection_RtpplType f =
  | x ->
    (smapAccumL_RtpplConnection_RtpplType (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConnection_RtpplType : all a. (a -> RtpplType -> a) -> a -> RtpplConnection -> a
sem sfold_RtpplConnection_RtpplType f acc =
  | x ->
    (smapAccumL_RtpplConnection_RtpplType (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConnection_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem smapAccumL_RtpplConnection_RtpplTypeNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConnection_RtpplTypeNoIdent : (RtpplTypeNoIdent -> RtpplTypeNoIdent) -> RtpplConnection -> RtpplConnection
sem smap_RtpplConnection_RtpplTypeNoIdent f =
  | x ->
    (smapAccumL_RtpplConnection_RtpplTypeNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConnection_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> a) -> a -> RtpplConnection -> a
sem sfold_RtpplConnection_RtpplTypeNoIdent f acc =
  | x ->
    (smapAccumL_RtpplConnection_RtpplTypeNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConnection_RtpplConst : all a. (a -> RtpplConst -> (a, RtpplConst)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem smapAccumL_RtpplConnection_RtpplConst f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConnection_RtpplConst : (RtpplConst -> RtpplConst) -> RtpplConnection -> RtpplConnection
sem smap_RtpplConnection_RtpplConst f =
  | x ->
    (smapAccumL_RtpplConnection_RtpplConst (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConnection_RtpplConst : all a. (a -> RtpplConst -> a) -> a -> RtpplConnection -> a
sem sfold_RtpplConnection_RtpplConst f acc =
  | x ->
    (smapAccumL_RtpplConnection_RtpplConst (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConnection_RtpplPort : all a. (a -> RtpplPort -> (a, RtpplPort)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem smapAccumL_RtpplConnection_RtpplPort f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConnection_RtpplPort : (RtpplPort -> RtpplPort) -> RtpplConnection -> RtpplConnection
sem smap_RtpplConnection_RtpplPort f =
  | x ->
    (smapAccumL_RtpplConnection_RtpplPort (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConnection_RtpplPort : all a. (a -> RtpplPort -> a) -> a -> RtpplConnection -> a
sem sfold_RtpplConnection_RtpplPort f acc =
  | x ->
    (smapAccumL_RtpplConnection_RtpplPort (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConnection_RtpplMain : all a. (a -> RtpplMain -> (a, RtpplMain)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem smapAccumL_RtpplConnection_RtpplMain f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConnection_RtpplMain : (RtpplMain -> RtpplMain) -> RtpplConnection -> RtpplConnection
sem smap_RtpplConnection_RtpplMain f =
  | x ->
    (smapAccumL_RtpplConnection_RtpplMain (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConnection_RtpplMain : all a. (a -> RtpplMain -> a) -> a -> RtpplConnection -> a
sem sfold_RtpplConnection_RtpplMain f acc =
  | x ->
    (smapAccumL_RtpplConnection_RtpplMain (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConnection_RtpplExt : all a. (a -> RtpplExt -> (a, RtpplExt)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem smapAccumL_RtpplConnection_RtpplExt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConnection_RtpplExt : (RtpplExt -> RtpplExt) -> RtpplConnection -> RtpplConnection
sem smap_RtpplConnection_RtpplExt f =
  | x ->
    (smapAccumL_RtpplConnection_RtpplExt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConnection_RtpplExt : all a. (a -> RtpplExt -> a) -> a -> RtpplConnection -> a
sem sfold_RtpplConnection_RtpplExt f acc =
  | x ->
    (smapAccumL_RtpplConnection_RtpplExt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConnection_RtpplTask : all a. (a -> RtpplTask -> (a, RtpplTask)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem smapAccumL_RtpplConnection_RtpplTask f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConnection_RtpplTask : (RtpplTask -> RtpplTask) -> RtpplConnection -> RtpplConnection
sem smap_RtpplConnection_RtpplTask f =
  | x ->
    (smapAccumL_RtpplConnection_RtpplTask (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConnection_RtpplTask : all a. (a -> RtpplTask -> a) -> a -> RtpplConnection -> a
sem sfold_RtpplConnection_RtpplTask f acc =
  | x ->
    (smapAccumL_RtpplConnection_RtpplTask (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConnection_RtpplConnection : all a. (a -> RtpplConnection -> (a, RtpplConnection)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem smapAccumL_RtpplConnection_RtpplConnection f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConnection_RtpplConnection : (RtpplConnection -> RtpplConnection) -> RtpplConnection -> RtpplConnection
sem smap_RtpplConnection_RtpplConnection f =
  | x ->
    (smapAccumL_RtpplConnection_RtpplConnection (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConnection_RtpplConnection : all a. (a -> RtpplConnection -> a) -> a -> RtpplConnection -> a
sem sfold_RtpplConnection_RtpplConnection f acc =
  | x ->
    (smapAccumL_RtpplConnection_RtpplConnection (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplConnection_RtpplPortSpec : all a. (a -> RtpplPortSpec -> (a, RtpplPortSpec)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem smapAccumL_RtpplConnection_RtpplPortSpec f acc =
  | x ->
    (acc, x)
  sem smap_RtpplConnection_RtpplPortSpec : (RtpplPortSpec -> RtpplPortSpec) -> RtpplConnection -> RtpplConnection
sem smap_RtpplConnection_RtpplPortSpec f =
  | x ->
    (smapAccumL_RtpplConnection_RtpplPortSpec (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplConnection_RtpplPortSpec : all a. (a -> RtpplPortSpec -> a) -> a -> RtpplConnection -> a
sem sfold_RtpplConnection_RtpplPortSpec f acc =
  | x ->
    (smapAccumL_RtpplConnection_RtpplPortSpec (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPortSpec_RtpplProgram : all a. (a -> RtpplProgram -> (a, RtpplProgram)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem smapAccumL_RtpplPortSpec_RtpplProgram f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPortSpec_RtpplProgram : (RtpplProgram -> RtpplProgram) -> RtpplPortSpec -> RtpplPortSpec
sem smap_RtpplPortSpec_RtpplProgram f =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplProgram (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPortSpec_RtpplProgram : all a. (a -> RtpplProgram -> a) -> a -> RtpplPortSpec -> a
sem sfold_RtpplPortSpec_RtpplProgram f acc =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplProgram (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPortSpec_RtpplTop : all a. (a -> RtpplTop -> (a, RtpplTop)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem smapAccumL_RtpplPortSpec_RtpplTop f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPortSpec_RtpplTop : (RtpplTop -> RtpplTop) -> RtpplPortSpec -> RtpplPortSpec
sem smap_RtpplPortSpec_RtpplTop f =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplTop (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPortSpec_RtpplTop : all a. (a -> RtpplTop -> a) -> a -> RtpplPortSpec -> a
sem sfold_RtpplPortSpec_RtpplTop f acc =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplTop (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPortSpec_RtpplTopParams : all a. (a -> RtpplTopParams -> (a, RtpplTopParams)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem smapAccumL_RtpplPortSpec_RtpplTopParams f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPortSpec_RtpplTopParams : (RtpplTopParams -> RtpplTopParams) -> RtpplPortSpec -> RtpplPortSpec
sem smap_RtpplPortSpec_RtpplTopParams f =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplTopParams (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPortSpec_RtpplTopParams : all a. (a -> RtpplTopParams -> a) -> a -> RtpplPortSpec -> a
sem sfold_RtpplPortSpec_RtpplTopParams f acc =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplTopParams (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPortSpec_RtpplStmt : all a. (a -> RtpplStmt -> (a, RtpplStmt)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem smapAccumL_RtpplPortSpec_RtpplStmt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPortSpec_RtpplStmt : (RtpplStmt -> RtpplStmt) -> RtpplPortSpec -> RtpplPortSpec
sem smap_RtpplPortSpec_RtpplStmt f =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplStmt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPortSpec_RtpplStmt : all a. (a -> RtpplStmt -> a) -> a -> RtpplPortSpec -> a
sem sfold_RtpplPortSpec_RtpplStmt f acc =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplStmt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPortSpec_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem smapAccumL_RtpplPortSpec_RtpplStmtNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPortSpec_RtpplStmtNoIdent : (RtpplStmtNoIdent -> RtpplStmtNoIdent) -> RtpplPortSpec -> RtpplPortSpec
sem smap_RtpplPortSpec_RtpplStmtNoIdent f =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplStmtNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPortSpec_RtpplStmtNoIdent : all a. (a -> RtpplStmtNoIdent -> a) -> a -> RtpplPortSpec -> a
sem sfold_RtpplPortSpec_RtpplStmtNoIdent f acc =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplStmtNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPortSpec_RtpplExpr : all a. (a -> RtpplExpr -> (a, RtpplExpr)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem smapAccumL_RtpplPortSpec_RtpplExpr f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPortSpec_RtpplExpr : (RtpplExpr -> RtpplExpr) -> RtpplPortSpec -> RtpplPortSpec
sem smap_RtpplPortSpec_RtpplExpr f =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplExpr (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPortSpec_RtpplExpr : all a. (a -> RtpplExpr -> a) -> a -> RtpplPortSpec -> a
sem sfold_RtpplPortSpec_RtpplExpr f acc =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplExpr (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPortSpec_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem smapAccumL_RtpplPortSpec_RtpplExprNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPortSpec_RtpplExprNoIdent : (RtpplExprNoIdent -> RtpplExprNoIdent) -> RtpplPortSpec -> RtpplPortSpec
sem smap_RtpplPortSpec_RtpplExprNoIdent f =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplExprNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPortSpec_RtpplExprNoIdent : all a. (a -> RtpplExprNoIdent -> a) -> a -> RtpplPortSpec -> a
sem sfold_RtpplPortSpec_RtpplExprNoIdent f acc =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplExprNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPortSpec_RtpplType : all a. (a -> RtpplType -> (a, RtpplType)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem smapAccumL_RtpplPortSpec_RtpplType f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPortSpec_RtpplType : (RtpplType -> RtpplType) -> RtpplPortSpec -> RtpplPortSpec
sem smap_RtpplPortSpec_RtpplType f =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplType (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPortSpec_RtpplType : all a. (a -> RtpplType -> a) -> a -> RtpplPortSpec -> a
sem sfold_RtpplPortSpec_RtpplType f acc =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplType (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPortSpec_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem smapAccumL_RtpplPortSpec_RtpplTypeNoIdent f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPortSpec_RtpplTypeNoIdent : (RtpplTypeNoIdent -> RtpplTypeNoIdent) -> RtpplPortSpec -> RtpplPortSpec
sem smap_RtpplPortSpec_RtpplTypeNoIdent f =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplTypeNoIdent (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPortSpec_RtpplTypeNoIdent : all a. (a -> RtpplTypeNoIdent -> a) -> a -> RtpplPortSpec -> a
sem sfold_RtpplPortSpec_RtpplTypeNoIdent f acc =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplTypeNoIdent (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPortSpec_RtpplConst : all a. (a -> RtpplConst -> (a, RtpplConst)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem smapAccumL_RtpplPortSpec_RtpplConst f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPortSpec_RtpplConst : (RtpplConst -> RtpplConst) -> RtpplPortSpec -> RtpplPortSpec
sem smap_RtpplPortSpec_RtpplConst f =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplConst (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPortSpec_RtpplConst : all a. (a -> RtpplConst -> a) -> a -> RtpplPortSpec -> a
sem sfold_RtpplPortSpec_RtpplConst f acc =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplConst (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPortSpec_RtpplPort : all a. (a -> RtpplPort -> (a, RtpplPort)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem smapAccumL_RtpplPortSpec_RtpplPort f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPortSpec_RtpplPort : (RtpplPort -> RtpplPort) -> RtpplPortSpec -> RtpplPortSpec
sem smap_RtpplPortSpec_RtpplPort f =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplPort (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPortSpec_RtpplPort : all a. (a -> RtpplPort -> a) -> a -> RtpplPortSpec -> a
sem sfold_RtpplPortSpec_RtpplPort f acc =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplPort (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPortSpec_RtpplMain : all a. (a -> RtpplMain -> (a, RtpplMain)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem smapAccumL_RtpplPortSpec_RtpplMain f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPortSpec_RtpplMain : (RtpplMain -> RtpplMain) -> RtpplPortSpec -> RtpplPortSpec
sem smap_RtpplPortSpec_RtpplMain f =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplMain (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPortSpec_RtpplMain : all a. (a -> RtpplMain -> a) -> a -> RtpplPortSpec -> a
sem sfold_RtpplPortSpec_RtpplMain f acc =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplMain (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPortSpec_RtpplExt : all a. (a -> RtpplExt -> (a, RtpplExt)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem smapAccumL_RtpplPortSpec_RtpplExt f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPortSpec_RtpplExt : (RtpplExt -> RtpplExt) -> RtpplPortSpec -> RtpplPortSpec
sem smap_RtpplPortSpec_RtpplExt f =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplExt (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPortSpec_RtpplExt : all a. (a -> RtpplExt -> a) -> a -> RtpplPortSpec -> a
sem sfold_RtpplPortSpec_RtpplExt f acc =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplExt (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPortSpec_RtpplTask : all a. (a -> RtpplTask -> (a, RtpplTask)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem smapAccumL_RtpplPortSpec_RtpplTask f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPortSpec_RtpplTask : (RtpplTask -> RtpplTask) -> RtpplPortSpec -> RtpplPortSpec
sem smap_RtpplPortSpec_RtpplTask f =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplTask (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPortSpec_RtpplTask : all a. (a -> RtpplTask -> a) -> a -> RtpplPortSpec -> a
sem sfold_RtpplPortSpec_RtpplTask f acc =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplTask (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPortSpec_RtpplConnection : all a. (a -> RtpplConnection -> (a, RtpplConnection)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem smapAccumL_RtpplPortSpec_RtpplConnection f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPortSpec_RtpplConnection : (RtpplConnection -> RtpplConnection) -> RtpplPortSpec -> RtpplPortSpec
sem smap_RtpplPortSpec_RtpplConnection f =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplConnection (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPortSpec_RtpplConnection : all a. (a -> RtpplConnection -> a) -> a -> RtpplPortSpec -> a
sem sfold_RtpplPortSpec_RtpplConnection f acc =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplConnection (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem smapAccumL_RtpplPortSpec_RtpplPortSpec : all a. (a -> RtpplPortSpec -> (a, RtpplPortSpec)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem smapAccumL_RtpplPortSpec_RtpplPortSpec f acc =
  | x ->
    (acc, x)
  sem smap_RtpplPortSpec_RtpplPortSpec : (RtpplPortSpec -> RtpplPortSpec) -> RtpplPortSpec -> RtpplPortSpec
sem smap_RtpplPortSpec_RtpplPortSpec f =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplPortSpec (lam #var"".
          lam x.
            ({}, f x)) {} x).1
  sem sfold_RtpplPortSpec_RtpplPortSpec : all a. (a -> RtpplPortSpec -> a) -> a -> RtpplPortSpec -> a
sem sfold_RtpplPortSpec_RtpplPortSpec f acc =
  | x ->
    (smapAccumL_RtpplPortSpec_RtpplPortSpec (lam acc.
          lam x.
            (f acc x, x)) acc x).0
  sem get_RtpplProgram_info : RtpplProgram -> Info
sem get_RtpplProgram_info =
  sem set_RtpplProgram_info : Info -> RtpplProgram -> RtpplProgram
sem set_RtpplProgram_info val =
  sem mapAccum_RtpplProgram_info : all a. (a -> Info -> (a, Info)) -> a -> RtpplProgram -> (a, RtpplProgram)
sem mapAccum_RtpplProgram_info f acc =
  | target ->
    match
      f acc (get_RtpplProgram_info target)
    with
      (acc, val)
    in
    (acc, set_RtpplProgram_info val target)
  sem map_RtpplProgram_info : (Info -> Info) -> RtpplProgram -> RtpplProgram
sem map_RtpplProgram_info f =
  | target ->
    set_RtpplProgram_info (f (get_RtpplProgram_info target)) target
  sem get_RtpplTop_info : RtpplTop -> Info
sem get_RtpplTop_info =
  sem set_RtpplTop_info : Info -> RtpplTop -> RtpplTop
sem set_RtpplTop_info val =
  sem mapAccum_RtpplTop_info : all a. (a -> Info -> (a, Info)) -> a -> RtpplTop -> (a, RtpplTop)
sem mapAccum_RtpplTop_info f acc =
  | target ->
    match
      f acc (get_RtpplTop_info target)
    with
      (acc, val)
    in
    (acc, set_RtpplTop_info val target)
  sem map_RtpplTop_info : (Info -> Info) -> RtpplTop -> RtpplTop
sem map_RtpplTop_info f =
  | target ->
    set_RtpplTop_info (f (get_RtpplTop_info target)) target
  sem get_RtpplTopParams_info : RtpplTopParams -> Info
sem get_RtpplTopParams_info =
  sem set_RtpplTopParams_info : Info -> RtpplTopParams -> RtpplTopParams
sem set_RtpplTopParams_info val =
  sem mapAccum_RtpplTopParams_info : all a. (a -> Info -> (a, Info)) -> a -> RtpplTopParams -> (a, RtpplTopParams)
sem mapAccum_RtpplTopParams_info f acc =
  | target ->
    match
      f acc (get_RtpplTopParams_info target)
    with
      (acc, val)
    in
    (acc, set_RtpplTopParams_info val target)
  sem map_RtpplTopParams_info : (Info -> Info) -> RtpplTopParams -> RtpplTopParams
sem map_RtpplTopParams_info f =
  | target ->
    set_RtpplTopParams_info (f (get_RtpplTopParams_info target)) target
  sem get_RtpplStmt_info : RtpplStmt -> Info
sem get_RtpplStmt_info =
  sem set_RtpplStmt_info : Info -> RtpplStmt -> RtpplStmt
sem set_RtpplStmt_info val =
  sem mapAccum_RtpplStmt_info : all a. (a -> Info -> (a, Info)) -> a -> RtpplStmt -> (a, RtpplStmt)
sem mapAccum_RtpplStmt_info f acc =
  | target ->
    match
      f acc (get_RtpplStmt_info target)
    with
      (acc, val)
    in
    (acc, set_RtpplStmt_info val target)
  sem map_RtpplStmt_info : (Info -> Info) -> RtpplStmt -> RtpplStmt
sem map_RtpplStmt_info f =
  | target ->
    set_RtpplStmt_info (f (get_RtpplStmt_info target)) target
  sem get_RtpplStmtNoIdent_info : RtpplStmtNoIdent -> Info
sem get_RtpplStmtNoIdent_info =
  sem set_RtpplStmtNoIdent_info : Info -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem set_RtpplStmtNoIdent_info val =
  sem mapAccum_RtpplStmtNoIdent_info : all a. (a -> Info -> (a, Info)) -> a -> RtpplStmtNoIdent -> (a, RtpplStmtNoIdent)
sem mapAccum_RtpplStmtNoIdent_info f acc =
  | target ->
    match
      f acc (get_RtpplStmtNoIdent_info target)
    with
      (acc, val)
    in
    (acc, set_RtpplStmtNoIdent_info val target)
  sem map_RtpplStmtNoIdent_info : (Info -> Info) -> RtpplStmtNoIdent -> RtpplStmtNoIdent
sem map_RtpplStmtNoIdent_info f =
  | target ->
    set_RtpplStmtNoIdent_info (f (get_RtpplStmtNoIdent_info target)) target
  sem get_RtpplExpr_info : RtpplExpr -> Info
sem get_RtpplExpr_info =
  sem set_RtpplExpr_info : Info -> RtpplExpr -> RtpplExpr
sem set_RtpplExpr_info val =
  sem mapAccum_RtpplExpr_info : all a. (a -> Info -> (a, Info)) -> a -> RtpplExpr -> (a, RtpplExpr)
sem mapAccum_RtpplExpr_info f acc =
  | target ->
    match
      f acc (get_RtpplExpr_info target)
    with
      (acc, val)
    in
    (acc, set_RtpplExpr_info val target)
  sem map_RtpplExpr_info : (Info -> Info) -> RtpplExpr -> RtpplExpr
sem map_RtpplExpr_info f =
  | target ->
    set_RtpplExpr_info (f (get_RtpplExpr_info target)) target
  sem get_RtpplExprNoIdent_info : RtpplExprNoIdent -> Info
sem get_RtpplExprNoIdent_info =
  sem set_RtpplExprNoIdent_info : Info -> RtpplExprNoIdent -> RtpplExprNoIdent
sem set_RtpplExprNoIdent_info val =
  sem mapAccum_RtpplExprNoIdent_info : all a. (a -> Info -> (a, Info)) -> a -> RtpplExprNoIdent -> (a, RtpplExprNoIdent)
sem mapAccum_RtpplExprNoIdent_info f acc =
  | target ->
    match
      f acc (get_RtpplExprNoIdent_info target)
    with
      (acc, val)
    in
    (acc, set_RtpplExprNoIdent_info val target)
  sem map_RtpplExprNoIdent_info : (Info -> Info) -> RtpplExprNoIdent -> RtpplExprNoIdent
sem map_RtpplExprNoIdent_info f =
  | target ->
    set_RtpplExprNoIdent_info (f (get_RtpplExprNoIdent_info target)) target
  sem get_RtpplType_info : RtpplType -> Info
sem get_RtpplType_info =
  sem set_RtpplType_info : Info -> RtpplType -> RtpplType
sem set_RtpplType_info val =
  sem mapAccum_RtpplType_info : all a. (a -> Info -> (a, Info)) -> a -> RtpplType -> (a, RtpplType)
sem mapAccum_RtpplType_info f acc =
  | target ->
    match
      f acc (get_RtpplType_info target)
    with
      (acc, val)
    in
    (acc, set_RtpplType_info val target)
  sem map_RtpplType_info : (Info -> Info) -> RtpplType -> RtpplType
sem map_RtpplType_info f =
  | target ->
    set_RtpplType_info (f (get_RtpplType_info target)) target
  sem get_RtpplTypeNoIdent_info : RtpplTypeNoIdent -> Info
sem get_RtpplTypeNoIdent_info =
  sem set_RtpplTypeNoIdent_info : Info -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem set_RtpplTypeNoIdent_info val =
  sem mapAccum_RtpplTypeNoIdent_info : all a. (a -> Info -> (a, Info)) -> a -> RtpplTypeNoIdent -> (a, RtpplTypeNoIdent)
sem mapAccum_RtpplTypeNoIdent_info f acc =
  | target ->
    match
      f acc (get_RtpplTypeNoIdent_info target)
    with
      (acc, val)
    in
    (acc, set_RtpplTypeNoIdent_info val target)
  sem map_RtpplTypeNoIdent_info : (Info -> Info) -> RtpplTypeNoIdent -> RtpplTypeNoIdent
sem map_RtpplTypeNoIdent_info f =
  | target ->
    set_RtpplTypeNoIdent_info (f (get_RtpplTypeNoIdent_info target)) target
  sem get_RtpplConst_info : RtpplConst -> Info
sem get_RtpplConst_info =
  sem set_RtpplConst_info : Info -> RtpplConst -> RtpplConst
sem set_RtpplConst_info val =
  sem mapAccum_RtpplConst_info : all a. (a -> Info -> (a, Info)) -> a -> RtpplConst -> (a, RtpplConst)
sem mapAccum_RtpplConst_info f acc =
  | target ->
    match
      f acc (get_RtpplConst_info target)
    with
      (acc, val)
    in
    (acc, set_RtpplConst_info val target)
  sem map_RtpplConst_info : (Info -> Info) -> RtpplConst -> RtpplConst
sem map_RtpplConst_info f =
  | target ->
    set_RtpplConst_info (f (get_RtpplConst_info target)) target
  sem get_RtpplPort_info : RtpplPort -> Info
sem get_RtpplPort_info =
  sem set_RtpplPort_info : Info -> RtpplPort -> RtpplPort
sem set_RtpplPort_info val =
  sem mapAccum_RtpplPort_info : all a. (a -> Info -> (a, Info)) -> a -> RtpplPort -> (a, RtpplPort)
sem mapAccum_RtpplPort_info f acc =
  | target ->
    match
      f acc (get_RtpplPort_info target)
    with
      (acc, val)
    in
    (acc, set_RtpplPort_info val target)
  sem map_RtpplPort_info : (Info -> Info) -> RtpplPort -> RtpplPort
sem map_RtpplPort_info f =
  | target ->
    set_RtpplPort_info (f (get_RtpplPort_info target)) target
  sem get_RtpplMain_info : RtpplMain -> Info
sem get_RtpplMain_info =
  sem set_RtpplMain_info : Info -> RtpplMain -> RtpplMain
sem set_RtpplMain_info val =
  sem mapAccum_RtpplMain_info : all a. (a -> Info -> (a, Info)) -> a -> RtpplMain -> (a, RtpplMain)
sem mapAccum_RtpplMain_info f acc =
  | target ->
    match
      f acc (get_RtpplMain_info target)
    with
      (acc, val)
    in
    (acc, set_RtpplMain_info val target)
  sem map_RtpplMain_info : (Info -> Info) -> RtpplMain -> RtpplMain
sem map_RtpplMain_info f =
  | target ->
    set_RtpplMain_info (f (get_RtpplMain_info target)) target
  sem get_RtpplExt_info : RtpplExt -> Info
sem get_RtpplExt_info =
  sem set_RtpplExt_info : Info -> RtpplExt -> RtpplExt
sem set_RtpplExt_info val =
  sem mapAccum_RtpplExt_info : all a. (a -> Info -> (a, Info)) -> a -> RtpplExt -> (a, RtpplExt)
sem mapAccum_RtpplExt_info f acc =
  | target ->
    match
      f acc (get_RtpplExt_info target)
    with
      (acc, val)
    in
    (acc, set_RtpplExt_info val target)
  sem map_RtpplExt_info : (Info -> Info) -> RtpplExt -> RtpplExt
sem map_RtpplExt_info f =
  | target ->
    set_RtpplExt_info (f (get_RtpplExt_info target)) target
  sem get_RtpplTask_info : RtpplTask -> Info
sem get_RtpplTask_info =
  sem set_RtpplTask_info : Info -> RtpplTask -> RtpplTask
sem set_RtpplTask_info val =
  sem mapAccum_RtpplTask_info : all a. (a -> Info -> (a, Info)) -> a -> RtpplTask -> (a, RtpplTask)
sem mapAccum_RtpplTask_info f acc =
  | target ->
    match
      f acc (get_RtpplTask_info target)
    with
      (acc, val)
    in
    (acc, set_RtpplTask_info val target)
  sem map_RtpplTask_info : (Info -> Info) -> RtpplTask -> RtpplTask
sem map_RtpplTask_info f =
  | target ->
    set_RtpplTask_info (f (get_RtpplTask_info target)) target
  sem get_RtpplConnection_info : RtpplConnection -> Info
sem get_RtpplConnection_info =
  sem set_RtpplConnection_info : Info -> RtpplConnection -> RtpplConnection
sem set_RtpplConnection_info val =
  sem mapAccum_RtpplConnection_info : all a. (a -> Info -> (a, Info)) -> a -> RtpplConnection -> (a, RtpplConnection)
sem mapAccum_RtpplConnection_info f acc =
  | target ->
    match
      f acc (get_RtpplConnection_info target)
    with
      (acc, val)
    in
    (acc, set_RtpplConnection_info val target)
  sem map_RtpplConnection_info : (Info -> Info) -> RtpplConnection -> RtpplConnection
sem map_RtpplConnection_info f =
  | target ->
    set_RtpplConnection_info (f (get_RtpplConnection_info target)) target
  sem get_RtpplPortSpec_info : RtpplPortSpec -> Info
sem get_RtpplPortSpec_info =
  sem set_RtpplPortSpec_info : Info -> RtpplPortSpec -> RtpplPortSpec
sem set_RtpplPortSpec_info val =
  sem mapAccum_RtpplPortSpec_info : all a. (a -> Info -> (a, Info)) -> a -> RtpplPortSpec -> (a, RtpplPortSpec)
sem mapAccum_RtpplPortSpec_info f acc =
  | target ->
    match
      f acc (get_RtpplPortSpec_info target)
    with
      (acc, val)
    in
    (acc, set_RtpplPortSpec_info val target)
  sem map_RtpplPortSpec_info : (Info -> Info) -> RtpplPortSpec -> RtpplPortSpec
sem map_RtpplPortSpec_info f =
  | target ->
    set_RtpplPortSpec_info (f (get_RtpplPortSpec_info target)) target
end
lang ProgramRtpplProgramAst =
  RtpplBaseAst
  type ProgramRtpplProgramRecord =
    {info: Info, main: RtpplMain, tops: [RtpplTop]}
  syn RtpplProgram =
  | ProgramRtpplProgram ProgramRtpplProgramRecord
  sem smapAccumL_RtpplProgram_RtpplTop f acc =
  | ProgramRtpplProgram x ->
    match
      match
        let tops = x.tops in
        mapAccumL
          (lam acc1.
             lam x1: RtpplTop.
               f acc1 x1)
          acc
          tops
      with
        (acc, tops)
      in
      (acc, { x with tops = tops })
    with
      (acc, x)
    in
    (acc, ProgramRtpplProgram
        x)
  sem smapAccumL_RtpplProgram_RtpplMain f acc =
  | ProgramRtpplProgram x ->
    match
      match
        let main = x.main in
        f acc main
      with
        (acc, main)
      in
      (acc, { x with main = main })
    with
      (acc, x)
    in
    (acc, ProgramRtpplProgram
        x)
  sem get_RtpplProgram_info =
  | ProgramRtpplProgram target ->
    target.info
  sem set_RtpplProgram_info val =
  | ProgramRtpplProgram target ->
    ProgramRtpplProgram
      { target with info = val }
end
lang ConstantRtpplTopAst =
  RtpplBaseAst
  type ConstantRtpplTopRecord =
    {e: RtpplExpr, id: {i: Info, v: Name}, ty: RtpplType, info: Info}
  syn RtpplTop =
  | ConstantRtpplTop ConstantRtpplTopRecord
  sem smapAccumL_RtpplTop_RtpplExpr f acc =
  | ConstantRtpplTop x ->
    match
      match
        let e = x.e in
        f acc e
      with
        (acc, e)
      in
      (acc, { x with e = e })
    with
      (acc, x)
    in
    (acc, ConstantRtpplTop
        x)
  sem smapAccumL_RtpplTop_RtpplType f acc =
  | ConstantRtpplTop x ->
    match
      match
        let ty = x.ty in
        f acc ty
      with
        (acc, ty)
      in
      (acc, { x with ty = ty })
    with
      (acc, x)
    in
    (acc, ConstantRtpplTop
        x)
  sem get_RtpplTop_info =
  | ConstantRtpplTop target ->
    target.info
  sem set_RtpplTop_info val =
  | ConstantRtpplTop target ->
    ConstantRtpplTop
      { target with info = val }
end
lang TypeAliasRtpplTopAst =
  RtpplBaseAst
  type TypeAliasRtpplTopRecord =
    {id: {i: Info, v: Name}, ty: RtpplType, info: Info}
  syn RtpplTop =
  | TypeAliasRtpplTop TypeAliasRtpplTopRecord
  sem smapAccumL_RtpplTop_RtpplType f acc =
  | TypeAliasRtpplTop x ->
    match
      match
        let ty = x.ty in
        f acc ty
      with
        (acc, ty)
      in
      (acc, { x with ty = ty })
    with
      (acc, x)
    in
    (acc, TypeAliasRtpplTop
        x)
  sem get_RtpplTop_info =
  | TypeAliasRtpplTop target ->
    target.info
  sem set_RtpplTop_info val =
  | TypeAliasRtpplTop target ->
    TypeAliasRtpplTop
      { target with info = val }
end
lang FunctionDefRtpplTopAst =
  RtpplBaseAst
  type FunctionDefRtpplTopRecord =
    {id: {i: Info, v: Name}, ty: RtpplType, body: {ret: Option RtpplExpr, stmts: [RtpplStmt]}, info: Info, params: RtpplTopParams}
  syn RtpplTop =
  | FunctionDefRtpplTop FunctionDefRtpplTopRecord
  sem smapAccumL_RtpplTop_RtpplTopParams f acc =
  | FunctionDefRtpplTop x ->
    match
      match
        let params = x.params in
        f acc params
      with
        (acc, params)
      in
      (acc, { x with params = params })
    with
      (acc, x)
    in
    (acc, FunctionDefRtpplTop
        x)
  sem smapAccumL_RtpplTop_RtpplStmt f acc =
  | FunctionDefRtpplTop x ->
    match
      match
        let body = x.body in
        match
          let stmts = body.stmts in
          mapAccumL
            (lam acc1.
               lam x1: RtpplStmt.
                 f acc1 x1)
            acc
            stmts
        with
          (acc, stmts)
        in
        (acc, { body with stmts = stmts })
      with
        (acc, body)
      in
      (acc, { x with body = body })
    with
      (acc, x)
    in
    (acc, FunctionDefRtpplTop
        x)
  sem smapAccumL_RtpplTop_RtpplExpr f acc =
  | FunctionDefRtpplTop x ->
    match
      match
        let body = x.body in
        match
          let ret = body.ret in
          optionMapAccum
            (lam acc1.
               lam x1.
                 f acc1 x1)
            acc
            ret
        with
          (acc, ret)
        in
        (acc, { body with ret = ret })
      with
        (acc, body)
      in
      (acc, { x with body = body })
    with
      (acc, x)
    in
    (acc, FunctionDefRtpplTop
        x)
  sem smapAccumL_RtpplTop_RtpplType f acc =
  | FunctionDefRtpplTop x ->
    match
      match
        let ty = x.ty in
        f acc ty
      with
        (acc, ty)
      in
      (acc, { x with ty = ty })
    with
      (acc, x)
    in
    (acc, FunctionDefRtpplTop
        x)
  sem get_RtpplTop_info =
  | FunctionDefRtpplTop target ->
    target.info
  sem set_RtpplTop_info val =
  | FunctionDefRtpplTop target ->
    FunctionDefRtpplTop
      { target with info = val }
end
lang ModelDefRtpplTopAst =
  RtpplBaseAst
  type ModelDefRtpplTopRecord =
    {id: {i: Info, v: Name}, ty: RtpplType, body: {ret: Option RtpplExpr, stmts: [RtpplStmt]}, info: Info, params: RtpplTopParams}
  syn RtpplTop =
  | ModelDefRtpplTop ModelDefRtpplTopRecord
  sem smapAccumL_RtpplTop_RtpplTopParams f acc =
  | ModelDefRtpplTop x ->
    match
      match
        let params = x.params in
        f acc params
      with
        (acc, params)
      in
      (acc, { x with params = params })
    with
      (acc, x)
    in
    (acc, ModelDefRtpplTop
        x)
  sem smapAccumL_RtpplTop_RtpplStmt f acc =
  | ModelDefRtpplTop x ->
    match
      match
        let body = x.body in
        match
          let stmts = body.stmts in
          mapAccumL
            (lam acc1.
               lam x1: RtpplStmt.
                 f acc1 x1)
            acc
            stmts
        with
          (acc, stmts)
        in
        (acc, { body with stmts = stmts })
      with
        (acc, body)
      in
      (acc, { x with body = body })
    with
      (acc, x)
    in
    (acc, ModelDefRtpplTop
        x)
  sem smapAccumL_RtpplTop_RtpplExpr f acc =
  | ModelDefRtpplTop x ->
    match
      match
        let body = x.body in
        match
          let ret = body.ret in
          optionMapAccum
            (lam acc1.
               lam x1.
                 f acc1 x1)
            acc
            ret
        with
          (acc, ret)
        in
        (acc, { body with ret = ret })
      with
        (acc, body)
      in
      (acc, { x with body = body })
    with
      (acc, x)
    in
    (acc, ModelDefRtpplTop
        x)
  sem smapAccumL_RtpplTop_RtpplType f acc =
  | ModelDefRtpplTop x ->
    match
      match
        let ty = x.ty in
        f acc ty
      with
        (acc, ty)
      in
      (acc, { x with ty = ty })
    with
      (acc, x)
    in
    (acc, ModelDefRtpplTop
        x)
  sem get_RtpplTop_info =
  | ModelDefRtpplTop target ->
    target.info
  sem set_RtpplTop_info val =
  | ModelDefRtpplTop target ->
    ModelDefRtpplTop
      { target with info = val }
end
lang TemplateDefRtpplTopAst =
  RtpplBaseAst
  type TemplateDefRtpplTopRecord =
    {id: {i: Info, v: Name}, body: {body: [RtpplStmt], ports: [RtpplPort]}, info: Info, params: RtpplTopParams}
  syn RtpplTop =
  | TemplateDefRtpplTop TemplateDefRtpplTopRecord
  sem smapAccumL_RtpplTop_RtpplTopParams f acc =
  | TemplateDefRtpplTop x ->
    match
      match
        let params = x.params in
        f acc params
      with
        (acc, params)
      in
      (acc, { x with params = params })
    with
      (acc, x)
    in
    (acc, TemplateDefRtpplTop
        x)
  sem smapAccumL_RtpplTop_RtpplStmt f acc =
  | TemplateDefRtpplTop x ->
    match
      match
        let body = x.body in
        match
          let body1 = body.body in
          mapAccumL
            (lam acc1.
               lam x1: RtpplStmt.
                 f acc1 x1)
            acc
            body1
        with
          (acc, body1)
        in
        (acc, { body with body = body1 })
      with
        (acc, body)
      in
      (acc, { x with body = body })
    with
      (acc, x)
    in
    (acc, TemplateDefRtpplTop
        x)
  sem smapAccumL_RtpplTop_RtpplPort f acc =
  | TemplateDefRtpplTop x ->
    match
      match
        let body = x.body in
        match
          let ports = body.ports in
          mapAccumL
            (lam acc1.
               lam x1: RtpplPort.
                 f acc1 x1)
            acc
            ports
        with
          (acc, ports)
        in
        (acc, { body with ports = ports })
      with
        (acc, body)
      in
      (acc, { x with body = body })
    with
      (acc, x)
    in
    (acc, TemplateDefRtpplTop
        x)
  sem get_RtpplTop_info =
  | TemplateDefRtpplTop target ->
    target.info
  sem set_RtpplTop_info val =
  | TemplateDefRtpplTop target ->
    TemplateDefRtpplTop
      { target with info = val }
end
lang ParamsRtpplTopParamsAst =
  RtpplBaseAst
  type ParamsRtpplTopParamsRecord =
    {info: Info, params: [{id: {i: Info, v: Name}, ty: RtpplType}]}
  syn RtpplTopParams =
  | ParamsRtpplTopParams ParamsRtpplTopParamsRecord
  sem smapAccumL_RtpplTopParams_RtpplType f acc =
  | ParamsRtpplTopParams x ->
    match
      match
        let params = x.params in
        mapAccumL
          (lam acc1.
             lam x1: {id: {i: Info, v: Name}, ty: RtpplType}.
               match
                 let ty = x1.ty in
                 f acc1 ty
               with
                 (acc1, ty)
               in
               (acc1, { x1 with ty = ty }))
          acc
          params
      with
        (acc, params)
      in
      (acc, { x with params = params })
    with
      (acc, x)
    in
    (acc, ParamsRtpplTopParams
        x)
  sem get_RtpplTopParams_info =
  | ParamsRtpplTopParams target ->
    target.info
  sem set_RtpplTopParams_info val =
  | ParamsRtpplTopParams target ->
    ParamsRtpplTopParams
      { target with info = val }
end
lang InputRtpplPortAst =
  RtpplBaseAst
  type InputRtpplPortRecord =
    {id: {i: Info, v: String}, ty: RtpplType, info: Info}
  syn RtpplPort =
  | InputRtpplPort InputRtpplPortRecord
  sem smapAccumL_RtpplPort_RtpplType f acc =
  | InputRtpplPort x ->
    match
      match
        let ty = x.ty in
        f acc ty
      with
        (acc, ty)
      in
      (acc, { x with ty = ty })
    with
      (acc, x)
    in
    (acc, InputRtpplPort
        x)
  sem get_RtpplPort_info =
  | InputRtpplPort target ->
    target.info
  sem set_RtpplPort_info val =
  | InputRtpplPort target ->
    InputRtpplPort
      { target with info = val }
end
lang OutputRtpplPortAst =
  RtpplBaseAst
  type OutputRtpplPortRecord =
    {id: {i: Info, v: String}, ty: RtpplType, info: Info}
  syn RtpplPort =
  | OutputRtpplPort OutputRtpplPortRecord
  sem smapAccumL_RtpplPort_RtpplType f acc =
  | OutputRtpplPort x ->
    match
      match
        let ty = x.ty in
        f acc ty
      with
        (acc, ty)
      in
      (acc, { x with ty = ty })
    with
      (acc, x)
    in
    (acc, OutputRtpplPort
        x)
  sem get_RtpplPort_info =
  | OutputRtpplPort target ->
    target.info
  sem set_RtpplPort_info val =
  | OutputRtpplPort target ->
    OutputRtpplPort
      { target with info = val }
end
lang BindingRtpplStmtAst =
  RtpplBaseAst
  type BindingRtpplStmtRecord =
    {e: Option RtpplExpr, id: {i: Info, v: Name}, ty: Option RtpplType, info: Info}
  syn RtpplStmt =
  | BindingRtpplStmt BindingRtpplStmtRecord
  sem smapAccumL_RtpplStmt_RtpplExpr f acc =
  | BindingRtpplStmt x ->
    match
      match
        let e = x.e in
        optionMapAccum
          (lam acc1.
             lam x1.
               f acc1 x1)
          acc
          e
      with
        (acc, e)
      in
      (acc, { x with e = e })
    with
      (acc, x)
    in
    (acc, BindingRtpplStmt
        x)
  sem smapAccumL_RtpplStmt_RtpplType f acc =
  | BindingRtpplStmt x ->
    match
      match
        let ty = x.ty in
        optionMapAccum
          (lam acc1.
             lam x1.
               f acc1 x1)
          acc
          ty
      with
        (acc, ty)
      in
      (acc, { x with ty = ty })
    with
      (acc, x)
    in
    (acc, BindingRtpplStmt
        x)
  sem get_RtpplStmt_info =
  | BindingRtpplStmt target ->
    target.info
  sem set_RtpplStmt_info val =
  | BindingRtpplStmt target ->
    BindingRtpplStmt
      { target with info = val }
end
lang ObserveRtpplStmtAst =
  RtpplBaseAst
  type ObserveRtpplStmtRecord =
    {d: RtpplExpr, e: RtpplExpr, info: Info}
  syn RtpplStmt =
  | ObserveRtpplStmt ObserveRtpplStmtRecord
  sem smapAccumL_RtpplStmt_RtpplExpr f acc =
  | ObserveRtpplStmt x ->
    match
      match
        let d = x.d in
        f acc d
      with
        (acc, d)
      in
      match
          let e = x.e in
          f acc e
        with
          (acc, e)
        in
        (acc, { x with d = d, e = e })
    with
      (acc, x)
    in
    (acc, ObserveRtpplStmt
        x)
  sem get_RtpplStmt_info =
  | ObserveRtpplStmt target ->
    target.info
  sem set_RtpplStmt_info val =
  | ObserveRtpplStmt target ->
    ObserveRtpplStmt
      { target with info = val }
end
lang AssumeRtpplStmtAst =
  RtpplBaseAst
  type AssumeRtpplStmtRecord =
    {d: RtpplExpr, id: {i: Info, v: Name}, info: Info}
  syn RtpplStmt =
  | AssumeRtpplStmt AssumeRtpplStmtRecord
  sem smapAccumL_RtpplStmt_RtpplExpr f acc =
  | AssumeRtpplStmt x ->
    match
      match
        let d = x.d in
        f acc d
      with
        (acc, d)
      in
      (acc, { x with d = d })
    with
      (acc, x)
    in
    (acc, AssumeRtpplStmt
        x)
  sem get_RtpplStmt_info =
  | AssumeRtpplStmt target ->
    target.info
  sem set_RtpplStmt_info val =
  | AssumeRtpplStmt target ->
    AssumeRtpplStmt
      { target with info = val }
end
lang InferRtpplStmtAst =
  RtpplBaseAst
  type InferRtpplStmtRecord =
    {p: Option RtpplExpr, id: {i: Info, v: Name}, info: Info, model: RtpplExpr}
  syn RtpplStmt =
  | InferRtpplStmt InferRtpplStmtRecord
  sem smapAccumL_RtpplStmt_RtpplExpr f acc =
  | InferRtpplStmt x ->
    match
      match
        let p = x.p in
        optionMapAccum
          (lam acc1.
             lam x1.
               f acc1 x1)
          acc
          p
      with
        (acc, p)
      in
      match
          let model = x.model in
          f acc model
        with
          (acc, model)
        in
        (acc, { x with p = p, model = model })
    with
      (acc, x)
    in
    (acc, InferRtpplStmt
        x)
  sem get_RtpplStmt_info =
  | InferRtpplStmt target ->
    target.info
  sem set_RtpplStmt_info val =
  | InferRtpplStmt target ->
    InferRtpplStmt
      { target with info = val }
end
lang DegenerateRtpplStmtAst =
  RtpplBaseAst
  type DegenerateRtpplStmtRecord =
    {info: Info}
  syn RtpplStmt =
  | DegenerateRtpplStmt DegenerateRtpplStmtRecord
  sem get_RtpplStmt_info =
  | DegenerateRtpplStmt target ->
    target.info
  sem set_RtpplStmt_info val =
  | DegenerateRtpplStmt target ->
    DegenerateRtpplStmt
      { target with info = val }
end
lang ResampleRtpplStmtAst =
  RtpplBaseAst
  type ResampleRtpplStmtRecord =
    {info: Info}
  syn RtpplStmt =
  | ResampleRtpplStmt ResampleRtpplStmtRecord
  sem get_RtpplStmt_info =
  | ResampleRtpplStmt target ->
    target.info
  sem set_RtpplStmt_info val =
  | ResampleRtpplStmt target ->
    ResampleRtpplStmt
      { target with info = val }
end
lang ReadRtpplStmtAst =
  RtpplBaseAst
  type ReadRtpplStmtRecord =
    {dst: {i: Info, v: Name}, info: Info, port: {i: Info, v: String}, proj: Option {i: Info, v: String}}
  syn RtpplStmt =
  | ReadRtpplStmt ReadRtpplStmtRecord
  sem get_RtpplStmt_info =
  | ReadRtpplStmt target ->
    target.info
  sem set_RtpplStmt_info val =
  | ReadRtpplStmt target ->
    ReadRtpplStmt
      { target with info = val }
end
lang WriteRtpplStmtAst =
  RtpplBaseAst
  type WriteRtpplStmtRecord =
    {src: RtpplExpr, info: Info, port: {i: Info, v: String}, delay: Option RtpplExpr}
  syn RtpplStmt =
  | WriteRtpplStmt WriteRtpplStmtRecord
  sem smapAccumL_RtpplStmt_RtpplExpr f acc =
  | WriteRtpplStmt x ->
    match
      match
        let src = x.src in
        f acc src
      with
        (acc, src)
      in
      match
          let delay = x.delay in
          optionMapAccum
            (lam acc1.
               lam x1.
                 f acc1 x1)
            acc
            delay
        with
          (acc, delay)
        in
        (acc, { x with src = src, delay = delay })
    with
      (acc, x)
    in
    (acc, WriteRtpplStmt
        x)
  sem get_RtpplStmt_info =
  | WriteRtpplStmt target ->
    target.info
  sem set_RtpplStmt_info val =
  | WriteRtpplStmt target ->
    WriteRtpplStmt
      { target with info = val }
end
lang ConditionRtpplStmtAst =
  RtpplBaseAst
  type ConditionRtpplStmtRecord =
    {id: Option {i: Info, v: Name}, els: [RtpplStmt], thn: [RtpplStmt], cond: RtpplExpr, info: Info}
  syn RtpplStmt =
  | ConditionRtpplStmt ConditionRtpplStmtRecord
  sem smapAccumL_RtpplStmt_RtpplStmt f acc =
  | ConditionRtpplStmt x ->
    match
      match
        let els = x.els in
        mapAccumL
          (lam acc1.
             lam x1: RtpplStmt.
               f acc1 x1)
          acc
          els
      with
        (acc, els)
      in
      match
          let thn = x.thn in
          mapAccumL
            (lam acc2.
               lam x2: RtpplStmt.
                 f acc2 x2)
            acc
            thn
        with
          (acc, thn)
        in
        (acc, { x with els = els, thn = thn })
    with
      (acc, x)
    in
    (acc, ConditionRtpplStmt
        x)
  sem smapAccumL_RtpplStmt_RtpplExpr f acc =
  | ConditionRtpplStmt x ->
    match
      match
        let cond = x.cond in
        f acc cond
      with
        (acc, cond)
      in
      (acc, { x with cond = cond })
    with
      (acc, x)
    in
    (acc, ConditionRtpplStmt
        x)
  sem get_RtpplStmt_info =
  | ConditionRtpplStmt target ->
    target.info
  sem set_RtpplStmt_info val =
  | ConditionRtpplStmt target ->
    ConditionRtpplStmt
      { target with info = val }
end
lang DelayRtpplStmtAst =
  RtpplBaseAst
  type DelayRtpplStmtRecord =
    {ns: RtpplExpr, info: Info}
  syn RtpplStmt =
  | DelayRtpplStmt DelayRtpplStmtRecord
  sem smapAccumL_RtpplStmt_RtpplExpr f acc =
  | DelayRtpplStmt x ->
    match
      match
        let ns = x.ns in
        f acc ns
      with
        (acc, ns)
      in
      (acc, { x with ns = ns })
    with
      (acc, x)
    in
    (acc, DelayRtpplStmt
        x)
  sem get_RtpplStmt_info =
  | DelayRtpplStmt target ->
    target.info
  sem set_RtpplStmt_info val =
  | DelayRtpplStmt target ->
    DelayRtpplStmt
      { target with info = val }
end
lang ForLoopRtpplStmtAst =
  RtpplBaseAst
  type ForLoopRtpplStmtRecord =
    {e: RtpplExpr, id: {i: Info, v: Name}, upd: Option {i: Info, v: Name}, body: [RtpplStmt], info: Info}
  syn RtpplStmt =
  | ForLoopRtpplStmt ForLoopRtpplStmtRecord
  sem smapAccumL_RtpplStmt_RtpplStmt f acc =
  | ForLoopRtpplStmt x ->
    match
      match
        let body = x.body in
        mapAccumL
          (lam acc1.
             lam x1: RtpplStmt.
               f acc1 x1)
          acc
          body
      with
        (acc, body)
      in
      (acc, { x with body = body })
    with
      (acc, x)
    in
    (acc, ForLoopRtpplStmt
        x)
  sem smapAccumL_RtpplStmt_RtpplExpr f acc =
  | ForLoopRtpplStmt x ->
    match
      match
        let e = x.e in
        f acc e
      with
        (acc, e)
      in
      (acc, { x with e = e })
    with
      (acc, x)
    in
    (acc, ForLoopRtpplStmt
        x)
  sem get_RtpplStmt_info =
  | ForLoopRtpplStmt target ->
    target.info
  sem set_RtpplStmt_info val =
  | ForLoopRtpplStmt target ->
    ForLoopRtpplStmt
      { target with info = val }
end
lang WhileLoopRtpplStmtAst =
  RtpplBaseAst
  type WhileLoopRtpplStmtRecord =
    {upd: Option {i: Info, v: Name}, body: [RtpplStmt], cond: RtpplExpr, info: Info}
  syn RtpplStmt =
  | WhileLoopRtpplStmt WhileLoopRtpplStmtRecord
  sem smapAccumL_RtpplStmt_RtpplStmt f acc =
  | WhileLoopRtpplStmt x ->
    match
      match
        let body = x.body in
        mapAccumL
          (lam acc1.
             lam x1: RtpplStmt.
               f acc1 x1)
          acc
          body
      with
        (acc, body)
      in
      (acc, { x with body = body })
    with
      (acc, x)
    in
    (acc, WhileLoopRtpplStmt
        x)
  sem smapAccumL_RtpplStmt_RtpplExpr f acc =
  | WhileLoopRtpplStmt x ->
    match
      match
        let cond = x.cond in
        f acc cond
      with
        (acc, cond)
      in
      (acc, { x with cond = cond })
    with
      (acc, x)
    in
    (acc, WhileLoopRtpplStmt
        x)
  sem get_RtpplStmt_info =
  | WhileLoopRtpplStmt target ->
    target.info
  sem set_RtpplStmt_info val =
  | WhileLoopRtpplStmt target ->
    WhileLoopRtpplStmt
      { target with info = val }
end
lang IdentPlusStmtRtpplStmtAst =
  RtpplBaseAst
  type IdentPlusStmtRtpplStmtRecord =
    {id: {i: Info, v: Name}, info: Info, next: Option RtpplStmtNoIdent}
  syn RtpplStmt =
  | IdentPlusStmtRtpplStmt IdentPlusStmtRtpplStmtRecord
  sem smapAccumL_RtpplStmt_RtpplStmtNoIdent f acc =
  | IdentPlusStmtRtpplStmt x ->
    match
      match
        let next = x.next in
        optionMapAccum
          (lam acc1.
             lam x1.
               f acc1 x1)
          acc
          next
      with
        (acc, next)
      in
      (acc, { x with next = next })
    with
      (acc, x)
    in
    (acc, IdentPlusStmtRtpplStmt
        x)
  sem get_RtpplStmt_info =
  | IdentPlusStmtRtpplStmt target ->
    target.info
  sem set_RtpplStmt_info val =
  | IdentPlusStmtRtpplStmt target ->
    IdentPlusStmtRtpplStmt
      { target with info = val }
end
lang ReassignRtpplStmtNoIdentAst =
  RtpplBaseAst
  type ReassignRtpplStmtNoIdentRecord =
    {e: RtpplExpr, info: Info, proj: Option {i: Info, v: String}}
  syn RtpplStmtNoIdent =
  | ReassignRtpplStmtNoIdent ReassignRtpplStmtNoIdentRecord
  sem smapAccumL_RtpplStmtNoIdent_RtpplExpr f acc =
  | ReassignRtpplStmtNoIdent x ->
    match
      match
        let e = x.e in
        f acc e
      with
        (acc, e)
      in
      (acc, { x with e = e })
    with
      (acc, x)
    in
    (acc, ReassignRtpplStmtNoIdent
        x)
  sem get_RtpplStmtNoIdent_info =
  | ReassignRtpplStmtNoIdent target ->
    target.info
  sem set_RtpplStmtNoIdent_info val =
  | ReassignRtpplStmtNoIdent target ->
    ReassignRtpplStmtNoIdent
      { target with info = val }
end
lang FunctionCallSRtpplStmtNoIdentAst =
  RtpplBaseAst
  type FunctionCallSRtpplStmtNoIdentRecord =
    {args: [RtpplExpr], info: Info}
  syn RtpplStmtNoIdent =
  | FunctionCallSRtpplStmtNoIdent FunctionCallSRtpplStmtNoIdentRecord
  sem smapAccumL_RtpplStmtNoIdent_RtpplExpr f acc =
  | FunctionCallSRtpplStmtNoIdent x ->
    match
      match
        let args = x.args in
        mapAccumL
          (lam acc1.
             lam x1: RtpplExpr.
               f acc1 x1)
          acc
          args
      with
        (acc, args)
      in
      (acc, { x with args = args })
    with
      (acc, x)
    in
    (acc, FunctionCallSRtpplStmtNoIdent
        x)
  sem get_RtpplStmtNoIdent_info =
  | FunctionCallSRtpplStmtNoIdent target ->
    target.info
  sem set_RtpplStmtNoIdent_info val =
  | FunctionCallSRtpplStmtNoIdent target ->
    FunctionCallSRtpplStmtNoIdent
      { target with info = val }
end
lang IdentPlusExprRtpplExprAst =
  RtpplBaseAst
  type IdentPlusExprRtpplExprRecord =
    {id: {i: Info, v: Name}, info: Info, next: RtpplExprNoIdent}
  syn RtpplExpr =
  | IdentPlusExprRtpplExpr IdentPlusExprRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExprNoIdent f acc =
  | IdentPlusExprRtpplExpr x ->
    match
      match
        let next = x.next in
        f acc next
      with
        (acc, next)
      in
      (acc, { x with next = next })
    with
      (acc, x)
    in
    (acc, IdentPlusExprRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | IdentPlusExprRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | IdentPlusExprRtpplExpr target ->
    IdentPlusExprRtpplExpr
      { target with info = val }
end
lang VariableRtpplExprNoIdentAst =
  RtpplBaseAst
  type VariableRtpplExprNoIdentRecord =
    {info: Info}
  syn RtpplExprNoIdent =
  | VariableRtpplExprNoIdent VariableRtpplExprNoIdentRecord
  sem get_RtpplExprNoIdent_info =
  | VariableRtpplExprNoIdent target ->
    target.info
  sem set_RtpplExprNoIdent_info val =
  | VariableRtpplExprNoIdent target ->
    VariableRtpplExprNoIdent
      { target with info = val }
end
lang FunctionCallERtpplExprNoIdentAst =
  RtpplBaseAst
  type FunctionCallERtpplExprNoIdentRecord =
    {args: [RtpplExpr], info: Info}
  syn RtpplExprNoIdent =
  | FunctionCallERtpplExprNoIdent FunctionCallERtpplExprNoIdentRecord
  sem smapAccumL_RtpplExprNoIdent_RtpplExpr f acc =
  | FunctionCallERtpplExprNoIdent x ->
    match
      match
        let args = x.args in
        mapAccumL
          (lam acc1.
             lam x1: RtpplExpr.
               f acc1 x1)
          acc
          args
      with
        (acc, args)
      in
      (acc, { x with args = args })
    with
      (acc, x)
    in
    (acc, FunctionCallERtpplExprNoIdent
        x)
  sem get_RtpplExprNoIdent_info =
  | FunctionCallERtpplExprNoIdent target ->
    target.info
  sem set_RtpplExprNoIdent_info val =
  | FunctionCallERtpplExprNoIdent target ->
    FunctionCallERtpplExprNoIdent
      { target with info = val }
end
lang ProjectionRtpplExprNoIdentAst =
  RtpplBaseAst
  type ProjectionRtpplExprNoIdentRecord =
    {id: {i: Info, v: String}, info: Info}
  syn RtpplExprNoIdent =
  | ProjectionRtpplExprNoIdent ProjectionRtpplExprNoIdentRecord
  sem get_RtpplExprNoIdent_info =
  | ProjectionRtpplExprNoIdent target ->
    target.info
  sem set_RtpplExprNoIdent_info val =
  | ProjectionRtpplExprNoIdent target ->
    ProjectionRtpplExprNoIdent
      { target with info = val }
end
lang ArrayAccessRtpplExprAst =
  RtpplBaseAst
  type ArrayAccessRtpplExprRecord =
    {e: RtpplExpr, idx: RtpplExpr, info: Info}
  syn RtpplExpr =
  | ArrayAccessRtpplExpr ArrayAccessRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | ArrayAccessRtpplExpr x ->
    match
      match
        let e = x.e in
        f acc e
      with
        (acc, e)
      in
      match
          let idx = x.idx in
          f acc idx
        with
          (acc, idx)
        in
        (acc, { x with e = e, idx = idx })
    with
      (acc, x)
    in
    (acc, ArrayAccessRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | ArrayAccessRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | ArrayAccessRtpplExpr target ->
    ArrayAccessRtpplExpr
      { target with info = val }
end
lang ArrayLitRtpplExprAst =
  RtpplBaseAst
  type ArrayLitRtpplExprRecord =
    {info: Info, elems: [RtpplExpr]}
  syn RtpplExpr =
  | ArrayLitRtpplExpr ArrayLitRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | ArrayLitRtpplExpr x ->
    match
      match
        let elems = x.elems in
        mapAccumL
          (lam acc1.
             lam x1: RtpplExpr.
               f acc1 x1)
          acc
          elems
      with
        (acc, elems)
      in
      (acc, { x with elems = elems })
    with
      (acc, x)
    in
    (acc, ArrayLitRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | ArrayLitRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | ArrayLitRtpplExpr target ->
    ArrayLitRtpplExpr
      { target with info = val }
end
lang RecordLitRtpplExprAst =
  RtpplBaseAst
  type RecordLitRtpplExprRecord =
    {info: Info, fields: [{e: RtpplExpr, id: {i: Info, v: String}}]}
  syn RtpplExpr =
  | RecordLitRtpplExpr RecordLitRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | RecordLitRtpplExpr x ->
    match
      match
        let fields = x.fields in
        mapAccumL
          (lam acc1.
             lam x1: {e: RtpplExpr, id: {i: Info, v: String}}.
               match
                 let e = x1.e in
                 f acc1 e
               with
                 (acc1, e)
               in
               (acc1, { x1 with e = e }))
          acc
          fields
      with
        (acc, fields)
      in
      (acc, { x with fields = fields })
    with
      (acc, x)
    in
    (acc, RecordLitRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | RecordLitRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | RecordLitRtpplExpr target ->
    RecordLitRtpplExpr
      { target with info = val }
end
lang LiteralRtpplExprAst =
  RtpplBaseAst
  type LiteralRtpplExprRecord =
    {info: Info, const: RtpplConst}
  syn RtpplExpr =
  | LiteralRtpplExpr LiteralRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplConst f acc =
  | LiteralRtpplExpr x ->
    match
      match
        let const = x.const in
        f acc const
      with
        (acc, const)
      in
      (acc, { x with const = const })
    with
      (acc, x)
    in
    (acc, LiteralRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | LiteralRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | LiteralRtpplExpr target ->
    LiteralRtpplExpr
      { target with info = val }
end
lang LengthRtpplExprAst =
  RtpplBaseAst
  type LengthRtpplExprRecord =
    {e: RtpplExpr, info: Info}
  syn RtpplExpr =
  | LengthRtpplExpr LengthRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | LengthRtpplExpr x ->
    match
      match
        let e = x.e in
        f acc e
      with
        (acc, e)
      in
      (acc, { x with e = e })
    with
      (acc, x)
    in
    (acc, LengthRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | LengthRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | LengthRtpplExpr target ->
    LengthRtpplExpr
      { target with info = val }
end
lang DistSamplesRtpplExprAst =
  RtpplBaseAst
  type DistSamplesRtpplExprRecord =
    {e: RtpplExpr, info: Info}
  syn RtpplExpr =
  | DistSamplesRtpplExpr DistSamplesRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | DistSamplesRtpplExpr x ->
    match
      match
        let e = x.e in
        f acc e
      with
        (acc, e)
      in
      (acc, { x with e = e })
    with
      (acc, x)
    in
    (acc, DistSamplesRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | DistSamplesRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | DistSamplesRtpplExpr target ->
    DistSamplesRtpplExpr
      { target with info = val }
end
lang GaussianDistRtpplExprAst =
  RtpplBaseAst
  type GaussianDistRtpplExprRecord =
    {mu: RtpplExpr, info: Info, sigma: RtpplExpr}
  syn RtpplExpr =
  | GaussianDistRtpplExpr GaussianDistRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | GaussianDistRtpplExpr x ->
    match
      match
        let mu = x.mu in
        f acc mu
      with
        (acc, mu)
      in
      match
          let sigma = x.sigma in
          f acc sigma
        with
          (acc, sigma)
        in
        (acc, { x with mu = mu, sigma = sigma })
    with
      (acc, x)
    in
    (acc, GaussianDistRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | GaussianDistRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | GaussianDistRtpplExpr target ->
    GaussianDistRtpplExpr
      { target with info = val }
end
lang UniformDistRtpplExprAst =
  RtpplBaseAst
  type UniformDistRtpplExprRecord =
    {hi: RtpplExpr, lo: RtpplExpr, info: Info}
  syn RtpplExpr =
  | UniformDistRtpplExpr UniformDistRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | UniformDistRtpplExpr x ->
    match
      match
        let hi = x.hi in
        f acc hi
      with
        (acc, hi)
      in
      match
          let lo = x.lo in
          f acc lo
        with
          (acc, lo)
        in
        (acc, { x with hi = hi, lo = lo })
    with
      (acc, x)
    in
    (acc, UniformDistRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | UniformDistRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | UniformDistRtpplExpr target ->
    UniformDistRtpplExpr
      { target with info = val }
end
lang BernoulliDistRtpplExprAst =
  RtpplBaseAst
  type BernoulliDistRtpplExprRecord =
    {p: RtpplExpr, info: Info}
  syn RtpplExpr =
  | BernoulliDistRtpplExpr BernoulliDistRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | BernoulliDistRtpplExpr x ->
    match
      match
        let p = x.p in
        f acc p
      with
        (acc, p)
      in
      (acc, { x with p = p })
    with
      (acc, x)
    in
    (acc, BernoulliDistRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | BernoulliDistRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | BernoulliDistRtpplExpr target ->
    BernoulliDistRtpplExpr
      { target with info = val }
end
lang GammaDistRtpplExprAst =
  RtpplBaseAst
  type GammaDistRtpplExprRecord =
    {k: RtpplExpr, info: Info, theta: RtpplExpr}
  syn RtpplExpr =
  | GammaDistRtpplExpr GammaDistRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | GammaDistRtpplExpr x ->
    match
      match
        let k = x.k in
        f acc k
      with
        (acc, k)
      in
      match
          let theta = x.theta in
          f acc theta
        with
          (acc, theta)
        in
        (acc, { x with k = k, theta = theta })
    with
      (acc, x)
    in
    (acc, GammaDistRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | GammaDistRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | GammaDistRtpplExpr target ->
    GammaDistRtpplExpr
      { target with info = val }
end
lang AddRtpplExprAst =
  RtpplBaseAst
  type AddRtpplExprRecord =
    {info: Info, left: RtpplExpr, right: RtpplExpr}
  syn RtpplExpr =
  | AddRtpplExpr AddRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | AddRtpplExpr x ->
    match
      match
        let left = x.left in
        f acc left
      with
        (acc, left)
      in
      match
          let right = x.right in
          f acc right
        with
          (acc, right)
        in
        (acc, { x with left = left, right = right })
    with
      (acc, x)
    in
    (acc, AddRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | AddRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | AddRtpplExpr target ->
    AddRtpplExpr
      { target with info = val }
end
lang SubRtpplExprAst =
  RtpplBaseAst
  type SubRtpplExprRecord =
    {info: Info, left: RtpplExpr, right: RtpplExpr}
  syn RtpplExpr =
  | SubRtpplExpr SubRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | SubRtpplExpr x ->
    match
      match
        let left = x.left in
        f acc left
      with
        (acc, left)
      in
      match
          let right = x.right in
          f acc right
        with
          (acc, right)
        in
        (acc, { x with left = left, right = right })
    with
      (acc, x)
    in
    (acc, SubRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | SubRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | SubRtpplExpr target ->
    SubRtpplExpr
      { target with info = val }
end
lang MulRtpplExprAst =
  RtpplBaseAst
  type MulRtpplExprRecord =
    {info: Info, left: RtpplExpr, right: RtpplExpr}
  syn RtpplExpr =
  | MulRtpplExpr MulRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | MulRtpplExpr x ->
    match
      match
        let left = x.left in
        f acc left
      with
        (acc, left)
      in
      match
          let right = x.right in
          f acc right
        with
          (acc, right)
        in
        (acc, { x with left = left, right = right })
    with
      (acc, x)
    in
    (acc, MulRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | MulRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | MulRtpplExpr target ->
    MulRtpplExpr
      { target with info = val }
end
lang DivRtpplExprAst =
  RtpplBaseAst
  type DivRtpplExprRecord =
    {info: Info, left: RtpplExpr, right: RtpplExpr}
  syn RtpplExpr =
  | DivRtpplExpr DivRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | DivRtpplExpr x ->
    match
      match
        let left = x.left in
        f acc left
      with
        (acc, left)
      in
      match
          let right = x.right in
          f acc right
        with
          (acc, right)
        in
        (acc, { x with left = left, right = right })
    with
      (acc, x)
    in
    (acc, DivRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | DivRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | DivRtpplExpr target ->
    DivRtpplExpr
      { target with info = val }
end
lang EqRtpplExprAst =
  RtpplBaseAst
  type EqRtpplExprRecord =
    {info: Info, left: RtpplExpr, right: RtpplExpr}
  syn RtpplExpr =
  | EqRtpplExpr EqRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | EqRtpplExpr x ->
    match
      match
        let left = x.left in
        f acc left
      with
        (acc, left)
      in
      match
          let right = x.right in
          f acc right
        with
          (acc, right)
        in
        (acc, { x with left = left, right = right })
    with
      (acc, x)
    in
    (acc, EqRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | EqRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | EqRtpplExpr target ->
    EqRtpplExpr
      { target with info = val }
end
lang NeqRtpplExprAst =
  RtpplBaseAst
  type NeqRtpplExprRecord =
    {info: Info, left: RtpplExpr, right: RtpplExpr}
  syn RtpplExpr =
  | NeqRtpplExpr NeqRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | NeqRtpplExpr x ->
    match
      match
        let left = x.left in
        f acc left
      with
        (acc, left)
      in
      match
          let right = x.right in
          f acc right
        with
          (acc, right)
        in
        (acc, { x with left = left, right = right })
    with
      (acc, x)
    in
    (acc, NeqRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | NeqRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | NeqRtpplExpr target ->
    NeqRtpplExpr
      { target with info = val }
end
lang LtRtpplExprAst =
  RtpplBaseAst
  type LtRtpplExprRecord =
    {info: Info, left: RtpplExpr, right: RtpplExpr}
  syn RtpplExpr =
  | LtRtpplExpr LtRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | LtRtpplExpr x ->
    match
      match
        let left = x.left in
        f acc left
      with
        (acc, left)
      in
      match
          let right = x.right in
          f acc right
        with
          (acc, right)
        in
        (acc, { x with left = left, right = right })
    with
      (acc, x)
    in
    (acc, LtRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | LtRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | LtRtpplExpr target ->
    LtRtpplExpr
      { target with info = val }
end
lang GtRtpplExprAst =
  RtpplBaseAst
  type GtRtpplExprRecord =
    {info: Info, left: RtpplExpr, right: RtpplExpr}
  syn RtpplExpr =
  | GtRtpplExpr GtRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | GtRtpplExpr x ->
    match
      match
        let left = x.left in
        f acc left
      with
        (acc, left)
      in
      match
          let right = x.right in
          f acc right
        with
          (acc, right)
        in
        (acc, { x with left = left, right = right })
    with
      (acc, x)
    in
    (acc, GtRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | GtRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | GtRtpplExpr target ->
    GtRtpplExpr
      { target with info = val }
end
lang LeqRtpplExprAst =
  RtpplBaseAst
  type LeqRtpplExprRecord =
    {info: Info, left: RtpplExpr, right: RtpplExpr}
  syn RtpplExpr =
  | LeqRtpplExpr LeqRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | LeqRtpplExpr x ->
    match
      match
        let left = x.left in
        f acc left
      with
        (acc, left)
      in
      match
          let right = x.right in
          f acc right
        with
          (acc, right)
        in
        (acc, { x with left = left, right = right })
    with
      (acc, x)
    in
    (acc, LeqRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | LeqRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | LeqRtpplExpr target ->
    LeqRtpplExpr
      { target with info = val }
end
lang GeqRtpplExprAst =
  RtpplBaseAst
  type GeqRtpplExprRecord =
    {info: Info, left: RtpplExpr, right: RtpplExpr}
  syn RtpplExpr =
  | GeqRtpplExpr GeqRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | GeqRtpplExpr x ->
    match
      match
        let left = x.left in
        f acc left
      with
        (acc, left)
      in
      match
          let right = x.right in
          f acc right
        with
          (acc, right)
        in
        (acc, { x with left = left, right = right })
    with
      (acc, x)
    in
    (acc, GeqRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | GeqRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | GeqRtpplExpr target ->
    GeqRtpplExpr
      { target with info = val }
end
lang AndRtpplExprAst =
  RtpplBaseAst
  type AndRtpplExprRecord =
    {info: Info, left: RtpplExpr, right: RtpplExpr}
  syn RtpplExpr =
  | AndRtpplExpr AndRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | AndRtpplExpr x ->
    match
      match
        let left = x.left in
        f acc left
      with
        (acc, left)
      in
      match
          let right = x.right in
          f acc right
        with
          (acc, right)
        in
        (acc, { x with left = left, right = right })
    with
      (acc, x)
    in
    (acc, AndRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | AndRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | AndRtpplExpr target ->
    AndRtpplExpr
      { target with info = val }
end
lang OrRtpplExprAst =
  RtpplBaseAst
  type OrRtpplExprRecord =
    {info: Info, left: RtpplExpr, right: RtpplExpr}
  syn RtpplExpr =
  | OrRtpplExpr OrRtpplExprRecord
  sem smapAccumL_RtpplExpr_RtpplExpr f acc =
  | OrRtpplExpr x ->
    match
      match
        let left = x.left in
        f acc left
      with
        (acc, left)
      in
      match
          let right = x.right in
          f acc right
        with
          (acc, right)
        in
        (acc, { x with left = left, right = right })
    with
      (acc, x)
    in
    (acc, OrRtpplExpr
        x)
  sem get_RtpplExpr_info =
  | OrRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | OrRtpplExpr target ->
    OrRtpplExpr
      { target with info = val }
end
lang MainRtpplMainAst =
  RtpplBaseAst
  type MainRtpplMainRecord =
    {ext: [RtpplExt], info: Info, tasks: [RtpplTask], connections: [RtpplConnection]}
  syn RtpplMain =
  | MainRtpplMain MainRtpplMainRecord
  sem smapAccumL_RtpplMain_RtpplExt f acc =
  | MainRtpplMain x ->
    match
      match
        let ext = x.ext in
        mapAccumL
          (lam acc1.
             lam x1: RtpplExt.
               f acc1 x1)
          acc
          ext
      with
        (acc, ext)
      in
      (acc, { x with ext = ext })
    with
      (acc, x)
    in
    (acc, MainRtpplMain
        x)
  sem smapAccumL_RtpplMain_RtpplTask f acc =
  | MainRtpplMain x ->
    match
      match
        let tasks = x.tasks in
        mapAccumL
          (lam acc1.
             lam x1: RtpplTask.
               f acc1 x1)
          acc
          tasks
      with
        (acc, tasks)
      in
      (acc, { x with tasks = tasks })
    with
      (acc, x)
    in
    (acc, MainRtpplMain
        x)
  sem smapAccumL_RtpplMain_RtpplConnection f acc =
  | MainRtpplMain x ->
    match
      match
        let connections = x.connections in
        mapAccumL
          (lam acc1.
             lam x1: RtpplConnection.
               f acc1 x1)
          acc
          connections
      with
        (acc, connections)
      in
      (acc, { x with connections = connections })
    with
      (acc, x)
    in
    (acc, MainRtpplMain
        x)
  sem get_RtpplMain_info =
  | MainRtpplMain target ->
    target.info
  sem set_RtpplMain_info val =
  | MainRtpplMain target ->
    MainRtpplMain
      { target with info = val }
end
lang SensorRtpplExtAst =
  RtpplBaseAst
  type SensorRtpplExtRecord =
    {r: RtpplExpr, id: {i: Info, v: Name}, ty: RtpplType, info: Info}
  syn RtpplExt =
  | SensorRtpplExt SensorRtpplExtRecord
  sem smapAccumL_RtpplExt_RtpplExpr f acc =
  | SensorRtpplExt x ->
    match
      match
        let r = x.r in
        f acc r
      with
        (acc, r)
      in
      (acc, { x with r = r })
    with
      (acc, x)
    in
    (acc, SensorRtpplExt
        x)
  sem smapAccumL_RtpplExt_RtpplType f acc =
  | SensorRtpplExt x ->
    match
      match
        let ty = x.ty in
        f acc ty
      with
        (acc, ty)
      in
      (acc, { x with ty = ty })
    with
      (acc, x)
    in
    (acc, SensorRtpplExt
        x)
  sem get_RtpplExt_info =
  | SensorRtpplExt target ->
    target.info
  sem set_RtpplExt_info val =
  | SensorRtpplExt target ->
    SensorRtpplExt
      { target with info = val }
end
lang ActuatorRtpplExtAst =
  RtpplBaseAst
  type ActuatorRtpplExtRecord =
    {r: RtpplExpr, id: {i: Info, v: Name}, ty: RtpplType, info: Info}
  syn RtpplExt =
  | ActuatorRtpplExt ActuatorRtpplExtRecord
  sem smapAccumL_RtpplExt_RtpplExpr f acc =
  | ActuatorRtpplExt x ->
    match
      match
        let r = x.r in
        f acc r
      with
        (acc, r)
      in
      (acc, { x with r = r })
    with
      (acc, x)
    in
    (acc, ActuatorRtpplExt
        x)
  sem smapAccumL_RtpplExt_RtpplType f acc =
  | ActuatorRtpplExt x ->
    match
      match
        let ty = x.ty in
        f acc ty
      with
        (acc, ty)
      in
      (acc, { x with ty = ty })
    with
      (acc, x)
    in
    (acc, ActuatorRtpplExt
        x)
  sem get_RtpplExt_info =
  | ActuatorRtpplExt target ->
    target.info
  sem set_RtpplExt_info val =
  | ActuatorRtpplExt target ->
    ActuatorRtpplExt
      { target with info = val }
end
lang TaskRtpplTaskAst =
  RtpplBaseAst
  type TaskRtpplTaskRecord =
    {p: {i: Info, v: Int}, id: {i: Info, v: Name}, args: [RtpplExpr], info: Info, templateId: {i: Info, v: Name}}
  syn RtpplTask =
  | TaskRtpplTask TaskRtpplTaskRecord
  sem smapAccumL_RtpplTask_RtpplExpr f acc =
  | TaskRtpplTask x ->
    match
      match
        let args = x.args in
        mapAccumL
          (lam acc1.
             lam x1: RtpplExpr.
               f acc1 x1)
          acc
          args
      with
        (acc, args)
      in
      (acc, { x with args = args })
    with
      (acc, x)
    in
    (acc, TaskRtpplTask
        x)
  sem get_RtpplTask_info =
  | TaskRtpplTask target ->
    target.info
  sem set_RtpplTask_info val =
  | TaskRtpplTask target ->
    TaskRtpplTask
      { target with info = val }
end
lang ConnectionRtpplConnectionAst =
  RtpplBaseAst
  type ConnectionRtpplConnectionRecord =
    {to: RtpplPortSpec, from: RtpplPortSpec, info: Info}
  syn RtpplConnection =
  | ConnectionRtpplConnection ConnectionRtpplConnectionRecord
  sem smapAccumL_RtpplConnection_RtpplPortSpec f acc =
  | ConnectionRtpplConnection x ->
    match
      match
        let to = x.to in
        f acc to
      with
        (acc, to)
      in
      match
          let from = x.from in
          f acc from
        with
          (acc, from)
        in
        (acc, { x with to = to, from = from })
    with
      (acc, x)
    in
    (acc, ConnectionRtpplConnection
        x)
  sem get_RtpplConnection_info =
  | ConnectionRtpplConnection target ->
    target.info
  sem set_RtpplConnection_info val =
  | ConnectionRtpplConnection target ->
    ConnectionRtpplConnection
      { target with info = val }
end
lang PortSpecRtpplPortSpecAst =
  RtpplBaseAst
  type PortSpecRtpplPortSpecRecord =
    {id: Option {i: Info, v: String}, info: Info, port: {i: Info, v: Name}}
  syn RtpplPortSpec =
  | PortSpecRtpplPortSpec PortSpecRtpplPortSpecRecord
  sem get_RtpplPortSpec_info =
  | PortSpecRtpplPortSpec target ->
    target.info
  sem set_RtpplPortSpec_info val =
  | PortSpecRtpplPortSpec target ->
    PortSpecRtpplPortSpec
      { target with info = val }
end
lang IntRtpplTypeAst =
  RtpplBaseAst
  type IntRtpplTypeRecord =
    {info: Info}
  syn RtpplType =
  | IntRtpplType IntRtpplTypeRecord
  sem get_RtpplType_info =
  | IntRtpplType target ->
    target.info
  sem set_RtpplType_info val =
  | IntRtpplType target ->
    IntRtpplType
      { target with info = val }
end
lang FloatRtpplTypeAst =
  RtpplBaseAst
  type FloatRtpplTypeRecord =
    {info: Info}
  syn RtpplType =
  | FloatRtpplType FloatRtpplTypeRecord
  sem get_RtpplType_info =
  | FloatRtpplType target ->
    target.info
  sem set_RtpplType_info val =
  | FloatRtpplType target ->
    FloatRtpplType
      { target with info = val }
end
lang BoolRtpplTypeAst =
  RtpplBaseAst
  type BoolRtpplTypeRecord =
    {info: Info}
  syn RtpplType =
  | BoolRtpplType BoolRtpplTypeRecord
  sem get_RtpplType_info =
  | BoolRtpplType target ->
    target.info
  sem set_RtpplType_info val =
  | BoolRtpplType target ->
    BoolRtpplType
      { target with info = val }
end
lang StringRtpplTypeAst =
  RtpplBaseAst
  type StringRtpplTypeRecord =
    {info: Info}
  syn RtpplType =
  | StringRtpplType StringRtpplTypeRecord
  sem get_RtpplType_info =
  | StringRtpplType target ->
    target.info
  sem set_RtpplType_info val =
  | StringRtpplType target ->
    StringRtpplType
      { target with info = val }
end
lang UnitRtpplTypeAst =
  RtpplBaseAst
  type UnitRtpplTypeRecord =
    {info: Info}
  syn RtpplType =
  | UnitRtpplType UnitRtpplTypeRecord
  sem get_RtpplType_info =
  | UnitRtpplType target ->
    target.info
  sem set_RtpplType_info val =
  | UnitRtpplType target ->
    UnitRtpplType
      { target with info = val }
end
lang SeqRtpplTypeAst =
  RtpplBaseAst
  type SeqRtpplTypeRecord =
    {ty: RtpplType, info: Info}
  syn RtpplType =
  | SeqRtpplType SeqRtpplTypeRecord
  sem smapAccumL_RtpplType_RtpplType f acc =
  | SeqRtpplType x ->
    match
      match
        let ty = x.ty in
        f acc ty
      with
        (acc, ty)
      in
      (acc, { x with ty = ty })
    with
      (acc, x)
    in
    (acc, SeqRtpplType
        x)
  sem get_RtpplType_info =
  | SeqRtpplType target ->
    target.info
  sem set_RtpplType_info val =
  | SeqRtpplType target ->
    SeqRtpplType
      { target with info = val }
end
lang RecordRtpplTypeAst =
  RtpplBaseAst
  type RecordRtpplTypeRecord =
    {info: Info, fields: [{id: {i: Info, v: String}, ty: RtpplType}]}
  syn RtpplType =
  | RecordRtpplType RecordRtpplTypeRecord
  sem smapAccumL_RtpplType_RtpplType f acc =
  | RecordRtpplType x ->
    match
      match
        let fields = x.fields in
        mapAccumL
          (lam acc1.
             lam x1: {id: {i: Info, v: String}, ty: RtpplType}.
               match
                 let ty = x1.ty in
                 f acc1 ty
               with
                 (acc1, ty)
               in
               (acc1, { x1 with ty = ty }))
          acc
          fields
      with
        (acc, fields)
      in
      (acc, { x with fields = fields })
    with
      (acc, x)
    in
    (acc, RecordRtpplType
        x)
  sem get_RtpplType_info =
  | RecordRtpplType target ->
    target.info
  sem set_RtpplType_info val =
  | RecordRtpplType target ->
    RecordRtpplType
      { target with info = val }
end
lang FunctionRtpplTypeAst =
  RtpplBaseAst
  type FunctionRtpplTypeRecord =
    {to: RtpplType, from: RtpplType, info: Info}
  syn RtpplType =
  | FunctionRtpplType FunctionRtpplTypeRecord
  sem smapAccumL_RtpplType_RtpplType f acc =
  | FunctionRtpplType x ->
    match
      match
        let to = x.to in
        f acc to
      with
        (acc, to)
      in
      match
          let from = x.from in
          f acc from
        with
          (acc, from)
        in
        (acc, { x with to = to, from = from })
    with
      (acc, x)
    in
    (acc, FunctionRtpplType
        x)
  sem get_RtpplType_info =
  | FunctionRtpplType target ->
    target.info
  sem set_RtpplType_info val =
  | FunctionRtpplType target ->
    FunctionRtpplType
      { target with info = val }
end
lang DistRtpplTypeAst =
  RtpplBaseAst
  type DistRtpplTypeRecord =
    {ty: RtpplType, info: Info}
  syn RtpplType =
  | DistRtpplType DistRtpplTypeRecord
  sem smapAccumL_RtpplType_RtpplType f acc =
  | DistRtpplType x ->
    match
      match
        let ty = x.ty in
        f acc ty
      with
        (acc, ty)
      in
      (acc, { x with ty = ty })
    with
      (acc, x)
    in
    (acc, DistRtpplType
        x)
  sem get_RtpplType_info =
  | DistRtpplType target ->
    target.info
  sem set_RtpplType_info val =
  | DistRtpplType target ->
    DistRtpplType
      { target with info = val }
end
lang AliasRtpplTypeAst =
  RtpplBaseAst
  type AliasRtpplTypeRecord =
    {id: {i: Info, v: Name}, info: Info, next: RtpplTypeNoIdent}
  syn RtpplType =
  | AliasRtpplType AliasRtpplTypeRecord
  sem smapAccumL_RtpplType_RtpplTypeNoIdent f acc =
  | AliasRtpplType x ->
    match
      match
        let next = x.next in
        f acc next
      with
        (acc, next)
      in
      (acc, { x with next = next })
    with
      (acc, x)
    in
    (acc, AliasRtpplType
        x)
  sem get_RtpplType_info =
  | AliasRtpplType target ->
    target.info
  sem set_RtpplType_info val =
  | AliasRtpplType target ->
    AliasRtpplType
      { target with info = val }
end
lang DirectRtpplTypeNoIdentAst =
  RtpplBaseAst
  type DirectRtpplTypeNoIdentRecord =
    {info: Info}
  syn RtpplTypeNoIdent =
  | DirectRtpplTypeNoIdent DirectRtpplTypeNoIdentRecord
  sem get_RtpplTypeNoIdent_info =
  | DirectRtpplTypeNoIdent target ->
    target.info
  sem set_RtpplTypeNoIdent_info val =
  | DirectRtpplTypeNoIdent target ->
    DirectRtpplTypeNoIdent
      { target with info = val }
end
lang ApplicationRtpplTypeNoIdentAst =
  RtpplBaseAst
  type ApplicationRtpplTypeNoIdentRecord =
    {args: [RtpplType], info: Info}
  syn RtpplTypeNoIdent =
  | ApplicationRtpplTypeNoIdent ApplicationRtpplTypeNoIdentRecord
  sem smapAccumL_RtpplTypeNoIdent_RtpplType f acc =
  | ApplicationRtpplTypeNoIdent x ->
    match
      match
        let args = x.args in
        mapAccumL
          (lam acc1.
             lam x1: RtpplType.
               f acc1 x1)
          acc
          args
      with
        (acc, args)
      in
      (acc, { x with args = args })
    with
      (acc, x)
    in
    (acc, ApplicationRtpplTypeNoIdent
        x)
  sem get_RtpplTypeNoIdent_info =
  | ApplicationRtpplTypeNoIdent target ->
    target.info
  sem set_RtpplTypeNoIdent_info val =
  | ApplicationRtpplTypeNoIdent target ->
    ApplicationRtpplTypeNoIdent
      { target with info = val }
end
lang LitIntRtpplConstAst =
  RtpplBaseAst
  type LitIntRtpplConstRecord =
    {info: Info, value: {i: Info, v: Int}}
  syn RtpplConst =
  | LitIntRtpplConst LitIntRtpplConstRecord
  sem get_RtpplConst_info =
  | LitIntRtpplConst target ->
    target.info
  sem set_RtpplConst_info val =
  | LitIntRtpplConst target ->
    LitIntRtpplConst
      { target with info = val }
end
lang LitFloatRtpplConstAst =
  RtpplBaseAst
  type LitFloatRtpplConstRecord =
    {info: Info, value: {i: Info, v: Float}}
  syn RtpplConst =
  | LitFloatRtpplConst LitFloatRtpplConstRecord
  sem get_RtpplConst_info =
  | LitFloatRtpplConst target ->
    target.info
  sem set_RtpplConst_info val =
  | LitFloatRtpplConst target ->
    LitFloatRtpplConst
      { target with info = val }
end
lang LitBoolRtpplConstAst =
  RtpplBaseAst
  type LitBoolRtpplConstRecord =
    {info: Info, value: {i: Info, v: Bool}}
  syn RtpplConst =
  | LitBoolRtpplConst LitBoolRtpplConstRecord
  sem get_RtpplConst_info =
  | LitBoolRtpplConst target ->
    target.info
  sem set_RtpplConst_info val =
  | LitBoolRtpplConst target ->
    LitBoolRtpplConst
      { target with info = val }
end
lang LitStringRtpplConstAst =
  RtpplBaseAst
  type LitStringRtpplConstRecord =
    {info: Info, value: {i: Info, v: String}}
  syn RtpplConst =
  | LitStringRtpplConst LitStringRtpplConstRecord
  sem get_RtpplConst_info =
  | LitStringRtpplConst target ->
    target.info
  sem set_RtpplConst_info val =
  | LitStringRtpplConst target ->
    LitStringRtpplConst
      { target with info = val }
end
lang BadRtpplProgramAst =
  RtpplBaseAst
  type BadRtpplProgramRecord =
    {info: Info}
  syn RtpplProgram =
  | BadRtpplProgram BadRtpplProgramRecord
  sem get_RtpplProgram_info =
  | BadRtpplProgram target ->
    target.info
  sem set_RtpplProgram_info val =
  | BadRtpplProgram target ->
    BadRtpplProgram
      { target with info = val }
end
lang BadRtpplTopAst =
  RtpplBaseAst
  type BadRtpplTopRecord =
    {info: Info}
  syn RtpplTop =
  | BadRtpplTop BadRtpplTopRecord
  sem get_RtpplTop_info =
  | BadRtpplTop target ->
    target.info
  sem set_RtpplTop_info val =
  | BadRtpplTop target ->
    BadRtpplTop
      { target with info = val }
end
lang BadRtpplTopParamsAst =
  RtpplBaseAst
  type BadRtpplTopParamsRecord =
    {info: Info}
  syn RtpplTopParams =
  | BadRtpplTopParams BadRtpplTopParamsRecord
  sem get_RtpplTopParams_info =
  | BadRtpplTopParams target ->
    target.info
  sem set_RtpplTopParams_info val =
  | BadRtpplTopParams target ->
    BadRtpplTopParams
      { target with info = val }
end
lang BadRtpplStmtAst =
  RtpplBaseAst
  type BadRtpplStmtRecord =
    {info: Info}
  syn RtpplStmt =
  | BadRtpplStmt BadRtpplStmtRecord
  sem get_RtpplStmt_info =
  | BadRtpplStmt target ->
    target.info
  sem set_RtpplStmt_info val =
  | BadRtpplStmt target ->
    BadRtpplStmt
      { target with info = val }
end
lang BadRtpplStmtNoIdentAst =
  RtpplBaseAst
  type BadRtpplStmtNoIdentRecord =
    {info: Info}
  syn RtpplStmtNoIdent =
  | BadRtpplStmtNoIdent BadRtpplStmtNoIdentRecord
  sem get_RtpplStmtNoIdent_info =
  | BadRtpplStmtNoIdent target ->
    target.info
  sem set_RtpplStmtNoIdent_info val =
  | BadRtpplStmtNoIdent target ->
    BadRtpplStmtNoIdent
      { target with info = val }
end
lang BadRtpplExprAst =
  RtpplBaseAst
  type BadRtpplExprRecord =
    {info: Info}
  syn RtpplExpr =
  | BadRtpplExpr BadRtpplExprRecord
  sem get_RtpplExpr_info =
  | BadRtpplExpr target ->
    target.info
  sem set_RtpplExpr_info val =
  | BadRtpplExpr target ->
    BadRtpplExpr
      { target with info = val }
end
lang BadRtpplExprNoIdentAst =
  RtpplBaseAst
  type BadRtpplExprNoIdentRecord =
    {info: Info}
  syn RtpplExprNoIdent =
  | BadRtpplExprNoIdent BadRtpplExprNoIdentRecord
  sem get_RtpplExprNoIdent_info =
  | BadRtpplExprNoIdent target ->
    target.info
  sem set_RtpplExprNoIdent_info val =
  | BadRtpplExprNoIdent target ->
    BadRtpplExprNoIdent
      { target with info = val }
end
lang BadRtpplTypeAst =
  RtpplBaseAst
  type BadRtpplTypeRecord =
    {info: Info}
  syn RtpplType =
  | BadRtpplType BadRtpplTypeRecord
  sem get_RtpplType_info =
  | BadRtpplType target ->
    target.info
  sem set_RtpplType_info val =
  | BadRtpplType target ->
    BadRtpplType
      { target with info = val }
end
lang BadRtpplTypeNoIdentAst =
  RtpplBaseAst
  type BadRtpplTypeNoIdentRecord =
    {info: Info}
  syn RtpplTypeNoIdent =
  | BadRtpplTypeNoIdent BadRtpplTypeNoIdentRecord
  sem get_RtpplTypeNoIdent_info =
  | BadRtpplTypeNoIdent target ->
    target.info
  sem set_RtpplTypeNoIdent_info val =
  | BadRtpplTypeNoIdent target ->
    BadRtpplTypeNoIdent
      { target with info = val }
end
lang BadRtpplConstAst =
  RtpplBaseAst
  type BadRtpplConstRecord =
    {info: Info}
  syn RtpplConst =
  | BadRtpplConst BadRtpplConstRecord
  sem get_RtpplConst_info =
  | BadRtpplConst target ->
    target.info
  sem set_RtpplConst_info val =
  | BadRtpplConst target ->
    BadRtpplConst
      { target with info = val }
end
lang BadRtpplPortAst =
  RtpplBaseAst
  type BadRtpplPortRecord =
    {info: Info}
  syn RtpplPort =
  | BadRtpplPort BadRtpplPortRecord
  sem get_RtpplPort_info =
  | BadRtpplPort target ->
    target.info
  sem set_RtpplPort_info val =
  | BadRtpplPort target ->
    BadRtpplPort
      { target with info = val }
end
lang BadRtpplMainAst =
  RtpplBaseAst
  type BadRtpplMainRecord =
    {info: Info}
  syn RtpplMain =
  | BadRtpplMain BadRtpplMainRecord
  sem get_RtpplMain_info =
  | BadRtpplMain target ->
    target.info
  sem set_RtpplMain_info val =
  | BadRtpplMain target ->
    BadRtpplMain
      { target with info = val }
end
lang BadRtpplExtAst =
  RtpplBaseAst
  type BadRtpplExtRecord =
    {info: Info}
  syn RtpplExt =
  | BadRtpplExt BadRtpplExtRecord
  sem get_RtpplExt_info =
  | BadRtpplExt target ->
    target.info
  sem set_RtpplExt_info val =
  | BadRtpplExt target ->
    BadRtpplExt
      { target with info = val }
end
lang BadRtpplTaskAst =
  RtpplBaseAst
  type BadRtpplTaskRecord =
    {info: Info}
  syn RtpplTask =
  | BadRtpplTask BadRtpplTaskRecord
  sem get_RtpplTask_info =
  | BadRtpplTask target ->
    target.info
  sem set_RtpplTask_info val =
  | BadRtpplTask target ->
    BadRtpplTask
      { target with info = val }
end
lang BadRtpplConnectionAst =
  RtpplBaseAst
  type BadRtpplConnectionRecord =
    {info: Info}
  syn RtpplConnection =
  | BadRtpplConnection BadRtpplConnectionRecord
  sem get_RtpplConnection_info =
  | BadRtpplConnection target ->
    target.info
  sem set_RtpplConnection_info val =
  | BadRtpplConnection target ->
    BadRtpplConnection
      { target with info = val }
end
lang BadRtpplPortSpecAst =
  RtpplBaseAst
  type BadRtpplPortSpecRecord =
    {info: Info}
  syn RtpplPortSpec =
  | BadRtpplPortSpec BadRtpplPortSpecRecord
  sem get_RtpplPortSpec_info =
  | BadRtpplPortSpec target ->
    target.info
  sem set_RtpplPortSpec_info val =
  | BadRtpplPortSpec target ->
    BadRtpplPortSpec
      { target with info = val }
end
lang RtpplAst =
  ProgramRtpplProgramAst
  + ConstantRtpplTopAst
  + TypeAliasRtpplTopAst
  + FunctionDefRtpplTopAst
  + ModelDefRtpplTopAst
  + TemplateDefRtpplTopAst
  + ParamsRtpplTopParamsAst
  + InputRtpplPortAst
  + OutputRtpplPortAst
  + BindingRtpplStmtAst
  + ObserveRtpplStmtAst
  + AssumeRtpplStmtAst
  + InferRtpplStmtAst
  + DegenerateRtpplStmtAst
  + ResampleRtpplStmtAst
  + ReadRtpplStmtAst
  + WriteRtpplStmtAst
  + ConditionRtpplStmtAst
  + DelayRtpplStmtAst
  + ForLoopRtpplStmtAst
  + WhileLoopRtpplStmtAst
  + IdentPlusStmtRtpplStmtAst
  + ReassignRtpplStmtNoIdentAst
  + FunctionCallSRtpplStmtNoIdentAst
  + IdentPlusExprRtpplExprAst
  + VariableRtpplExprNoIdentAst
  + FunctionCallERtpplExprNoIdentAst
  + ProjectionRtpplExprNoIdentAst
  + ArrayAccessRtpplExprAst
  + ArrayLitRtpplExprAst
  + RecordLitRtpplExprAst
  + LiteralRtpplExprAst
  + LengthRtpplExprAst
  + DistSamplesRtpplExprAst
  + GaussianDistRtpplExprAst
  + UniformDistRtpplExprAst
  + BernoulliDistRtpplExprAst
  + GammaDistRtpplExprAst
  + AddRtpplExprAst
  + SubRtpplExprAst
  + MulRtpplExprAst
  + DivRtpplExprAst
  + EqRtpplExprAst
  + NeqRtpplExprAst
  + LtRtpplExprAst
  + GtRtpplExprAst
  + LeqRtpplExprAst
  + GeqRtpplExprAst
  + AndRtpplExprAst
  + OrRtpplExprAst
  + MainRtpplMainAst
  + SensorRtpplExtAst
  + ActuatorRtpplExtAst
  + TaskRtpplTaskAst
  + ConnectionRtpplConnectionAst
  + PortSpecRtpplPortSpecAst
  + IntRtpplTypeAst
  + FloatRtpplTypeAst
  + BoolRtpplTypeAst
  + StringRtpplTypeAst
  + UnitRtpplTypeAst
  + SeqRtpplTypeAst
  + RecordRtpplTypeAst
  + FunctionRtpplTypeAst
  + DistRtpplTypeAst
  + AliasRtpplTypeAst
  + DirectRtpplTypeNoIdentAst
  + ApplicationRtpplTypeNoIdentAst
  + LitIntRtpplConstAst
  + LitFloatRtpplConstAst
  + LitBoolRtpplConstAst
  + LitStringRtpplConstAst
  + BadRtpplProgramAst
  + BadRtpplTopAst
  + BadRtpplTopParamsAst
  + BadRtpplStmtAst
  + BadRtpplStmtNoIdentAst
  + BadRtpplExprAst
  + BadRtpplExprNoIdentAst
  + BadRtpplTypeAst
  + BadRtpplTypeNoIdentAst
  + BadRtpplConstAst
  + BadRtpplPortAst
  + BadRtpplMainAst
  + BadRtpplExtAst
  + BadRtpplTaskAst
  + BadRtpplConnectionAst
  + BadRtpplPortSpecAst
end
lang RtpplProgramOpBase =
  RtpplAst
  syn RtpplProgramOp lstyle rstyle =
  sem topAllowed_RtpplProgramOp : all lstyle. all rstyle. RtpplProgramOp lstyle rstyle -> Bool
sem topAllowed_RtpplProgramOp =
  | _ ->
    true
  sem leftAllowed_RtpplProgramOp : all lstyle. all style. all rstyle. {child: RtpplProgramOp lstyle rstyle, parent: RtpplProgramOp LOpen style} -> Bool
sem leftAllowed_RtpplProgramOp =
  | _ ->
    true
  sem rightAllowed_RtpplProgramOp : all style. all lstyle. all rstyle. {child: RtpplProgramOp lstyle rstyle, parent: RtpplProgramOp style ROpen} -> Bool
sem rightAllowed_RtpplProgramOp =
  | _ ->
    true
  sem groupingsAllowed_RtpplProgramOp : all lstyle. all rstyle. (RtpplProgramOp lstyle ROpen, RtpplProgramOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_RtpplProgramOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_RtpplProgramOp : all lstyle. all rstyle. RtpplProgramOp lstyle rstyle -> AllowedDirection
sem parenAllowed_RtpplProgramOp =
  | _ ->
    GEither
      {}
  sem getInfo_RtpplProgramOp : all lstyle. all rstyle. RtpplProgramOp lstyle rstyle -> Info
sem getInfo_RtpplProgramOp =
  sem getTerms_RtpplProgramOp : all lstyle. all rstyle. RtpplProgramOp lstyle rstyle -> [Info]
sem getTerms_RtpplProgramOp =
  sem unsplit_RtpplProgramOp : PermanentNode RtpplProgramOp -> (Info, RtpplProgram)
sem unsplit_RtpplProgramOp =
end
lang RtpplTopOpBase =
  RtpplAst
  syn RtpplTopOp lstyle rstyle =
  sem topAllowed_RtpplTopOp : all lstyle. all rstyle. RtpplTopOp lstyle rstyle -> Bool
sem topAllowed_RtpplTopOp =
  | _ ->
    true
  sem leftAllowed_RtpplTopOp : all lstyle. all style. all rstyle. {child: RtpplTopOp lstyle rstyle, parent: RtpplTopOp LOpen style} -> Bool
sem leftAllowed_RtpplTopOp =
  | _ ->
    true
  sem rightAllowed_RtpplTopOp : all style. all lstyle. all rstyle. {child: RtpplTopOp lstyle rstyle, parent: RtpplTopOp style ROpen} -> Bool
sem rightAllowed_RtpplTopOp =
  | _ ->
    true
  sem groupingsAllowed_RtpplTopOp : all lstyle. all rstyle. (RtpplTopOp lstyle ROpen, RtpplTopOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_RtpplTopOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_RtpplTopOp : all lstyle. all rstyle. RtpplTopOp lstyle rstyle -> AllowedDirection
sem parenAllowed_RtpplTopOp =
  | _ ->
    GEither
      {}
  sem getInfo_RtpplTopOp : all lstyle. all rstyle. RtpplTopOp lstyle rstyle -> Info
sem getInfo_RtpplTopOp =
  sem getTerms_RtpplTopOp : all lstyle. all rstyle. RtpplTopOp lstyle rstyle -> [Info]
sem getTerms_RtpplTopOp =
  sem unsplit_RtpplTopOp : PermanentNode RtpplTopOp -> (Info, RtpplTop)
sem unsplit_RtpplTopOp =
end
lang RtpplTopParamsOpBase =
  RtpplAst
  syn RtpplTopParamsOp lstyle rstyle =
  sem topAllowed_RtpplTopParamsOp : all lstyle. all rstyle. RtpplTopParamsOp lstyle rstyle -> Bool
sem topAllowed_RtpplTopParamsOp =
  | _ ->
    true
  sem leftAllowed_RtpplTopParamsOp : all lstyle. all style. all rstyle. {child: RtpplTopParamsOp lstyle rstyle, parent: RtpplTopParamsOp LOpen style} -> Bool
sem leftAllowed_RtpplTopParamsOp =
  | _ ->
    true
  sem rightAllowed_RtpplTopParamsOp : all style. all lstyle. all rstyle. {child: RtpplTopParamsOp lstyle rstyle, parent: RtpplTopParamsOp style ROpen} -> Bool
sem rightAllowed_RtpplTopParamsOp =
  | _ ->
    true
  sem groupingsAllowed_RtpplTopParamsOp : all lstyle. all rstyle. (RtpplTopParamsOp lstyle ROpen, RtpplTopParamsOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_RtpplTopParamsOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_RtpplTopParamsOp : all lstyle. all rstyle. RtpplTopParamsOp lstyle rstyle -> AllowedDirection
sem parenAllowed_RtpplTopParamsOp =
  | _ ->
    GEither
      {}
  sem getInfo_RtpplTopParamsOp : all lstyle. all rstyle. RtpplTopParamsOp lstyle rstyle -> Info
sem getInfo_RtpplTopParamsOp =
  sem getTerms_RtpplTopParamsOp : all lstyle. all rstyle. RtpplTopParamsOp lstyle rstyle -> [Info]
sem getTerms_RtpplTopParamsOp =
  sem unsplit_RtpplTopParamsOp : PermanentNode RtpplTopParamsOp -> (Info, RtpplTopParams)
sem unsplit_RtpplTopParamsOp =
end
lang RtpplStmtOpBase =
  RtpplAst
  syn RtpplStmtOp lstyle rstyle =
  sem topAllowed_RtpplStmtOp : all lstyle. all rstyle. RtpplStmtOp lstyle rstyle -> Bool
sem topAllowed_RtpplStmtOp =
  | _ ->
    true
  sem leftAllowed_RtpplStmtOp : all lstyle. all style. all rstyle. {child: RtpplStmtOp lstyle rstyle, parent: RtpplStmtOp LOpen style} -> Bool
sem leftAllowed_RtpplStmtOp =
  | _ ->
    true
  sem rightAllowed_RtpplStmtOp : all style. all lstyle. all rstyle. {child: RtpplStmtOp lstyle rstyle, parent: RtpplStmtOp style ROpen} -> Bool
sem rightAllowed_RtpplStmtOp =
  | _ ->
    true
  sem groupingsAllowed_RtpplStmtOp : all lstyle. all rstyle. (RtpplStmtOp lstyle ROpen, RtpplStmtOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_RtpplStmtOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_RtpplStmtOp : all lstyle. all rstyle. RtpplStmtOp lstyle rstyle -> AllowedDirection
sem parenAllowed_RtpplStmtOp =
  | _ ->
    GEither
      {}
  sem getInfo_RtpplStmtOp : all lstyle. all rstyle. RtpplStmtOp lstyle rstyle -> Info
sem getInfo_RtpplStmtOp =
  sem getTerms_RtpplStmtOp : all lstyle. all rstyle. RtpplStmtOp lstyle rstyle -> [Info]
sem getTerms_RtpplStmtOp =
  sem unsplit_RtpplStmtOp : PermanentNode RtpplStmtOp -> (Info, RtpplStmt)
sem unsplit_RtpplStmtOp =
end
lang RtpplStmtNoIdentOpBase =
  RtpplAst
  syn RtpplStmtNoIdentOp lstyle rstyle =
  sem topAllowed_RtpplStmtNoIdentOp : all lstyle. all rstyle. RtpplStmtNoIdentOp lstyle rstyle -> Bool
sem topAllowed_RtpplStmtNoIdentOp =
  | _ ->
    true
  sem leftAllowed_RtpplStmtNoIdentOp : all lstyle. all style. all rstyle. {child: RtpplStmtNoIdentOp lstyle rstyle, parent: RtpplStmtNoIdentOp LOpen style} -> Bool
sem leftAllowed_RtpplStmtNoIdentOp =
  | _ ->
    true
  sem rightAllowed_RtpplStmtNoIdentOp : all style. all lstyle. all rstyle. {child: RtpplStmtNoIdentOp lstyle rstyle, parent: RtpplStmtNoIdentOp style ROpen} -> Bool
sem rightAllowed_RtpplStmtNoIdentOp =
  | _ ->
    true
  sem groupingsAllowed_RtpplStmtNoIdentOp : all lstyle. all rstyle. (RtpplStmtNoIdentOp lstyle ROpen, RtpplStmtNoIdentOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_RtpplStmtNoIdentOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_RtpplStmtNoIdentOp : all lstyle. all rstyle. RtpplStmtNoIdentOp lstyle rstyle -> AllowedDirection
sem parenAllowed_RtpplStmtNoIdentOp =
  | _ ->
    GEither
      {}
  sem getInfo_RtpplStmtNoIdentOp : all lstyle. all rstyle. RtpplStmtNoIdentOp lstyle rstyle -> Info
sem getInfo_RtpplStmtNoIdentOp =
  sem getTerms_RtpplStmtNoIdentOp : all lstyle. all rstyle. RtpplStmtNoIdentOp lstyle rstyle -> [Info]
sem getTerms_RtpplStmtNoIdentOp =
  sem unsplit_RtpplStmtNoIdentOp : PermanentNode RtpplStmtNoIdentOp -> (Info, RtpplStmtNoIdent)
sem unsplit_RtpplStmtNoIdentOp =
end
lang RtpplExprOpBase =
  RtpplAst
  syn RtpplExprOp lstyle rstyle =
  sem topAllowed_RtpplExprOp : all lstyle. all rstyle. RtpplExprOp lstyle rstyle -> Bool
sem topAllowed_RtpplExprOp =
  | _ ->
    true
  sem leftAllowed_RtpplExprOp : all lstyle. all style. all rstyle. {child: RtpplExprOp lstyle rstyle, parent: RtpplExprOp LOpen style} -> Bool
sem leftAllowed_RtpplExprOp =
  | _ ->
    true
  sem rightAllowed_RtpplExprOp : all style. all lstyle. all rstyle. {child: RtpplExprOp lstyle rstyle, parent: RtpplExprOp style ROpen} -> Bool
sem rightAllowed_RtpplExprOp =
  | _ ->
    true
  sem groupingsAllowed_RtpplExprOp : all lstyle. all rstyle. (RtpplExprOp lstyle ROpen, RtpplExprOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_RtpplExprOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_RtpplExprOp : all lstyle. all rstyle. RtpplExprOp lstyle rstyle -> AllowedDirection
sem parenAllowed_RtpplExprOp =
  | _ ->
    GEither
      {}
  sem getInfo_RtpplExprOp : all lstyle. all rstyle. RtpplExprOp lstyle rstyle -> Info
sem getInfo_RtpplExprOp =
  sem getTerms_RtpplExprOp : all lstyle. all rstyle. RtpplExprOp lstyle rstyle -> [Info]
sem getTerms_RtpplExprOp =
  sem unsplit_RtpplExprOp : PermanentNode RtpplExprOp -> (Info, RtpplExpr)
sem unsplit_RtpplExprOp =
end
lang RtpplExprNoIdentOpBase =
  RtpplAst
  syn RtpplExprNoIdentOp lstyle rstyle =
  sem topAllowed_RtpplExprNoIdentOp : all lstyle. all rstyle. RtpplExprNoIdentOp lstyle rstyle -> Bool
sem topAllowed_RtpplExprNoIdentOp =
  | _ ->
    true
  sem leftAllowed_RtpplExprNoIdentOp : all lstyle. all style. all rstyle. {child: RtpplExprNoIdentOp lstyle rstyle, parent: RtpplExprNoIdentOp LOpen style} -> Bool
sem leftAllowed_RtpplExprNoIdentOp =
  | _ ->
    true
  sem rightAllowed_RtpplExprNoIdentOp : all style. all lstyle. all rstyle. {child: RtpplExprNoIdentOp lstyle rstyle, parent: RtpplExprNoIdentOp style ROpen} -> Bool
sem rightAllowed_RtpplExprNoIdentOp =
  | _ ->
    true
  sem groupingsAllowed_RtpplExprNoIdentOp : all lstyle. all rstyle. (RtpplExprNoIdentOp lstyle ROpen, RtpplExprNoIdentOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_RtpplExprNoIdentOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_RtpplExprNoIdentOp : all lstyle. all rstyle. RtpplExprNoIdentOp lstyle rstyle -> AllowedDirection
sem parenAllowed_RtpplExprNoIdentOp =
  | _ ->
    GEither
      {}
  sem getInfo_RtpplExprNoIdentOp : all lstyle. all rstyle. RtpplExprNoIdentOp lstyle rstyle -> Info
sem getInfo_RtpplExprNoIdentOp =
  sem getTerms_RtpplExprNoIdentOp : all lstyle. all rstyle. RtpplExprNoIdentOp lstyle rstyle -> [Info]
sem getTerms_RtpplExprNoIdentOp =
  sem unsplit_RtpplExprNoIdentOp : PermanentNode RtpplExprNoIdentOp -> (Info, RtpplExprNoIdent)
sem unsplit_RtpplExprNoIdentOp =
end
lang RtpplTypeOpBase =
  RtpplAst
  syn RtpplTypeOp lstyle rstyle =
  sem topAllowed_RtpplTypeOp : all lstyle. all rstyle. RtpplTypeOp lstyle rstyle -> Bool
sem topAllowed_RtpplTypeOp =
  | _ ->
    true
  sem leftAllowed_RtpplTypeOp : all lstyle. all style. all rstyle. {child: RtpplTypeOp lstyle rstyle, parent: RtpplTypeOp LOpen style} -> Bool
sem leftAllowed_RtpplTypeOp =
  | _ ->
    true
  sem rightAllowed_RtpplTypeOp : all style. all lstyle. all rstyle. {child: RtpplTypeOp lstyle rstyle, parent: RtpplTypeOp style ROpen} -> Bool
sem rightAllowed_RtpplTypeOp =
  | _ ->
    true
  sem groupingsAllowed_RtpplTypeOp : all lstyle. all rstyle. (RtpplTypeOp lstyle ROpen, RtpplTypeOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_RtpplTypeOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_RtpplTypeOp : all lstyle. all rstyle. RtpplTypeOp lstyle rstyle -> AllowedDirection
sem parenAllowed_RtpplTypeOp =
  | _ ->
    GEither
      {}
  sem getInfo_RtpplTypeOp : all lstyle. all rstyle. RtpplTypeOp lstyle rstyle -> Info
sem getInfo_RtpplTypeOp =
  sem getTerms_RtpplTypeOp : all lstyle. all rstyle. RtpplTypeOp lstyle rstyle -> [Info]
sem getTerms_RtpplTypeOp =
  sem unsplit_RtpplTypeOp : PermanentNode RtpplTypeOp -> (Info, RtpplType)
sem unsplit_RtpplTypeOp =
end
lang RtpplTypeNoIdentOpBase =
  RtpplAst
  syn RtpplTypeNoIdentOp lstyle rstyle =
  sem topAllowed_RtpplTypeNoIdentOp : all lstyle. all rstyle. RtpplTypeNoIdentOp lstyle rstyle -> Bool
sem topAllowed_RtpplTypeNoIdentOp =
  | _ ->
    true
  sem leftAllowed_RtpplTypeNoIdentOp : all lstyle. all style. all rstyle. {child: RtpplTypeNoIdentOp lstyle rstyle, parent: RtpplTypeNoIdentOp LOpen style} -> Bool
sem leftAllowed_RtpplTypeNoIdentOp =
  | _ ->
    true
  sem rightAllowed_RtpplTypeNoIdentOp : all style. all lstyle. all rstyle. {child: RtpplTypeNoIdentOp lstyle rstyle, parent: RtpplTypeNoIdentOp style ROpen} -> Bool
sem rightAllowed_RtpplTypeNoIdentOp =
  | _ ->
    true
  sem groupingsAllowed_RtpplTypeNoIdentOp : all lstyle. all rstyle. (RtpplTypeNoIdentOp lstyle ROpen, RtpplTypeNoIdentOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_RtpplTypeNoIdentOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_RtpplTypeNoIdentOp : all lstyle. all rstyle. RtpplTypeNoIdentOp lstyle rstyle -> AllowedDirection
sem parenAllowed_RtpplTypeNoIdentOp =
  | _ ->
    GEither
      {}
  sem getInfo_RtpplTypeNoIdentOp : all lstyle. all rstyle. RtpplTypeNoIdentOp lstyle rstyle -> Info
sem getInfo_RtpplTypeNoIdentOp =
  sem getTerms_RtpplTypeNoIdentOp : all lstyle. all rstyle. RtpplTypeNoIdentOp lstyle rstyle -> [Info]
sem getTerms_RtpplTypeNoIdentOp =
  sem unsplit_RtpplTypeNoIdentOp : PermanentNode RtpplTypeNoIdentOp -> (Info, RtpplTypeNoIdent)
sem unsplit_RtpplTypeNoIdentOp =
end
lang RtpplConstOpBase =
  RtpplAst
  syn RtpplConstOp lstyle rstyle =
  sem topAllowed_RtpplConstOp : all lstyle. all rstyle. RtpplConstOp lstyle rstyle -> Bool
sem topAllowed_RtpplConstOp =
  | _ ->
    true
  sem leftAllowed_RtpplConstOp : all lstyle. all style. all rstyle. {child: RtpplConstOp lstyle rstyle, parent: RtpplConstOp LOpen style} -> Bool
sem leftAllowed_RtpplConstOp =
  | _ ->
    true
  sem rightAllowed_RtpplConstOp : all style. all lstyle. all rstyle. {child: RtpplConstOp lstyle rstyle, parent: RtpplConstOp style ROpen} -> Bool
sem rightAllowed_RtpplConstOp =
  | _ ->
    true
  sem groupingsAllowed_RtpplConstOp : all lstyle. all rstyle. (RtpplConstOp lstyle ROpen, RtpplConstOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_RtpplConstOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_RtpplConstOp : all lstyle. all rstyle. RtpplConstOp lstyle rstyle -> AllowedDirection
sem parenAllowed_RtpplConstOp =
  | _ ->
    GEither
      {}
  sem getInfo_RtpplConstOp : all lstyle. all rstyle. RtpplConstOp lstyle rstyle -> Info
sem getInfo_RtpplConstOp =
  sem getTerms_RtpplConstOp : all lstyle. all rstyle. RtpplConstOp lstyle rstyle -> [Info]
sem getTerms_RtpplConstOp =
  sem unsplit_RtpplConstOp : PermanentNode RtpplConstOp -> (Info, RtpplConst)
sem unsplit_RtpplConstOp =
end
lang RtpplPortOpBase =
  RtpplAst
  syn RtpplPortOp lstyle rstyle =
  sem topAllowed_RtpplPortOp : all lstyle. all rstyle. RtpplPortOp lstyle rstyle -> Bool
sem topAllowed_RtpplPortOp =
  | _ ->
    true
  sem leftAllowed_RtpplPortOp : all lstyle. all style. all rstyle. {child: RtpplPortOp lstyle rstyle, parent: RtpplPortOp LOpen style} -> Bool
sem leftAllowed_RtpplPortOp =
  | _ ->
    true
  sem rightAllowed_RtpplPortOp : all style. all lstyle. all rstyle. {child: RtpplPortOp lstyle rstyle, parent: RtpplPortOp style ROpen} -> Bool
sem rightAllowed_RtpplPortOp =
  | _ ->
    true
  sem groupingsAllowed_RtpplPortOp : all lstyle. all rstyle. (RtpplPortOp lstyle ROpen, RtpplPortOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_RtpplPortOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_RtpplPortOp : all lstyle. all rstyle. RtpplPortOp lstyle rstyle -> AllowedDirection
sem parenAllowed_RtpplPortOp =
  | _ ->
    GEither
      {}
  sem getInfo_RtpplPortOp : all lstyle. all rstyle. RtpplPortOp lstyle rstyle -> Info
sem getInfo_RtpplPortOp =
  sem getTerms_RtpplPortOp : all lstyle. all rstyle. RtpplPortOp lstyle rstyle -> [Info]
sem getTerms_RtpplPortOp =
  sem unsplit_RtpplPortOp : PermanentNode RtpplPortOp -> (Info, RtpplPort)
sem unsplit_RtpplPortOp =
end
lang RtpplMainOpBase =
  RtpplAst
  syn RtpplMainOp lstyle rstyle =
  sem topAllowed_RtpplMainOp : all lstyle. all rstyle. RtpplMainOp lstyle rstyle -> Bool
sem topAllowed_RtpplMainOp =
  | _ ->
    true
  sem leftAllowed_RtpplMainOp : all lstyle. all style. all rstyle. {child: RtpplMainOp lstyle rstyle, parent: RtpplMainOp LOpen style} -> Bool
sem leftAllowed_RtpplMainOp =
  | _ ->
    true
  sem rightAllowed_RtpplMainOp : all style. all lstyle. all rstyle. {child: RtpplMainOp lstyle rstyle, parent: RtpplMainOp style ROpen} -> Bool
sem rightAllowed_RtpplMainOp =
  | _ ->
    true
  sem groupingsAllowed_RtpplMainOp : all lstyle. all rstyle. (RtpplMainOp lstyle ROpen, RtpplMainOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_RtpplMainOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_RtpplMainOp : all lstyle. all rstyle. RtpplMainOp lstyle rstyle -> AllowedDirection
sem parenAllowed_RtpplMainOp =
  | _ ->
    GEither
      {}
  sem getInfo_RtpplMainOp : all lstyle. all rstyle. RtpplMainOp lstyle rstyle -> Info
sem getInfo_RtpplMainOp =
  sem getTerms_RtpplMainOp : all lstyle. all rstyle. RtpplMainOp lstyle rstyle -> [Info]
sem getTerms_RtpplMainOp =
  sem unsplit_RtpplMainOp : PermanentNode RtpplMainOp -> (Info, RtpplMain)
sem unsplit_RtpplMainOp =
end
lang RtpplExtOpBase =
  RtpplAst
  syn RtpplExtOp lstyle rstyle =
  sem topAllowed_RtpplExtOp : all lstyle. all rstyle. RtpplExtOp lstyle rstyle -> Bool
sem topAllowed_RtpplExtOp =
  | _ ->
    true
  sem leftAllowed_RtpplExtOp : all lstyle. all style. all rstyle. {child: RtpplExtOp lstyle rstyle, parent: RtpplExtOp LOpen style} -> Bool
sem leftAllowed_RtpplExtOp =
  | _ ->
    true
  sem rightAllowed_RtpplExtOp : all style. all lstyle. all rstyle. {child: RtpplExtOp lstyle rstyle, parent: RtpplExtOp style ROpen} -> Bool
sem rightAllowed_RtpplExtOp =
  | _ ->
    true
  sem groupingsAllowed_RtpplExtOp : all lstyle. all rstyle. (RtpplExtOp lstyle ROpen, RtpplExtOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_RtpplExtOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_RtpplExtOp : all lstyle. all rstyle. RtpplExtOp lstyle rstyle -> AllowedDirection
sem parenAllowed_RtpplExtOp =
  | _ ->
    GEither
      {}
  sem getInfo_RtpplExtOp : all lstyle. all rstyle. RtpplExtOp lstyle rstyle -> Info
sem getInfo_RtpplExtOp =
  sem getTerms_RtpplExtOp : all lstyle. all rstyle. RtpplExtOp lstyle rstyle -> [Info]
sem getTerms_RtpplExtOp =
  sem unsplit_RtpplExtOp : PermanentNode RtpplExtOp -> (Info, RtpplExt)
sem unsplit_RtpplExtOp =
end
lang RtpplTaskOpBase =
  RtpplAst
  syn RtpplTaskOp lstyle rstyle =
  sem topAllowed_RtpplTaskOp : all lstyle. all rstyle. RtpplTaskOp lstyle rstyle -> Bool
sem topAllowed_RtpplTaskOp =
  | _ ->
    true
  sem leftAllowed_RtpplTaskOp : all lstyle. all style. all rstyle. {child: RtpplTaskOp lstyle rstyle, parent: RtpplTaskOp LOpen style} -> Bool
sem leftAllowed_RtpplTaskOp =
  | _ ->
    true
  sem rightAllowed_RtpplTaskOp : all style. all lstyle. all rstyle. {child: RtpplTaskOp lstyle rstyle, parent: RtpplTaskOp style ROpen} -> Bool
sem rightAllowed_RtpplTaskOp =
  | _ ->
    true
  sem groupingsAllowed_RtpplTaskOp : all lstyle. all rstyle. (RtpplTaskOp lstyle ROpen, RtpplTaskOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_RtpplTaskOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_RtpplTaskOp : all lstyle. all rstyle. RtpplTaskOp lstyle rstyle -> AllowedDirection
sem parenAllowed_RtpplTaskOp =
  | _ ->
    GEither
      {}
  sem getInfo_RtpplTaskOp : all lstyle. all rstyle. RtpplTaskOp lstyle rstyle -> Info
sem getInfo_RtpplTaskOp =
  sem getTerms_RtpplTaskOp : all lstyle. all rstyle. RtpplTaskOp lstyle rstyle -> [Info]
sem getTerms_RtpplTaskOp =
  sem unsplit_RtpplTaskOp : PermanentNode RtpplTaskOp -> (Info, RtpplTask)
sem unsplit_RtpplTaskOp =
end
lang RtpplConnectionOpBase =
  RtpplAst
  syn RtpplConnectionOp lstyle rstyle =
  sem topAllowed_RtpplConnectionOp : all lstyle. all rstyle. RtpplConnectionOp lstyle rstyle -> Bool
sem topAllowed_RtpplConnectionOp =
  | _ ->
    true
  sem leftAllowed_RtpplConnectionOp : all lstyle. all style. all rstyle. {child: RtpplConnectionOp lstyle rstyle, parent: RtpplConnectionOp LOpen style} -> Bool
sem leftAllowed_RtpplConnectionOp =
  | _ ->
    true
  sem rightAllowed_RtpplConnectionOp : all style. all lstyle. all rstyle. {child: RtpplConnectionOp lstyle rstyle, parent: RtpplConnectionOp style ROpen} -> Bool
sem rightAllowed_RtpplConnectionOp =
  | _ ->
    true
  sem groupingsAllowed_RtpplConnectionOp : all lstyle. all rstyle. (RtpplConnectionOp lstyle ROpen, RtpplConnectionOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_RtpplConnectionOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_RtpplConnectionOp : all lstyle. all rstyle. RtpplConnectionOp lstyle rstyle -> AllowedDirection
sem parenAllowed_RtpplConnectionOp =
  | _ ->
    GEither
      {}
  sem getInfo_RtpplConnectionOp : all lstyle. all rstyle. RtpplConnectionOp lstyle rstyle -> Info
sem getInfo_RtpplConnectionOp =
  sem getTerms_RtpplConnectionOp : all lstyle. all rstyle. RtpplConnectionOp lstyle rstyle -> [Info]
sem getTerms_RtpplConnectionOp =
  sem unsplit_RtpplConnectionOp : PermanentNode RtpplConnectionOp -> (Info, RtpplConnection)
sem unsplit_RtpplConnectionOp =
end
lang RtpplPortSpecOpBase =
  RtpplAst
  syn RtpplPortSpecOp lstyle rstyle =
  sem topAllowed_RtpplPortSpecOp : all lstyle. all rstyle. RtpplPortSpecOp lstyle rstyle -> Bool
sem topAllowed_RtpplPortSpecOp =
  | _ ->
    true
  sem leftAllowed_RtpplPortSpecOp : all lstyle. all style. all rstyle. {child: RtpplPortSpecOp lstyle rstyle, parent: RtpplPortSpecOp LOpen style} -> Bool
sem leftAllowed_RtpplPortSpecOp =
  | _ ->
    true
  sem rightAllowed_RtpplPortSpecOp : all style. all lstyle. all rstyle. {child: RtpplPortSpecOp lstyle rstyle, parent: RtpplPortSpecOp style ROpen} -> Bool
sem rightAllowed_RtpplPortSpecOp =
  | _ ->
    true
  sem groupingsAllowed_RtpplPortSpecOp : all lstyle. all rstyle. (RtpplPortSpecOp lstyle ROpen, RtpplPortSpecOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_RtpplPortSpecOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_RtpplPortSpecOp : all lstyle. all rstyle. RtpplPortSpecOp lstyle rstyle -> AllowedDirection
sem parenAllowed_RtpplPortSpecOp =
  | _ ->
    GEither
      {}
  sem getInfo_RtpplPortSpecOp : all lstyle. all rstyle. RtpplPortSpecOp lstyle rstyle -> Info
sem getInfo_RtpplPortSpecOp =
  sem getTerms_RtpplPortSpecOp : all lstyle. all rstyle. RtpplPortSpecOp lstyle rstyle -> [Info]
sem getTerms_RtpplPortSpecOp =
  sem unsplit_RtpplPortSpecOp : PermanentNode RtpplPortSpecOp -> (Info, RtpplPortSpec)
sem unsplit_RtpplPortSpecOp =
end
lang ProgramRtpplProgramOp =
  RtpplProgramOpBase
  + ProgramRtpplProgramAst
  syn RtpplProgramOp lstyle rstyle =
  | ProgramRtpplProgramOp {main: [RtpplMain], tops: [RtpplTop], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplProgramOp =
  | ProgramRtpplProgramOp x ->
    x.__br_info
  sem getTerms_RtpplProgramOp =
  | ProgramRtpplProgramOp x ->
    x.__br_terms
  sem unsplit_RtpplProgramOp =
  | AtomP {self = ProgramRtpplProgramOp x} ->
    (x.__br_info, ProgramRtpplProgram
      { tops = x.tops,
        main =
          match
            x.main
          with
            [ x1 ] ++ _
          in
          x1,
        info = x.__br_info })
end
lang ConstantRtpplTopOp =
  RtpplTopOpBase
  + ConstantRtpplTopAst
  syn RtpplTopOp lstyle rstyle =
  | ConstantRtpplTopOp {e: [RtpplExpr], id: [{i: Info, v: Name}], ty: [RtpplType], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTopOp =
  | ConstantRtpplTopOp x ->
    x.__br_info
  sem getTerms_RtpplTopOp =
  | ConstantRtpplTopOp x ->
    x.__br_terms
  sem unsplit_RtpplTopOp =
  | AtomP {self = ConstantRtpplTopOp x} ->
    (x.__br_info, ConstantRtpplTop
      { info = x.__br_info,
        e =
          match
            x.e
          with
            [ x1 ] ++ _
          in
          x1,
        id =
          match
            x.id
          with
            [ x2 ] ++ _
          in
          x2,
        ty =
          match
            x.ty
          with
            [ x3 ] ++ _
          in
          x3 })
end
lang TypeAliasRtpplTopOp =
  RtpplTopOpBase
  + TypeAliasRtpplTopAst
  syn RtpplTopOp lstyle rstyle =
  | TypeAliasRtpplTopOp {id: [{i: Info, v: Name}], ty: [RtpplType], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTopOp =
  | TypeAliasRtpplTopOp x ->
    x.__br_info
  sem getTerms_RtpplTopOp =
  | TypeAliasRtpplTopOp x ->
    x.__br_terms
  sem unsplit_RtpplTopOp =
  | AtomP {self = TypeAliasRtpplTopOp x} ->
    (x.__br_info, TypeAliasRtpplTop
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          in
          x1,
        ty =
          match
            x.ty
          with
            [ x2 ] ++ _
          in
          x2 })
end
lang FunctionDefRtpplTopOp =
  RtpplTopOpBase
  + FunctionDefRtpplTopAst
  syn RtpplTopOp lstyle rstyle =
  | FunctionDefRtpplTopOp {id: [{i: Info, v: Name}], ty: [RtpplType], body: [{ret: Option RtpplExpr, stmts: [RtpplStmt]}], params: [RtpplTopParams], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTopOp =
  | FunctionDefRtpplTopOp x ->
    x.__br_info
  sem getTerms_RtpplTopOp =
  | FunctionDefRtpplTopOp x ->
    x.__br_terms
  sem unsplit_RtpplTopOp =
  | AtomP {self = FunctionDefRtpplTopOp x} ->
    (x.__br_info, FunctionDefRtpplTop
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          in
          x1,
        ty =
          match
            x.ty
          with
            [ x2 ] ++ _
          in
          x2,
        body =
          match
            x.body
          with
            [ x3 ] ++ _
          in
          x3,
        params =
          match
            x.params
          with
            [ x4 ] ++ _
          in
          x4 })
end
lang ModelDefRtpplTopOp =
  RtpplTopOpBase
  + ModelDefRtpplTopAst
  syn RtpplTopOp lstyle rstyle =
  | ModelDefRtpplTopOp {id: [{i: Info, v: Name}], ty: [RtpplType], body: [{ret: Option RtpplExpr, stmts: [RtpplStmt]}], params: [RtpplTopParams], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTopOp =
  | ModelDefRtpplTopOp x ->
    x.__br_info
  sem getTerms_RtpplTopOp =
  | ModelDefRtpplTopOp x ->
    x.__br_terms
  sem unsplit_RtpplTopOp =
  | AtomP {self = ModelDefRtpplTopOp x} ->
    (x.__br_info, ModelDefRtpplTop
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          in
          x1,
        ty =
          match
            x.ty
          with
            [ x2 ] ++ _
          in
          x2,
        body =
          match
            x.body
          with
            [ x3 ] ++ _
          in
          x3,
        params =
          match
            x.params
          with
            [ x4 ] ++ _
          in
          x4 })
end
lang TemplateDefRtpplTopOp =
  RtpplTopOpBase
  + TemplateDefRtpplTopAst
  syn RtpplTopOp lstyle rstyle =
  | TemplateDefRtpplTopOp {id: [{i: Info, v: Name}], body: [{body: [RtpplStmt], ports: [RtpplPort]}], params: [RtpplTopParams], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTopOp =
  | TemplateDefRtpplTopOp x ->
    x.__br_info
  sem getTerms_RtpplTopOp =
  | TemplateDefRtpplTopOp x ->
    x.__br_terms
  sem unsplit_RtpplTopOp =
  | AtomP {self = TemplateDefRtpplTopOp x} ->
    (x.__br_info, TemplateDefRtpplTop
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          in
          x1,
        body =
          match
            x.body
          with
            [ x2 ] ++ _
          in
          x2,
        params =
          match
            x.params
          with
            [ x3 ] ++ _
          in
          x3 })
end
lang ParamsRtpplTopParamsOp =
  RtpplTopParamsOpBase
  + ParamsRtpplTopParamsAst
  syn RtpplTopParamsOp lstyle rstyle =
  | ParamsRtpplTopParamsOp {params: [{id: {i: Info, v: Name}, ty: RtpplType}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTopParamsOp =
  | ParamsRtpplTopParamsOp x ->
    x.__br_info
  sem getTerms_RtpplTopParamsOp =
  | ParamsRtpplTopParamsOp x ->
    x.__br_terms
  sem unsplit_RtpplTopParamsOp =
  | AtomP {self = ParamsRtpplTopParamsOp x} ->
    (x.__br_info, ParamsRtpplTopParams
      { info = x.__br_info, params = x.params })
end
lang InputRtpplPortOp =
  RtpplPortOpBase
  + InputRtpplPortAst
  syn RtpplPortOp lstyle rstyle =
  | InputRtpplPortOp {id: [{i: Info, v: String}], ty: [RtpplType], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplPortOp =
  | InputRtpplPortOp x ->
    x.__br_info
  sem getTerms_RtpplPortOp =
  | InputRtpplPortOp x ->
    x.__br_terms
  sem unsplit_RtpplPortOp =
  | AtomP {self = InputRtpplPortOp x} ->
    (x.__br_info, InputRtpplPort
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          in
          x1,
        ty =
          match
            x.ty
          with
            [ x2 ] ++ _
          in
          x2 })
end
lang OutputRtpplPortOp =
  RtpplPortOpBase
  + OutputRtpplPortAst
  syn RtpplPortOp lstyle rstyle =
  | OutputRtpplPortOp {id: [{i: Info, v: String}], ty: [RtpplType], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplPortOp =
  | OutputRtpplPortOp x ->
    x.__br_info
  sem getTerms_RtpplPortOp =
  | OutputRtpplPortOp x ->
    x.__br_terms
  sem unsplit_RtpplPortOp =
  | AtomP {self = OutputRtpplPortOp x} ->
    (x.__br_info, OutputRtpplPort
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          in
          x1,
        ty =
          match
            x.ty
          with
            [ x2 ] ++ _
          in
          x2 })
end
lang BindingRtpplStmtOp =
  RtpplStmtOpBase
  + BindingRtpplStmtAst
  syn RtpplStmtOp lstyle rstyle =
  | BindingRtpplStmtOp {e: [RtpplExpr], id: [{i: Info, v: Name}], ty: [RtpplType], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplStmtOp =
  | BindingRtpplStmtOp x ->
    x.__br_info
  sem getTerms_RtpplStmtOp =
  | BindingRtpplStmtOp x ->
    x.__br_terms
  sem unsplit_RtpplStmtOp =
  | AtomP {self = BindingRtpplStmtOp x} ->
    (x.__br_info, BindingRtpplStmt
      { info = x.__br_info,
        e =
          match
            x.e
          with
            [ x1 ] ++ _
          then
            Some
              x1
          else
            None
              {},
        id =
          match
            x.id
          with
            [ x2 ] ++ _
          in
          x2,
        ty =
          match
            x.ty
          with
            [ x3 ] ++ _
          then
            Some
              x3
          else
            None
              {} })
end
lang ObserveRtpplStmtOp =
  RtpplStmtOpBase
  + ObserveRtpplStmtAst
  syn RtpplStmtOp lstyle rstyle =
  | ObserveRtpplStmtOp {d: [RtpplExpr], e: [RtpplExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplStmtOp =
  | ObserveRtpplStmtOp x ->
    x.__br_info
  sem getTerms_RtpplStmtOp =
  | ObserveRtpplStmtOp x ->
    x.__br_terms
  sem unsplit_RtpplStmtOp =
  | AtomP {self = ObserveRtpplStmtOp x} ->
    (x.__br_info, ObserveRtpplStmt
      { info = x.__br_info,
        e =
          match
            x.e
          with
            [ x1 ] ++ _
          in
          x1,
        d =
          match
            x.d
          with
            [ x2 ] ++ _
          in
          x2 })
end
lang AssumeRtpplStmtOp =
  RtpplStmtOpBase
  + AssumeRtpplStmtAst
  syn RtpplStmtOp lstyle rstyle =
  | AssumeRtpplStmtOp {d: [RtpplExpr], id: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplStmtOp =
  | AssumeRtpplStmtOp x ->
    x.__br_info
  sem getTerms_RtpplStmtOp =
  | AssumeRtpplStmtOp x ->
    x.__br_terms
  sem unsplit_RtpplStmtOp =
  | AtomP {self = AssumeRtpplStmtOp x} ->
    (x.__br_info, AssumeRtpplStmt
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          in
          x1,
        d =
          match
            x.d
          with
            [ x2 ] ++ _
          in
          x2 })
end
lang InferRtpplStmtOp =
  RtpplStmtOpBase
  + InferRtpplStmtAst
  syn RtpplStmtOp lstyle rstyle =
  | InferRtpplStmtOp {p: [RtpplExpr], id: [{i: Info, v: Name}], model: [RtpplExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplStmtOp =
  | InferRtpplStmtOp x ->
    x.__br_info
  sem getTerms_RtpplStmtOp =
  | InferRtpplStmtOp x ->
    x.__br_terms
  sem unsplit_RtpplStmtOp =
  | AtomP {self = InferRtpplStmtOp x} ->
    (x.__br_info, InferRtpplStmt
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          in
          x1,
        p =
          match
            x.p
          with
            [ x2 ] ++ _
          then
            Some
              x2
          else
            None
              {},
        model =
          match
            x.model
          with
            [ x3 ] ++ _
          in
          x3 })
end
lang DegenerateRtpplStmtOp =
  RtpplStmtOpBase
  + DegenerateRtpplStmtAst
  syn RtpplStmtOp lstyle rstyle =
  | DegenerateRtpplStmtOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplStmtOp =
  | DegenerateRtpplStmtOp x ->
    x.__br_info
  sem getTerms_RtpplStmtOp =
  | DegenerateRtpplStmtOp x ->
    x.__br_terms
  sem unsplit_RtpplStmtOp =
  | AtomP {self = DegenerateRtpplStmtOp x} ->
    (x.__br_info, DegenerateRtpplStmt
      { info = x.__br_info })
end
lang ResampleRtpplStmtOp =
  RtpplStmtOpBase
  + ResampleRtpplStmtAst
  syn RtpplStmtOp lstyle rstyle =
  | ResampleRtpplStmtOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplStmtOp =
  | ResampleRtpplStmtOp x ->
    x.__br_info
  sem getTerms_RtpplStmtOp =
  | ResampleRtpplStmtOp x ->
    x.__br_terms
  sem unsplit_RtpplStmtOp =
  | AtomP {self = ResampleRtpplStmtOp x} ->
    (x.__br_info, ResampleRtpplStmt
      { info = x.__br_info })
end
lang ReadRtpplStmtOp =
  RtpplStmtOpBase
  + ReadRtpplStmtAst
  syn RtpplStmtOp lstyle rstyle =
  | ReadRtpplStmtOp {dst: [{i: Info, v: Name}], port: [{i: Info, v: String}], proj: [{i: Info, v: String}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplStmtOp =
  | ReadRtpplStmtOp x ->
    x.__br_info
  sem getTerms_RtpplStmtOp =
  | ReadRtpplStmtOp x ->
    x.__br_terms
  sem unsplit_RtpplStmtOp =
  | AtomP {self = ReadRtpplStmtOp x} ->
    (x.__br_info, ReadRtpplStmt
      { info = x.__br_info,
        proj =
          match
            x.proj
          with
            [ x1 ] ++ _
          then
            Some
              x1
          else
            None
              {},
        dst =
          match
            x.dst
          with
            [ x2 ] ++ _
          in
          x2,
        port =
          match
            x.port
          with
            [ x3 ] ++ _
          in
          x3 })
end
lang WriteRtpplStmtOp =
  RtpplStmtOpBase
  + WriteRtpplStmtAst
  syn RtpplStmtOp lstyle rstyle =
  | WriteRtpplStmtOp {src: [RtpplExpr], port: [{i: Info, v: String}], delay: [RtpplExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplStmtOp =
  | WriteRtpplStmtOp x ->
    x.__br_info
  sem getTerms_RtpplStmtOp =
  | WriteRtpplStmtOp x ->
    x.__br_terms
  sem unsplit_RtpplStmtOp =
  | AtomP {self = WriteRtpplStmtOp x} ->
    (x.__br_info, WriteRtpplStmt
      { info = x.__br_info,
        port =
          match
            x.port
          with
            [ x1 ] ++ _
          in
          x1,
        delay =
          match
            x.delay
          with
            [ x2 ] ++ _
          then
            Some
              x2
          else
            None
              {},
        src =
          match
            x.src
          with
            [ x3 ] ++ _
          in
          x3 })
end
lang ConditionRtpplStmtOp =
  RtpplStmtOpBase
  + ConditionRtpplStmtAst
  syn RtpplStmtOp lstyle rstyle =
  | ConditionRtpplStmtOp {id: [{i: Info, v: Name}], els: [RtpplStmt], thn: [RtpplStmt], cond: [RtpplExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplStmtOp =
  | ConditionRtpplStmtOp x ->
    x.__br_info
  sem getTerms_RtpplStmtOp =
  | ConditionRtpplStmtOp x ->
    x.__br_terms
  sem unsplit_RtpplStmtOp =
  | AtomP {self = ConditionRtpplStmtOp x} ->
    (x.__br_info, ConditionRtpplStmt
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          then
            Some
              x1
          else
            None
              {},
        thn = x.thn,
        els = x.els,
        cond =
          match
            x.cond
          with
            [ x2 ] ++ _
          in
          x2 })
end
lang DelayRtpplStmtOp =
  RtpplStmtOpBase
  + DelayRtpplStmtAst
  syn RtpplStmtOp lstyle rstyle =
  | DelayRtpplStmtOp {ns: [RtpplExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplStmtOp =
  | DelayRtpplStmtOp x ->
    x.__br_info
  sem getTerms_RtpplStmtOp =
  | DelayRtpplStmtOp x ->
    x.__br_terms
  sem unsplit_RtpplStmtOp =
  | AtomP {self = DelayRtpplStmtOp x} ->
    (x.__br_info, DelayRtpplStmt
      { info = x.__br_info,
        ns =
          match
            x.ns
          with
            [ x1 ] ++ _
          in
          x1 })
end
lang ForLoopRtpplStmtOp =
  RtpplStmtOpBase
  + ForLoopRtpplStmtAst
  syn RtpplStmtOp lstyle rstyle =
  | ForLoopRtpplStmtOp {e: [RtpplExpr], id: [{i: Info, v: Name}], upd: [{i: Info, v: Name}], body: [RtpplStmt], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplStmtOp =
  | ForLoopRtpplStmtOp x ->
    x.__br_info
  sem getTerms_RtpplStmtOp =
  | ForLoopRtpplStmtOp x ->
    x.__br_terms
  sem unsplit_RtpplStmtOp =
  | AtomP {self = ForLoopRtpplStmtOp x} ->
    (x.__br_info, ForLoopRtpplStmt
      { info = x.__br_info,
        e =
          match
            x.e
          with
            [ x1 ] ++ _
          in
          x1,
        id =
          match
            x.id
          with
            [ x2 ] ++ _
          in
          x2,
        body = x.body,
        upd =
          match
            x.upd
          with
            [ x3 ] ++ _
          then
            Some
              x3
          else
            None
              {} })
end
lang WhileLoopRtpplStmtOp =
  RtpplStmtOpBase
  + WhileLoopRtpplStmtAst
  syn RtpplStmtOp lstyle rstyle =
  | WhileLoopRtpplStmtOp {upd: [{i: Info, v: Name}], body: [RtpplStmt], cond: [RtpplExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplStmtOp =
  | WhileLoopRtpplStmtOp x ->
    x.__br_info
  sem getTerms_RtpplStmtOp =
  | WhileLoopRtpplStmtOp x ->
    x.__br_terms
  sem unsplit_RtpplStmtOp =
  | AtomP {self = WhileLoopRtpplStmtOp x} ->
    (x.__br_info, WhileLoopRtpplStmt
      { info = x.__br_info,
        body = x.body,
        cond =
          match
            x.cond
          with
            [ x1 ] ++ _
          in
          x1,
        upd =
          match
            x.upd
          with
            [ x2 ] ++ _
          then
            Some
              x2
          else
            None
              {} })
end
lang IdentPlusStmtRtpplStmtOp =
  RtpplStmtOpBase
  + IdentPlusStmtRtpplStmtAst
  syn RtpplStmtOp lstyle rstyle =
  | IdentPlusStmtRtpplStmtOp {id: [{i: Info, v: Name}], next: [RtpplStmtNoIdent], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplStmtOp =
  | IdentPlusStmtRtpplStmtOp x ->
    x.__br_info
  sem getTerms_RtpplStmtOp =
  | IdentPlusStmtRtpplStmtOp x ->
    x.__br_terms
  sem unsplit_RtpplStmtOp =
  | AtomP {self = IdentPlusStmtRtpplStmtOp x} ->
    (x.__br_info, IdentPlusStmtRtpplStmt
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          in
          x1,
        next =
          match
            x.next
          with
            [ x2 ] ++ _
          then
            Some
              x2
          else
            None
              {} })
end
lang ReassignRtpplStmtNoIdentOp =
  RtpplStmtNoIdentOpBase
  + ReassignRtpplStmtNoIdentAst
  syn RtpplStmtNoIdentOp lstyle rstyle =
  | ReassignRtpplStmtNoIdentOp {e: [RtpplExpr], proj: [{i: Info, v: String}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplStmtNoIdentOp =
  | ReassignRtpplStmtNoIdentOp x ->
    x.__br_info
  sem getTerms_RtpplStmtNoIdentOp =
  | ReassignRtpplStmtNoIdentOp x ->
    x.__br_terms
  sem unsplit_RtpplStmtNoIdentOp =
  | AtomP {self = ReassignRtpplStmtNoIdentOp x} ->
    (x.__br_info, ReassignRtpplStmtNoIdent
      { info = x.__br_info,
        e =
          match
            x.e
          with
            [ x1 ] ++ _
          in
          x1,
        proj =
          match
            x.proj
          with
            [ x2 ] ++ _
          then
            Some
              x2
          else
            None
              {} })
end
lang FunctionCallSRtpplStmtNoIdentOp =
  RtpplStmtNoIdentOpBase
  + FunctionCallSRtpplStmtNoIdentAst
  syn RtpplStmtNoIdentOp lstyle rstyle =
  | FunctionCallSRtpplStmtNoIdentOp {args: [RtpplExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplStmtNoIdentOp =
  | FunctionCallSRtpplStmtNoIdentOp x ->
    x.__br_info
  sem getTerms_RtpplStmtNoIdentOp =
  | FunctionCallSRtpplStmtNoIdentOp x ->
    x.__br_terms
  sem unsplit_RtpplStmtNoIdentOp =
  | AtomP {self = FunctionCallSRtpplStmtNoIdentOp x} ->
    (x.__br_info, FunctionCallSRtpplStmtNoIdent
      { info = x.__br_info, args = x.args })
end
lang IdentPlusExprRtpplExprOp =
  RtpplExprOpBase
  + IdentPlusExprRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | IdentPlusExprRtpplExprOp {id: [{i: Info, v: Name}], next: [RtpplExprNoIdent], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | IdentPlusExprRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | IdentPlusExprRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | AtomP {self = IdentPlusExprRtpplExprOp x} ->
    (x.__br_info, IdentPlusExprRtpplExpr
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          in
          x1,
        next =
          match
            x.next
          with
            [ x2 ] ++ _
          in
          x2 })
end
lang VariableRtpplExprNoIdentOp =
  RtpplExprNoIdentOpBase
  + VariableRtpplExprNoIdentAst
  syn RtpplExprNoIdentOp lstyle rstyle =
  | VariableRtpplExprNoIdentOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprNoIdentOp =
  | VariableRtpplExprNoIdentOp x ->
    x.__br_info
  sem getTerms_RtpplExprNoIdentOp =
  | VariableRtpplExprNoIdentOp x ->
    x.__br_terms
  sem unsplit_RtpplExprNoIdentOp =
  | AtomP {self = VariableRtpplExprNoIdentOp x} ->
    (x.__br_info, VariableRtpplExprNoIdent
      { info = x.__br_info })
end
lang FunctionCallERtpplExprNoIdentOp =
  RtpplExprNoIdentOpBase
  + FunctionCallERtpplExprNoIdentAst
  syn RtpplExprNoIdentOp lstyle rstyle =
  | FunctionCallERtpplExprNoIdentOp {args: [RtpplExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprNoIdentOp =
  | FunctionCallERtpplExprNoIdentOp x ->
    x.__br_info
  sem getTerms_RtpplExprNoIdentOp =
  | FunctionCallERtpplExprNoIdentOp x ->
    x.__br_terms
  sem unsplit_RtpplExprNoIdentOp =
  | AtomP {self = FunctionCallERtpplExprNoIdentOp x} ->
    (x.__br_info, FunctionCallERtpplExprNoIdent
      { info = x.__br_info, args = x.args })
end
lang ProjectionRtpplExprNoIdentOp =
  RtpplExprNoIdentOpBase
  + ProjectionRtpplExprNoIdentAst
  syn RtpplExprNoIdentOp lstyle rstyle =
  | ProjectionRtpplExprNoIdentOp {id: [{i: Info, v: String}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprNoIdentOp =
  | ProjectionRtpplExprNoIdentOp x ->
    x.__br_info
  sem getTerms_RtpplExprNoIdentOp =
  | ProjectionRtpplExprNoIdentOp x ->
    x.__br_terms
  sem unsplit_RtpplExprNoIdentOp =
  | AtomP {self = ProjectionRtpplExprNoIdentOp x} ->
    (x.__br_info, ProjectionRtpplExprNoIdent
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          in
          x1 })
end
lang ArrayAccessRtpplExprOp =
  RtpplExprOpBase
  + ArrayAccessRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | ArrayAccessRtpplExprOp {idx: [RtpplExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | ArrayAccessRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | ArrayAccessRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | PostfixP {self = ArrayAccessRtpplExprOp x, leftChildAlts = [ l ] ++ _} ->
    match
      unsplit_RtpplExprOp l
    with
      (linfo, l)
    in
    let info = mergeInfo linfo x.__br_info in
      (info, ArrayAccessRtpplExpr
        { info = info,
          e =
            match
              [ l ]
            with
              [ x1 ] ++ _
            in
            x1,
          idx =
            match
              x.idx
            with
              [ x2 ] ++ _
            in
            x2 })
end
lang ArrayLitRtpplExprOp =
  RtpplExprOpBase
  + ArrayLitRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | ArrayLitRtpplExprOp {elems: [RtpplExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | ArrayLitRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | ArrayLitRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | AtomP {self = ArrayLitRtpplExprOp x} ->
    (x.__br_info, ArrayLitRtpplExpr
      { info = x.__br_info, elems = x.elems })
end
lang RecordLitRtpplExprOp =
  RtpplExprOpBase
  + RecordLitRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | RecordLitRtpplExprOp {fields: [{e: RtpplExpr, id: {i: Info, v: String}}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | RecordLitRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | RecordLitRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | AtomP {self = RecordLitRtpplExprOp x} ->
    (x.__br_info, RecordLitRtpplExpr
      { info = x.__br_info, fields = x.fields })
end
lang LiteralRtpplExprOp =
  RtpplExprOpBase
  + LiteralRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | LiteralRtpplExprOp {const: [RtpplConst], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | LiteralRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | LiteralRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | AtomP {self = LiteralRtpplExprOp x} ->
    (x.__br_info, LiteralRtpplExpr
      { info = x.__br_info,
        const =
          match
            x.const
          with
            [ x1 ] ++ _
          in
          x1 })
end
lang LengthRtpplExprOp =
  RtpplExprOpBase
  + LengthRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | LengthRtpplExprOp {e: [RtpplExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | LengthRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | LengthRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | AtomP {self = LengthRtpplExprOp x} ->
    (x.__br_info, LengthRtpplExpr
      { info = x.__br_info,
        e =
          match
            x.e
          with
            [ x1 ] ++ _
          in
          x1 })
end
lang DistSamplesRtpplExprOp =
  RtpplExprOpBase
  + DistSamplesRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | DistSamplesRtpplExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | DistSamplesRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | DistSamplesRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | PrefixP {self = DistSamplesRtpplExprOp x, rightChildAlts = [ r ] ++ _} ->
    match
      unsplit_RtpplExprOp r
    with
      (rinfo, r)
    in
    let info = mergeInfo x.__br_info rinfo in
      (info, DistSamplesRtpplExpr
        { info = info,
          e =
            match
              [ r ]
            with
              [ x1 ] ++ _
            in
            x1 })
end
lang GaussianDistRtpplExprOp =
  RtpplExprOpBase
  + GaussianDistRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | GaussianDistRtpplExprOp {mu: [RtpplExpr], sigma: [RtpplExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | GaussianDistRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | GaussianDistRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | AtomP {self = GaussianDistRtpplExprOp x} ->
    (x.__br_info, GaussianDistRtpplExpr
      { info = x.__br_info,
        mu =
          match
            x.mu
          with
            [ x1 ] ++ _
          in
          x1,
        sigma =
          match
            x.sigma
          with
            [ x2 ] ++ _
          in
          x2 })
end
lang UniformDistRtpplExprOp =
  RtpplExprOpBase
  + UniformDistRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | UniformDistRtpplExprOp {hi: [RtpplExpr], lo: [RtpplExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | UniformDistRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | UniformDistRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | AtomP {self = UniformDistRtpplExprOp x} ->
    (x.__br_info, UniformDistRtpplExpr
      { info = x.__br_info,
        hi =
          match
            x.hi
          with
            [ x1 ] ++ _
          in
          x1,
        lo =
          match
            x.lo
          with
            [ x2 ] ++ _
          in
          x2 })
end
lang BernoulliDistRtpplExprOp =
  RtpplExprOpBase
  + BernoulliDistRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | BernoulliDistRtpplExprOp {p: [RtpplExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | BernoulliDistRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | BernoulliDistRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | AtomP {self = BernoulliDistRtpplExprOp x} ->
    (x.__br_info, BernoulliDistRtpplExpr
      { info = x.__br_info,
        p =
          match
            x.p
          with
            [ x1 ] ++ _
          in
          x1 })
end
lang GammaDistRtpplExprOp =
  RtpplExprOpBase
  + GammaDistRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | GammaDistRtpplExprOp {k: [RtpplExpr], theta: [RtpplExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | GammaDistRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | GammaDistRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | AtomP {self = GammaDistRtpplExprOp x} ->
    (x.__br_info, GammaDistRtpplExpr
      { info = x.__br_info,
        k =
          match
            x.k
          with
            [ x1 ] ++ _
          in
          x1,
        theta =
          match
            x.theta
          with
            [ x2 ] ++ _
          in
          x2 })
end
lang AddRtpplExprOp =
  RtpplExprOpBase
  + AddRtpplExprAst
  sem groupingsAllowed_RtpplExprOp =
  | (AddRtpplExprOp _, AddRtpplExprOp _) ->
    GLeft
      {}
  syn RtpplExprOp lstyle rstyle =
  | AddRtpplExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | AddRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | AddRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | InfixP {self = AddRtpplExprOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _} ->
    match
      (unsplit_RtpplExprOp l, unsplit_RtpplExprOp r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info = foldl mergeInfo linfo [ x.__br_info,
            rinfo ]
      in
      (info, AddRtpplExpr
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
end
lang SubRtpplExprOp =
  RtpplExprOpBase
  + SubRtpplExprAst
  sem groupingsAllowed_RtpplExprOp =
  | (SubRtpplExprOp _, SubRtpplExprOp _) ->
    GLeft
      {}
  syn RtpplExprOp lstyle rstyle =
  | SubRtpplExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | SubRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | SubRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | InfixP {self = SubRtpplExprOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _} ->
    match
      (unsplit_RtpplExprOp l, unsplit_RtpplExprOp r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info = foldl mergeInfo linfo [ x.__br_info,
            rinfo ]
      in
      (info, SubRtpplExpr
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
end
lang MulRtpplExprOp =
  RtpplExprOpBase
  + MulRtpplExprAst
  sem groupingsAllowed_RtpplExprOp =
  | (MulRtpplExprOp _, MulRtpplExprOp _) ->
    GLeft
      {}
  syn RtpplExprOp lstyle rstyle =
  | MulRtpplExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | MulRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | MulRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | InfixP {self = MulRtpplExprOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _} ->
    match
      (unsplit_RtpplExprOp l, unsplit_RtpplExprOp r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info = foldl mergeInfo linfo [ x.__br_info,
            rinfo ]
      in
      (info, MulRtpplExpr
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
end
lang DivRtpplExprOp =
  RtpplExprOpBase
  + DivRtpplExprAst
  sem groupingsAllowed_RtpplExprOp =
  | (DivRtpplExprOp _, DivRtpplExprOp _) ->
    GLeft
      {}
  syn RtpplExprOp lstyle rstyle =
  | DivRtpplExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | DivRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | DivRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | InfixP {self = DivRtpplExprOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _} ->
    match
      (unsplit_RtpplExprOp l, unsplit_RtpplExprOp r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info = foldl mergeInfo linfo [ x.__br_info,
            rinfo ]
      in
      (info, DivRtpplExpr
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
end
lang EqRtpplExprOp =
  RtpplExprOpBase
  + EqRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | EqRtpplExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | EqRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | EqRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | InfixP {self = EqRtpplExprOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _} ->
    match
      (unsplit_RtpplExprOp l, unsplit_RtpplExprOp r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info = foldl mergeInfo linfo [ x.__br_info,
            rinfo ]
      in
      (info, EqRtpplExpr
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
end
lang NeqRtpplExprOp =
  RtpplExprOpBase
  + NeqRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | NeqRtpplExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | NeqRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | NeqRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | InfixP {self = NeqRtpplExprOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _} ->
    match
      (unsplit_RtpplExprOp l, unsplit_RtpplExprOp r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info = foldl mergeInfo linfo [ x.__br_info,
            rinfo ]
      in
      (info, NeqRtpplExpr
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
end
lang LtRtpplExprOp =
  RtpplExprOpBase
  + LtRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | LtRtpplExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | LtRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | LtRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | InfixP {self = LtRtpplExprOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _} ->
    match
      (unsplit_RtpplExprOp l, unsplit_RtpplExprOp r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info = foldl mergeInfo linfo [ x.__br_info,
            rinfo ]
      in
      (info, LtRtpplExpr
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
end
lang GtRtpplExprOp =
  RtpplExprOpBase
  + GtRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | GtRtpplExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | GtRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | GtRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | InfixP {self = GtRtpplExprOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _} ->
    match
      (unsplit_RtpplExprOp l, unsplit_RtpplExprOp r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info = foldl mergeInfo linfo [ x.__br_info,
            rinfo ]
      in
      (info, GtRtpplExpr
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
end
lang LeqRtpplExprOp =
  RtpplExprOpBase
  + LeqRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | LeqRtpplExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | LeqRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | LeqRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | InfixP {self = LeqRtpplExprOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _} ->
    match
      (unsplit_RtpplExprOp l, unsplit_RtpplExprOp r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info = foldl mergeInfo linfo [ x.__br_info,
            rinfo ]
      in
      (info, LeqRtpplExpr
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
end
lang GeqRtpplExprOp =
  RtpplExprOpBase
  + GeqRtpplExprAst
  syn RtpplExprOp lstyle rstyle =
  | GeqRtpplExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | GeqRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | GeqRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | InfixP {self = GeqRtpplExprOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _} ->
    match
      (unsplit_RtpplExprOp l, unsplit_RtpplExprOp r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info = foldl mergeInfo linfo [ x.__br_info,
            rinfo ]
      in
      (info, GeqRtpplExpr
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
end
lang AndRtpplExprOp =
  RtpplExprOpBase
  + AndRtpplExprAst
  sem groupingsAllowed_RtpplExprOp =
  | (AndRtpplExprOp _, AndRtpplExprOp _) ->
    GLeft
      {}
  syn RtpplExprOp lstyle rstyle =
  | AndRtpplExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | AndRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | AndRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | InfixP {self = AndRtpplExprOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _} ->
    match
      (unsplit_RtpplExprOp l, unsplit_RtpplExprOp r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info = foldl mergeInfo linfo [ x.__br_info,
            rinfo ]
      in
      (info, AndRtpplExpr
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
end
lang OrRtpplExprOp =
  RtpplExprOpBase
  + OrRtpplExprAst
  sem groupingsAllowed_RtpplExprOp =
  | (OrRtpplExprOp _, OrRtpplExprOp _) ->
    GLeft
      {}
  syn RtpplExprOp lstyle rstyle =
  | OrRtpplExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | OrRtpplExprOp x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | OrRtpplExprOp x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | InfixP {self = OrRtpplExprOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _} ->
    match
      (unsplit_RtpplExprOp l, unsplit_RtpplExprOp r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info = foldl mergeInfo linfo [ x.__br_info,
            rinfo ]
      in
      (info, OrRtpplExpr
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
end
lang MainRtpplMainOp =
  RtpplMainOpBase
  + MainRtpplMainAst
  syn RtpplMainOp lstyle rstyle =
  | MainRtpplMainOp {ext: [RtpplExt], tasks: [RtpplTask], __br_info: Info, __br_terms: [Info], connections: [RtpplConnection]}
  sem getInfo_RtpplMainOp =
  | MainRtpplMainOp x ->
    x.__br_info
  sem getTerms_RtpplMainOp =
  | MainRtpplMainOp x ->
    x.__br_terms
  sem unsplit_RtpplMainOp =
  | AtomP {self = MainRtpplMainOp x} ->
    (x.__br_info, MainRtpplMain
      { info = x.__br_info,
        ext = x.ext,
        tasks = x.tasks,
        connections = x.connections })
end
lang SensorRtpplExtOp =
  RtpplExtOpBase
  + SensorRtpplExtAst
  syn RtpplExtOp lstyle rstyle =
  | SensorRtpplExtOp {r: [RtpplExpr], id: [{i: Info, v: Name}], ty: [RtpplType], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExtOp =
  | SensorRtpplExtOp x ->
    x.__br_info
  sem getTerms_RtpplExtOp =
  | SensorRtpplExtOp x ->
    x.__br_terms
  sem unsplit_RtpplExtOp =
  | AtomP {self = SensorRtpplExtOp x} ->
    (x.__br_info, SensorRtpplExt
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          in
          x1,
        ty =
          match
            x.ty
          with
            [ x2 ] ++ _
          in
          x2,
        r =
          match
            x.r
          with
            [ x3 ] ++ _
          in
          x3 })
end
lang ActuatorRtpplExtOp =
  RtpplExtOpBase
  + ActuatorRtpplExtAst
  syn RtpplExtOp lstyle rstyle =
  | ActuatorRtpplExtOp {r: [RtpplExpr], id: [{i: Info, v: Name}], ty: [RtpplType], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExtOp =
  | ActuatorRtpplExtOp x ->
    x.__br_info
  sem getTerms_RtpplExtOp =
  | ActuatorRtpplExtOp x ->
    x.__br_terms
  sem unsplit_RtpplExtOp =
  | AtomP {self = ActuatorRtpplExtOp x} ->
    (x.__br_info, ActuatorRtpplExt
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          in
          x1,
        ty =
          match
            x.ty
          with
            [ x2 ] ++ _
          in
          x2,
        r =
          match
            x.r
          with
            [ x3 ] ++ _
          in
          x3 })
end
lang TaskRtpplTaskOp =
  RtpplTaskOpBase
  + TaskRtpplTaskAst
  syn RtpplTaskOp lstyle rstyle =
  | TaskRtpplTaskOp {p: [{i: Info, v: Int}], id: [{i: Info, v: Name}], args: [RtpplExpr], __br_info: Info, __br_terms: [Info], templateId: [{i: Info, v: Name}]}
  sem getInfo_RtpplTaskOp =
  | TaskRtpplTaskOp x ->
    x.__br_info
  sem getTerms_RtpplTaskOp =
  | TaskRtpplTaskOp x ->
    x.__br_terms
  sem unsplit_RtpplTaskOp =
  | AtomP {self = TaskRtpplTaskOp x} ->
    (x.__br_info, TaskRtpplTask
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          in
          x1,
        p =
          match
            x.p
          with
            [ x2 ] ++ _
          in
          x2,
        args = x.args,
        templateId =
          match
            x.templateId
          with
            [ x3 ] ++ _
          in
          x3 })
end
lang ConnectionRtpplConnectionOp =
  RtpplConnectionOpBase
  + ConnectionRtpplConnectionAst
  syn RtpplConnectionOp lstyle rstyle =
  | ConnectionRtpplConnectionOp {to: [RtpplPortSpec], from: [RtpplPortSpec], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplConnectionOp =
  | ConnectionRtpplConnectionOp x ->
    x.__br_info
  sem getTerms_RtpplConnectionOp =
  | ConnectionRtpplConnectionOp x ->
    x.__br_terms
  sem unsplit_RtpplConnectionOp =
  | AtomP {self = ConnectionRtpplConnectionOp x} ->
    (x.__br_info, ConnectionRtpplConnection
      { info = x.__br_info,
        to =
          match
            x.to
          with
            [ x1 ] ++ _
          in
          x1,
        from =
          match
            x.from
          with
            [ x2 ] ++ _
          in
          x2 })
end
lang PortSpecRtpplPortSpecOp =
  RtpplPortSpecOpBase
  + PortSpecRtpplPortSpecAst
  syn RtpplPortSpecOp lstyle rstyle =
  | PortSpecRtpplPortSpecOp {id: [{i: Info, v: String}], port: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplPortSpecOp =
  | PortSpecRtpplPortSpecOp x ->
    x.__br_info
  sem getTerms_RtpplPortSpecOp =
  | PortSpecRtpplPortSpecOp x ->
    x.__br_terms
  sem unsplit_RtpplPortSpecOp =
  | AtomP {self = PortSpecRtpplPortSpecOp x} ->
    (x.__br_info, PortSpecRtpplPortSpec
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          then
            Some
              x1
          else
            None
              {},
        port =
          match
            x.port
          with
            [ x2 ] ++ _
          in
          x2 })
end
lang IntRtpplTypeOp =
  RtpplTypeOpBase
  + IntRtpplTypeAst
  syn RtpplTypeOp lstyle rstyle =
  | IntRtpplTypeOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTypeOp =
  | IntRtpplTypeOp x ->
    x.__br_info
  sem getTerms_RtpplTypeOp =
  | IntRtpplTypeOp x ->
    x.__br_terms
  sem unsplit_RtpplTypeOp =
  | AtomP {self = IntRtpplTypeOp x} ->
    (x.__br_info, IntRtpplType
      { info = x.__br_info })
end
lang FloatRtpplTypeOp =
  RtpplTypeOpBase
  + FloatRtpplTypeAst
  syn RtpplTypeOp lstyle rstyle =
  | FloatRtpplTypeOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTypeOp =
  | FloatRtpplTypeOp x ->
    x.__br_info
  sem getTerms_RtpplTypeOp =
  | FloatRtpplTypeOp x ->
    x.__br_terms
  sem unsplit_RtpplTypeOp =
  | AtomP {self = FloatRtpplTypeOp x} ->
    (x.__br_info, FloatRtpplType
      { info = x.__br_info })
end
lang BoolRtpplTypeOp =
  RtpplTypeOpBase
  + BoolRtpplTypeAst
  syn RtpplTypeOp lstyle rstyle =
  | BoolRtpplTypeOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTypeOp =
  | BoolRtpplTypeOp x ->
    x.__br_info
  sem getTerms_RtpplTypeOp =
  | BoolRtpplTypeOp x ->
    x.__br_terms
  sem unsplit_RtpplTypeOp =
  | AtomP {self = BoolRtpplTypeOp x} ->
    (x.__br_info, BoolRtpplType
      { info = x.__br_info })
end
lang StringRtpplTypeOp =
  RtpplTypeOpBase
  + StringRtpplTypeAst
  syn RtpplTypeOp lstyle rstyle =
  | StringRtpplTypeOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTypeOp =
  | StringRtpplTypeOp x ->
    x.__br_info
  sem getTerms_RtpplTypeOp =
  | StringRtpplTypeOp x ->
    x.__br_terms
  sem unsplit_RtpplTypeOp =
  | AtomP {self = StringRtpplTypeOp x} ->
    (x.__br_info, StringRtpplType
      { info = x.__br_info })
end
lang UnitRtpplTypeOp =
  RtpplTypeOpBase
  + UnitRtpplTypeAst
  syn RtpplTypeOp lstyle rstyle =
  | UnitRtpplTypeOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTypeOp =
  | UnitRtpplTypeOp x ->
    x.__br_info
  sem getTerms_RtpplTypeOp =
  | UnitRtpplTypeOp x ->
    x.__br_terms
  sem unsplit_RtpplTypeOp =
  | AtomP {self = UnitRtpplTypeOp x} ->
    (x.__br_info, UnitRtpplType
      { info = x.__br_info })
end
lang SeqRtpplTypeOp =
  RtpplTypeOpBase
  + SeqRtpplTypeAst
  syn RtpplTypeOp lstyle rstyle =
  | SeqRtpplTypeOp {ty: [RtpplType], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTypeOp =
  | SeqRtpplTypeOp x ->
    x.__br_info
  sem getTerms_RtpplTypeOp =
  | SeqRtpplTypeOp x ->
    x.__br_terms
  sem unsplit_RtpplTypeOp =
  | AtomP {self = SeqRtpplTypeOp x} ->
    (x.__br_info, SeqRtpplType
      { info = x.__br_info,
        ty =
          match
            x.ty
          with
            [ x1 ] ++ _
          in
          x1 })
end
lang RecordRtpplTypeOp =
  RtpplTypeOpBase
  + RecordRtpplTypeAst
  syn RtpplTypeOp lstyle rstyle =
  | RecordRtpplTypeOp {fields: [{id: {i: Info, v: String}, ty: RtpplType}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTypeOp =
  | RecordRtpplTypeOp x ->
    x.__br_info
  sem getTerms_RtpplTypeOp =
  | RecordRtpplTypeOp x ->
    x.__br_terms
  sem unsplit_RtpplTypeOp =
  | AtomP {self = RecordRtpplTypeOp x} ->
    (x.__br_info, RecordRtpplType
      { info = x.__br_info, fields = x.fields })
end
lang FunctionRtpplTypeOp =
  RtpplTypeOpBase
  + FunctionRtpplTypeAst
  sem groupingsAllowed_RtpplTypeOp =
  | (FunctionRtpplTypeOp _, FunctionRtpplTypeOp _) ->
    GRight
      {}
  syn RtpplTypeOp lstyle rstyle =
  | FunctionRtpplTypeOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTypeOp =
  | FunctionRtpplTypeOp x ->
    x.__br_info
  sem getTerms_RtpplTypeOp =
  | FunctionRtpplTypeOp x ->
    x.__br_terms
  sem unsplit_RtpplTypeOp =
  | InfixP {self = FunctionRtpplTypeOp x, leftChildAlts = [ l ] ++ _, rightChildAlts = [ r ] ++ _} ->
    match
      (unsplit_RtpplTypeOp l, unsplit_RtpplTypeOp r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info = foldl mergeInfo linfo [ x.__br_info,
            rinfo ]
      in
      (info, FunctionRtpplType
        { info = info,
          to =
            match
              [ r ]
            with
              [ x1 ] ++ _
            in
            x1,
          from =
            match
              [ l ]
            with
              [ x2 ] ++ _
            in
            x2 })
end
lang DistRtpplTypeOp =
  RtpplTypeOpBase
  + DistRtpplTypeAst
  syn RtpplTypeOp lstyle rstyle =
  | DistRtpplTypeOp {ty: [RtpplType], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTypeOp =
  | DistRtpplTypeOp x ->
    x.__br_info
  sem getTerms_RtpplTypeOp =
  | DistRtpplTypeOp x ->
    x.__br_terms
  sem unsplit_RtpplTypeOp =
  | AtomP {self = DistRtpplTypeOp x} ->
    (x.__br_info, DistRtpplType
      { info = x.__br_info,
        ty =
          match
            x.ty
          with
            [ x1 ] ++ _
          in
          x1 })
end
lang AliasRtpplTypeOp =
  RtpplTypeOpBase
  + AliasRtpplTypeAst
  syn RtpplTypeOp lstyle rstyle =
  | AliasRtpplTypeOp {id: [{i: Info, v: Name}], next: [RtpplTypeNoIdent], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTypeOp =
  | AliasRtpplTypeOp x ->
    x.__br_info
  sem getTerms_RtpplTypeOp =
  | AliasRtpplTypeOp x ->
    x.__br_terms
  sem unsplit_RtpplTypeOp =
  | AtomP {self = AliasRtpplTypeOp x} ->
    (x.__br_info, AliasRtpplType
      { info = x.__br_info,
        id =
          match
            x.id
          with
            [ x1 ] ++ _
          in
          x1,
        next =
          match
            x.next
          with
            [ x2 ] ++ _
          in
          x2 })
end
lang DirectRtpplTypeNoIdentOp =
  RtpplTypeNoIdentOpBase
  + DirectRtpplTypeNoIdentAst
  syn RtpplTypeNoIdentOp lstyle rstyle =
  | DirectRtpplTypeNoIdentOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTypeNoIdentOp =
  | DirectRtpplTypeNoIdentOp x ->
    x.__br_info
  sem getTerms_RtpplTypeNoIdentOp =
  | DirectRtpplTypeNoIdentOp x ->
    x.__br_terms
  sem unsplit_RtpplTypeNoIdentOp =
  | AtomP {self = DirectRtpplTypeNoIdentOp x} ->
    (x.__br_info, DirectRtpplTypeNoIdent
      { info = x.__br_info })
end
lang ApplicationRtpplTypeNoIdentOp =
  RtpplTypeNoIdentOpBase
  + ApplicationRtpplTypeNoIdentAst
  syn RtpplTypeNoIdentOp lstyle rstyle =
  | ApplicationRtpplTypeNoIdentOp {args: [RtpplType], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTypeNoIdentOp =
  | ApplicationRtpplTypeNoIdentOp x ->
    x.__br_info
  sem getTerms_RtpplTypeNoIdentOp =
  | ApplicationRtpplTypeNoIdentOp x ->
    x.__br_terms
  sem unsplit_RtpplTypeNoIdentOp =
  | AtomP {self = ApplicationRtpplTypeNoIdentOp x} ->
    (x.__br_info, ApplicationRtpplTypeNoIdent
      { info = x.__br_info, args = x.args })
end
lang LitIntRtpplConstOp =
  RtpplConstOpBase
  + LitIntRtpplConstAst
  syn RtpplConstOp lstyle rstyle =
  | LitIntRtpplConstOp {value: [{i: Info, v: Int}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplConstOp =
  | LitIntRtpplConstOp x ->
    x.__br_info
  sem getTerms_RtpplConstOp =
  | LitIntRtpplConstOp x ->
    x.__br_terms
  sem unsplit_RtpplConstOp =
  | AtomP {self = LitIntRtpplConstOp x} ->
    (x.__br_info, LitIntRtpplConst
      { info = x.__br_info,
        value =
          match
            x.value
          with
            [ x1 ] ++ _
          in
          x1 })
end
lang LitFloatRtpplConstOp =
  RtpplConstOpBase
  + LitFloatRtpplConstAst
  syn RtpplConstOp lstyle rstyle =
  | LitFloatRtpplConstOp {value: [{i: Info, v: Float}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplConstOp =
  | LitFloatRtpplConstOp x ->
    x.__br_info
  sem getTerms_RtpplConstOp =
  | LitFloatRtpplConstOp x ->
    x.__br_terms
  sem unsplit_RtpplConstOp =
  | AtomP {self = LitFloatRtpplConstOp x} ->
    (x.__br_info, LitFloatRtpplConst
      { info = x.__br_info,
        value =
          match
            x.value
          with
            [ x1 ] ++ _
          in
          x1 })
end
lang LitBoolRtpplConstOp =
  RtpplConstOpBase
  + LitBoolRtpplConstAst
  syn RtpplConstOp lstyle rstyle =
  | LitBoolRtpplConstOp {value: [{i: Info, v: Bool}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplConstOp =
  | LitBoolRtpplConstOp x ->
    x.__br_info
  sem getTerms_RtpplConstOp =
  | LitBoolRtpplConstOp x ->
    x.__br_terms
  sem unsplit_RtpplConstOp =
  | AtomP {self = LitBoolRtpplConstOp x} ->
    (x.__br_info, LitBoolRtpplConst
      { info = x.__br_info,
        value =
          match
            x.value
          with
            [ x1 ] ++ _
          in
          x1 })
end
lang LitStringRtpplConstOp =
  RtpplConstOpBase
  + LitStringRtpplConstAst
  syn RtpplConstOp lstyle rstyle =
  | LitStringRtpplConstOp {value: [{i: Info, v: String}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplConstOp =
  | LitStringRtpplConstOp x ->
    x.__br_info
  sem getTerms_RtpplConstOp =
  | LitStringRtpplConstOp x ->
    x.__br_terms
  sem unsplit_RtpplConstOp =
  | AtomP {self = LitStringRtpplConstOp x} ->
    (x.__br_info, LitStringRtpplConst
      { info = x.__br_info,
        value =
          match
            x.value
          with
            [ x1 ] ++ _
          in
          x1 })
end
lang RtpplExprGrouping =
  RtpplExprOpBase
  syn RtpplExprOp lstyle rstyle =
  | RtpplExprGrouping {inner: RtpplExpr, __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplExprOp =
  | RtpplExprGrouping x ->
    x.__br_info
  sem getTerms_RtpplExprOp =
  | RtpplExprGrouping x ->
    x.__br_terms
  sem unsplit_RtpplExprOp =
  | AtomP {self = RtpplExprGrouping x} ->
    (x.__br_info, x.inner)
end
lang RtpplTypeGrouping =
  RtpplTypeOpBase
  syn RtpplTypeOp lstyle rstyle =
  | RtpplTypeGrouping {inner: RtpplType, __br_info: Info, __br_terms: [Info]}
  sem getInfo_RtpplTypeOp =
  | RtpplTypeGrouping x ->
    x.__br_info
  sem getTerms_RtpplTypeOp =
  | RtpplTypeGrouping x ->
    x.__br_terms
  sem unsplit_RtpplTypeOp =
  | AtomP {self = RtpplTypeGrouping x} ->
    (x.__br_info, x.inner)
end
lang ParseRtppl =
  ProgramRtpplProgramOp
  + ConstantRtpplTopOp
  + TypeAliasRtpplTopOp
  + FunctionDefRtpplTopOp
  + ModelDefRtpplTopOp
  + TemplateDefRtpplTopOp
  + ParamsRtpplTopParamsOp
  + InputRtpplPortOp
  + OutputRtpplPortOp
  + BindingRtpplStmtOp
  + ObserveRtpplStmtOp
  + AssumeRtpplStmtOp
  + InferRtpplStmtOp
  + DegenerateRtpplStmtOp
  + ResampleRtpplStmtOp
  + ReadRtpplStmtOp
  + WriteRtpplStmtOp
  + ConditionRtpplStmtOp
  + DelayRtpplStmtOp
  + ForLoopRtpplStmtOp
  + WhileLoopRtpplStmtOp
  + IdentPlusStmtRtpplStmtOp
  + ReassignRtpplStmtNoIdentOp
  + FunctionCallSRtpplStmtNoIdentOp
  + IdentPlusExprRtpplExprOp
  + VariableRtpplExprNoIdentOp
  + FunctionCallERtpplExprNoIdentOp
  + ProjectionRtpplExprNoIdentOp
  + ArrayAccessRtpplExprOp
  + ArrayLitRtpplExprOp
  + RecordLitRtpplExprOp
  + LiteralRtpplExprOp
  + LengthRtpplExprOp
  + DistSamplesRtpplExprOp
  + GaussianDistRtpplExprOp
  + UniformDistRtpplExprOp
  + BernoulliDistRtpplExprOp
  + GammaDistRtpplExprOp
  + AddRtpplExprOp
  + SubRtpplExprOp
  + MulRtpplExprOp
  + DivRtpplExprOp
  + EqRtpplExprOp
  + NeqRtpplExprOp
  + LtRtpplExprOp
  + GtRtpplExprOp
  + LeqRtpplExprOp
  + GeqRtpplExprOp
  + AndRtpplExprOp
  + OrRtpplExprOp
  + MainRtpplMainOp
  + SensorRtpplExtOp
  + ActuatorRtpplExtOp
  + TaskRtpplTaskOp
  + ConnectionRtpplConnectionOp
  + PortSpecRtpplPortSpecOp
  + IntRtpplTypeOp
  + FloatRtpplTypeOp
  + BoolRtpplTypeOp
  + StringRtpplTypeOp
  + UnitRtpplTypeOp
  + SeqRtpplTypeOp
  + RecordRtpplTypeOp
  + FunctionRtpplTypeOp
  + DistRtpplTypeOp
  + AliasRtpplTypeOp
  + DirectRtpplTypeNoIdentOp
  + ApplicationRtpplTypeNoIdentOp
  + LitIntRtpplConstOp
  + LitFloatRtpplConstOp
  + LitBoolRtpplConstOp
  + LitStringRtpplConstOp
  + RtpplExprGrouping
  + RtpplTypeGrouping
  + BadRtpplProgramAst
  + BadRtpplTopAst
  + BadRtpplTopParamsAst
  + BadRtpplStmtAst
  + BadRtpplStmtNoIdentAst
  + BadRtpplExprAst
  + BadRtpplExprNoIdentAst
  + BadRtpplTypeAst
  + BadRtpplTypeNoIdentAst
  + BadRtpplConstAst
  + BadRtpplPortAst
  + BadRtpplMainAst
  + BadRtpplExtAst
  + BadRtpplTaskAst
  + BadRtpplConnectionAst
  + BadRtpplPortSpecAst
  + LL1Parser
  + CommaTokenParser
  + WhitespaceParser
  + LIdentTokenParser
  + StringTokenParser
  + UFloatTokenParser
  + UIdentTokenParser
  + BooleanTokenParser
  + BracketTokenParser
  + OperatorTokenParser
  + RTPPLLineCommentParser
  + TimedIntegerTokenParser
  sem groupingsAllowed_RtpplProgramOp =
  sem groupingsAllowed_RtpplTopOp =
  sem groupingsAllowed_RtpplTopParamsOp =
  sem groupingsAllowed_RtpplStmtOp =
  sem groupingsAllowed_RtpplStmtNoIdentOp =
  sem groupingsAllowed_RtpplExprOp =
  | (AddRtpplExprOp _, ArrayAccessRtpplExprOp _) ->
    GRight
      {}
  | (SubRtpplExprOp _, ArrayAccessRtpplExprOp _) ->
    GRight
      {}
  | (MulRtpplExprOp _, ArrayAccessRtpplExprOp _) ->
    GRight
      {}
  | (DivRtpplExprOp _, ArrayAccessRtpplExprOp _) ->
    GRight
      {}
  | (EqRtpplExprOp _, ArrayAccessRtpplExprOp _) ->
    GRight
      {}
  | (NeqRtpplExprOp _, ArrayAccessRtpplExprOp _) ->
    GRight
      {}
  | (LtRtpplExprOp _, ArrayAccessRtpplExprOp _) ->
    GRight
      {}
  | (GtRtpplExprOp _, ArrayAccessRtpplExprOp _) ->
    GRight
      {}
  | (LeqRtpplExprOp _, ArrayAccessRtpplExprOp _) ->
    GRight
      {}
  | (GeqRtpplExprOp _, ArrayAccessRtpplExprOp _) ->
    GRight
      {}
  | (AndRtpplExprOp _, ArrayAccessRtpplExprOp _) ->
    GRight
      {}
  | (OrRtpplExprOp _, ArrayAccessRtpplExprOp _) ->
    GRight
      {}
  | (AddRtpplExprOp _, SubRtpplExprOp _) ->
    GLeft
      {}
  | (SubRtpplExprOp _, AddRtpplExprOp _) ->
    GLeft
      {}
  | (AddRtpplExprOp _, MulRtpplExprOp _) ->
    GRight
      {}
  | (MulRtpplExprOp _, AddRtpplExprOp _) ->
    GLeft
      {}
  | (AddRtpplExprOp _, DivRtpplExprOp _) ->
    GRight
      {}
  | (DivRtpplExprOp _, AddRtpplExprOp _) ->
    GLeft
      {}
  | (AddRtpplExprOp _, EqRtpplExprOp _) ->
    GLeft
      {}
  | (EqRtpplExprOp _, AddRtpplExprOp _) ->
    GRight
      {}
  | (AddRtpplExprOp _, NeqRtpplExprOp _) ->
    GLeft
      {}
  | (NeqRtpplExprOp _, AddRtpplExprOp _) ->
    GRight
      {}
  | (AddRtpplExprOp _, LtRtpplExprOp _) ->
    GLeft
      {}
  | (LtRtpplExprOp _, AddRtpplExprOp _) ->
    GRight
      {}
  | (AddRtpplExprOp _, GtRtpplExprOp _) ->
    GLeft
      {}
  | (GtRtpplExprOp _, AddRtpplExprOp _) ->
    GRight
      {}
  | (AddRtpplExprOp _, LeqRtpplExprOp _) ->
    GLeft
      {}
  | (LeqRtpplExprOp _, AddRtpplExprOp _) ->
    GRight
      {}
  | (AddRtpplExprOp _, GeqRtpplExprOp _) ->
    GLeft
      {}
  | (GeqRtpplExprOp _, AddRtpplExprOp _) ->
    GRight
      {}
  | (AddRtpplExprOp _, AndRtpplExprOp _) ->
    GLeft
      {}
  | (AndRtpplExprOp _, AddRtpplExprOp _) ->
    GRight
      {}
  | (AddRtpplExprOp _, OrRtpplExprOp _) ->
    GLeft
      {}
  | (OrRtpplExprOp _, AddRtpplExprOp _) ->
    GRight
      {}
  | (SubRtpplExprOp _, MulRtpplExprOp _) ->
    GRight
      {}
  | (MulRtpplExprOp _, SubRtpplExprOp _) ->
    GLeft
      {}
  | (SubRtpplExprOp _, DivRtpplExprOp _) ->
    GRight
      {}
  | (DivRtpplExprOp _, SubRtpplExprOp _) ->
    GLeft
      {}
  | (SubRtpplExprOp _, EqRtpplExprOp _) ->
    GLeft
      {}
  | (EqRtpplExprOp _, SubRtpplExprOp _) ->
    GRight
      {}
  | (SubRtpplExprOp _, NeqRtpplExprOp _) ->
    GLeft
      {}
  | (NeqRtpplExprOp _, SubRtpplExprOp _) ->
    GRight
      {}
  | (SubRtpplExprOp _, LtRtpplExprOp _) ->
    GLeft
      {}
  | (LtRtpplExprOp _, SubRtpplExprOp _) ->
    GRight
      {}
  | (SubRtpplExprOp _, GtRtpplExprOp _) ->
    GLeft
      {}
  | (GtRtpplExprOp _, SubRtpplExprOp _) ->
    GRight
      {}
  | (SubRtpplExprOp _, LeqRtpplExprOp _) ->
    GLeft
      {}
  | (LeqRtpplExprOp _, SubRtpplExprOp _) ->
    GRight
      {}
  | (SubRtpplExprOp _, GeqRtpplExprOp _) ->
    GLeft
      {}
  | (GeqRtpplExprOp _, SubRtpplExprOp _) ->
    GRight
      {}
  | (SubRtpplExprOp _, AndRtpplExprOp _) ->
    GLeft
      {}
  | (AndRtpplExprOp _, SubRtpplExprOp _) ->
    GRight
      {}
  | (SubRtpplExprOp _, OrRtpplExprOp _) ->
    GLeft
      {}
  | (OrRtpplExprOp _, SubRtpplExprOp _) ->
    GRight
      {}
  | (MulRtpplExprOp _, DivRtpplExprOp _) ->
    GLeft
      {}
  | (DivRtpplExprOp _, MulRtpplExprOp _) ->
    GLeft
      {}
  | (MulRtpplExprOp _, EqRtpplExprOp _) ->
    GLeft
      {}
  | (EqRtpplExprOp _, MulRtpplExprOp _) ->
    GRight
      {}
  | (MulRtpplExprOp _, NeqRtpplExprOp _) ->
    GLeft
      {}
  | (NeqRtpplExprOp _, MulRtpplExprOp _) ->
    GRight
      {}
  | (MulRtpplExprOp _, LtRtpplExprOp _) ->
    GLeft
      {}
  | (LtRtpplExprOp _, MulRtpplExprOp _) ->
    GRight
      {}
  | (MulRtpplExprOp _, GtRtpplExprOp _) ->
    GLeft
      {}
  | (GtRtpplExprOp _, MulRtpplExprOp _) ->
    GRight
      {}
  | (MulRtpplExprOp _, LeqRtpplExprOp _) ->
    GLeft
      {}
  | (LeqRtpplExprOp _, MulRtpplExprOp _) ->
    GRight
      {}
  | (MulRtpplExprOp _, GeqRtpplExprOp _) ->
    GLeft
      {}
  | (GeqRtpplExprOp _, MulRtpplExprOp _) ->
    GRight
      {}
  | (MulRtpplExprOp _, AndRtpplExprOp _) ->
    GLeft
      {}
  | (AndRtpplExprOp _, MulRtpplExprOp _) ->
    GRight
      {}
  | (MulRtpplExprOp _, OrRtpplExprOp _) ->
    GLeft
      {}
  | (OrRtpplExprOp _, MulRtpplExprOp _) ->
    GRight
      {}
  | (DivRtpplExprOp _, EqRtpplExprOp _) ->
    GLeft
      {}
  | (EqRtpplExprOp _, DivRtpplExprOp _) ->
    GRight
      {}
  | (DivRtpplExprOp _, NeqRtpplExprOp _) ->
    GLeft
      {}
  | (NeqRtpplExprOp _, DivRtpplExprOp _) ->
    GRight
      {}
  | (DivRtpplExprOp _, LtRtpplExprOp _) ->
    GLeft
      {}
  | (LtRtpplExprOp _, DivRtpplExprOp _) ->
    GRight
      {}
  | (DivRtpplExprOp _, GtRtpplExprOp _) ->
    GLeft
      {}
  | (GtRtpplExprOp _, DivRtpplExprOp _) ->
    GRight
      {}
  | (DivRtpplExprOp _, LeqRtpplExprOp _) ->
    GLeft
      {}
  | (LeqRtpplExprOp _, DivRtpplExprOp _) ->
    GRight
      {}
  | (DivRtpplExprOp _, GeqRtpplExprOp _) ->
    GLeft
      {}
  | (GeqRtpplExprOp _, DivRtpplExprOp _) ->
    GRight
      {}
  | (DivRtpplExprOp _, AndRtpplExprOp _) ->
    GLeft
      {}
  | (AndRtpplExprOp _, DivRtpplExprOp _) ->
    GRight
      {}
  | (DivRtpplExprOp _, OrRtpplExprOp _) ->
    GLeft
      {}
  | (OrRtpplExprOp _, DivRtpplExprOp _) ->
    GRight
      {}
  | (EqRtpplExprOp _, AndRtpplExprOp _) ->
    GLeft
      {}
  | (AndRtpplExprOp _, EqRtpplExprOp _) ->
    GRight
      {}
  | (EqRtpplExprOp _, OrRtpplExprOp _) ->
    GLeft
      {}
  | (OrRtpplExprOp _, EqRtpplExprOp _) ->
    GRight
      {}
  | (NeqRtpplExprOp _, AndRtpplExprOp _) ->
    GLeft
      {}
  | (AndRtpplExprOp _, NeqRtpplExprOp _) ->
    GRight
      {}
  | (NeqRtpplExprOp _, OrRtpplExprOp _) ->
    GLeft
      {}
  | (OrRtpplExprOp _, NeqRtpplExprOp _) ->
    GRight
      {}
  | (LtRtpplExprOp _, AndRtpplExprOp _) ->
    GLeft
      {}
  | (AndRtpplExprOp _, LtRtpplExprOp _) ->
    GRight
      {}
  | (LtRtpplExprOp _, OrRtpplExprOp _) ->
    GLeft
      {}
  | (OrRtpplExprOp _, LtRtpplExprOp _) ->
    GRight
      {}
  | (GtRtpplExprOp _, AndRtpplExprOp _) ->
    GLeft
      {}
  | (AndRtpplExprOp _, GtRtpplExprOp _) ->
    GRight
      {}
  | (GtRtpplExprOp _, OrRtpplExprOp _) ->
    GLeft
      {}
  | (OrRtpplExprOp _, GtRtpplExprOp _) ->
    GRight
      {}
  | (LeqRtpplExprOp _, AndRtpplExprOp _) ->
    GLeft
      {}
  | (AndRtpplExprOp _, LeqRtpplExprOp _) ->
    GRight
      {}
  | (LeqRtpplExprOp _, OrRtpplExprOp _) ->
    GLeft
      {}
  | (OrRtpplExprOp _, LeqRtpplExprOp _) ->
    GRight
      {}
  | (GeqRtpplExprOp _, AndRtpplExprOp _) ->
    GLeft
      {}
  | (AndRtpplExprOp _, GeqRtpplExprOp _) ->
    GRight
      {}
  | (GeqRtpplExprOp _, OrRtpplExprOp _) ->
    GLeft
      {}
  | (OrRtpplExprOp _, GeqRtpplExprOp _) ->
    GRight
      {}
  | (AndRtpplExprOp _, OrRtpplExprOp _) ->
    GLeft
      {}
  | (OrRtpplExprOp _, AndRtpplExprOp _) ->
    GLeft
      {}
  sem groupingsAllowed_RtpplExprNoIdentOp =
  sem groupingsAllowed_RtpplTypeOp =
  sem groupingsAllowed_RtpplTypeNoIdentOp =
  sem groupingsAllowed_RtpplConstOp =
  sem groupingsAllowed_RtpplPortOp =
  sem groupingsAllowed_RtpplMainOp =
  sem groupingsAllowed_RtpplExtOp =
  sem groupingsAllowed_RtpplTaskOp =
  sem groupingsAllowed_RtpplConnectionOp =
  sem groupingsAllowed_RtpplPortSpecOp =
end
let _table =
  use ParseRtppl
  in
  let target =
    genParsingTable
      (let #var"RtpplProgram" = nameSym "RtpplProgram" in
       let #var"RtpplTop" = nameSym "RtpplTop" in
       let #var"RtpplTopParams" = nameSym "RtpplTopParams" in
       let #var"RtpplStmt" = nameSym "RtpplStmt" in
       let #var"RtpplStmtNoIdent" = nameSym "RtpplStmtNoIdent" in
       let #var"RtpplExpr" = nameSym "RtpplExpr" in
       let #var"RtpplExprNoIdent" = nameSym "RtpplExprNoIdent" in
       let #var"RtpplType" = nameSym "RtpplType" in
       let #var"RtpplTypeNoIdent" = nameSym "RtpplTypeNoIdent" in
       let #var"RtpplConst" = nameSym "RtpplConst" in
       let #var"RtpplPort" = nameSym "RtpplPort" in
       let #var"RtpplMain" = nameSym "RtpplMain" in
       let #var"RtpplExt" = nameSym "RtpplExt" in
       let #var"RtpplTask" = nameSym "RtpplTask" in
       let #var"RtpplConnection" = nameSym "RtpplConnection" in
       let #var"RtpplPortSpec" = nameSym "RtpplPortSpec" in
       let #var"RtpplProgramPostfix" = nameSym "RtpplProgramPostfix"
       in
       let #var"RtpplProgramPrefix" = nameSym "RtpplProgramPrefix" in
       let #var"RtpplProgramInfix" = nameSym "RtpplProgramInfix" in
       let #var"RtpplProgramAtom" = nameSym "RtpplProgramAtom" in
       let #var"RtpplTopPostfix" = nameSym "RtpplTopPostfix" in
       let #var"RtpplTopPrefix" = nameSym "RtpplTopPrefix" in
       let #var"RtpplTopInfix" = nameSym "RtpplTopInfix" in
       let #var"RtpplTopAtom" = nameSym "RtpplTopAtom" in
       let #var"RtpplTopParamsPostfix" = nameSym "RtpplTopParamsPostfix"
       in
       let #var"RtpplTopParamsPrefix" = nameSym "RtpplTopParamsPrefix"
       in
       let #var"RtpplTopParamsInfix" = nameSym "RtpplTopParamsInfix"
       in
       let #var"RtpplTopParamsAtom" = nameSym "RtpplTopParamsAtom" in
       let #var"RtpplStmtPostfix" = nameSym "RtpplStmtPostfix" in
       let #var"RtpplStmtPrefix" = nameSym "RtpplStmtPrefix" in
       let #var"RtpplStmtInfix" = nameSym "RtpplStmtInfix" in
       let #var"RtpplStmtAtom" = nameSym "RtpplStmtAtom" in
       let #var"RtpplStmtNoIdentPostfix" = nameSym "RtpplStmtNoIdentPostfix"
       in
       let #var"RtpplStmtNoIdentPrefix" = nameSym "RtpplStmtNoIdentPrefix"
       in
       let #var"RtpplStmtNoIdentInfix" = nameSym "RtpplStmtNoIdentInfix"
       in
       let #var"RtpplStmtNoIdentAtom" = nameSym "RtpplStmtNoIdentAtom"
       in
       let #var"RtpplExprPostfix" = nameSym "RtpplExprPostfix" in
       let #var"RtpplExprPrefix" = nameSym "RtpplExprPrefix" in
       let #var"RtpplExprInfix" = nameSym "RtpplExprInfix" in
       let #var"RtpplExprAtom" = nameSym "RtpplExprAtom" in
       let #var"RtpplExprNoIdentPostfix" = nameSym "RtpplExprNoIdentPostfix"
       in
       let #var"RtpplExprNoIdentPrefix" = nameSym "RtpplExprNoIdentPrefix"
       in
       let #var"RtpplExprNoIdentInfix" = nameSym "RtpplExprNoIdentInfix"
       in
       let #var"RtpplExprNoIdentAtom" = nameSym "RtpplExprNoIdentAtom"
       in
       let #var"RtpplTypePostfix" = nameSym "RtpplTypePostfix" in
       let #var"RtpplTypePrefix" = nameSym "RtpplTypePrefix" in
       let #var"RtpplTypeInfix" = nameSym "RtpplTypeInfix" in
       let #var"RtpplTypeAtom" = nameSym "RtpplTypeAtom" in
       let #var"RtpplTypeNoIdentPostfix" = nameSym "RtpplTypeNoIdentPostfix"
       in
       let #var"RtpplTypeNoIdentPrefix" = nameSym "RtpplTypeNoIdentPrefix"
       in
       let #var"RtpplTypeNoIdentInfix" = nameSym "RtpplTypeNoIdentInfix"
       in
       let #var"RtpplTypeNoIdentAtom" = nameSym "RtpplTypeNoIdentAtom"
       in
       let #var"RtpplConstPostfix" = nameSym "RtpplConstPostfix" in
       let #var"RtpplConstPrefix" = nameSym "RtpplConstPrefix" in
       let #var"RtpplConstInfix" = nameSym "RtpplConstInfix" in
       let #var"RtpplConstAtom" = nameSym "RtpplConstAtom" in
       let #var"RtpplPortPostfix" = nameSym "RtpplPortPostfix" in
       let #var"RtpplPortPrefix" = nameSym "RtpplPortPrefix" in
       let #var"RtpplPortInfix" = nameSym "RtpplPortInfix" in
       let #var"RtpplPortAtom" = nameSym "RtpplPortAtom" in
       let #var"RtpplMainPostfix" = nameSym "RtpplMainPostfix" in
       let #var"RtpplMainPrefix" = nameSym "RtpplMainPrefix" in
       let #var"RtpplMainInfix" = nameSym "RtpplMainInfix" in
       let #var"RtpplMainAtom" = nameSym "RtpplMainAtom" in
       let #var"RtpplExtPostfix" = nameSym "RtpplExtPostfix" in
       let #var"RtpplExtPrefix" = nameSym "RtpplExtPrefix" in
       let #var"RtpplExtInfix" = nameSym "RtpplExtInfix" in
       let #var"RtpplExtAtom" = nameSym "RtpplExtAtom" in
       let #var"RtpplTaskPostfix" = nameSym "RtpplTaskPostfix" in
       let #var"RtpplTaskPrefix" = nameSym "RtpplTaskPrefix" in
       let #var"RtpplTaskInfix" = nameSym "RtpplTaskInfix" in
       let #var"RtpplTaskAtom" = nameSym "RtpplTaskAtom" in
       let #var"RtpplConnectionPostfix" = nameSym "RtpplConnectionPostfix"
       in
       let #var"RtpplConnectionPrefix" = nameSym "RtpplConnectionPrefix"
       in
       let #var"RtpplConnectionInfix" = nameSym "RtpplConnectionInfix"
       in
       let #var"RtpplConnectionAtom" = nameSym "RtpplConnectionAtom"
       in
       let #var"RtpplPortSpecPostfix" = nameSym "RtpplPortSpecPostfix"
       in
       let #var"RtpplPortSpecPrefix" = nameSym "RtpplPortSpecPrefix"
       in
       let #var"RtpplPortSpecInfix" = nameSym "RtpplPortSpecInfix" in
       let #var"RtpplPortSpecAtom" = nameSym "RtpplPortSpecAtom" in
       let kleene = nameSym "kleene" in
       let kleene1 = nameSym "kleene" in
       let alt = nameSym "alt" in
       let kleene2 = nameSym "kleene" in
       let alt1 = nameSym "alt" in
       let kleene3 = nameSym "kleene" in
       let kleene4 = nameSym "kleene" in
       let kleene5 = nameSym "kleene" in
       let alt2 = nameSym "alt" in
       let alt3 = nameSym "alt" in
       let alt4 = nameSym "alt" in
       let alt5 = nameSym "alt" in
       let alt6 = nameSym "alt" in
       let alt7 = nameSym "alt" in
       let alt8 = nameSym "alt" in
       let kleene6 = nameSym "kleene" in
       let kleene7 = nameSym "kleene" in
       let alt9 = nameSym "alt" in
       let kleene8 = nameSym "kleene" in
       let alt10 = nameSym "alt" in
       let kleene9 = nameSym "kleene" in
       let alt11 = nameSym "alt" in
       let alt12 = nameSym "alt" in
       let kleene10 = nameSym "kleene" in
       let alt13 = nameSym "alt" in
       let kleene11 = nameSym "kleene" in
       let alt14 = nameSym "alt" in
       let kleene12 = nameSym "kleene" in
       let alt15 = nameSym "alt" in
       let kleene13 = nameSym "kleene" in
       let alt16 = nameSym "alt" in
       let kleene14 = nameSym "kleene" in
       let kleene15 = nameSym "kleene" in
       let kleene16 = nameSym "kleene" in
       let kleene17 = nameSym "kleene" in
       let alt17 = nameSym "alt" in
       let alt18 = nameSym "alt" in
       let kleene18 = nameSym "kleene" in
       let alt19 = nameSym "alt" in
       let kleene19 = nameSym "kleene" in
       let #var"RtpplProgram_lclosed" = nameSym "RtpplProgram_lclosed"
       in
       let #var"RtpplProgram_lopen" = nameSym "RtpplProgram_lopen" in
       let #var"RtpplTop_lclosed" = nameSym "RtpplTop_lclosed" in
       let #var"RtpplTop_lopen" = nameSym "RtpplTop_lopen" in
       let #var"RtpplTopParams_lclosed" = nameSym "RtpplTopParams_lclosed"
       in
       let #var"RtpplTopParams_lopen" = nameSym "RtpplTopParams_lopen"
       in
       let #var"RtpplStmt_lclosed" = nameSym "RtpplStmt_lclosed" in
       let #var"RtpplStmt_lopen" = nameSym "RtpplStmt_lopen" in
       let #var"RtpplStmtNoIdent_lclosed" = nameSym "RtpplStmtNoIdent_lclosed"
       in
       let #var"RtpplStmtNoIdent_lopen" = nameSym "RtpplStmtNoIdent_lopen"
       in
       let #var"RtpplExpr_lclosed" = nameSym "RtpplExpr_lclosed" in
       let #var"RtpplExpr_lopen" = nameSym "RtpplExpr_lopen" in
       let #var"RtpplExprNoIdent_lclosed" = nameSym "RtpplExprNoIdent_lclosed"
       in
       let #var"RtpplExprNoIdent_lopen" = nameSym "RtpplExprNoIdent_lopen"
       in
       let #var"RtpplType_lclosed" = nameSym "RtpplType_lclosed" in
       let #var"RtpplType_lopen" = nameSym "RtpplType_lopen" in
       let #var"RtpplTypeNoIdent_lclosed" = nameSym "RtpplTypeNoIdent_lclosed"
       in
       let #var"RtpplTypeNoIdent_lopen" = nameSym "RtpplTypeNoIdent_lopen"
       in
       let #var"RtpplConst_lclosed" = nameSym "RtpplConst_lclosed" in
       let #var"RtpplConst_lopen" = nameSym "RtpplConst_lopen" in
       let #var"RtpplPort_lclosed" = nameSym "RtpplPort_lclosed" in
       let #var"RtpplPort_lopen" = nameSym "RtpplPort_lopen" in
       let #var"RtpplMain_lclosed" = nameSym "RtpplMain_lclosed" in
       let #var"RtpplMain_lopen" = nameSym "RtpplMain_lopen" in
       let #var"RtpplExt_lclosed" = nameSym "RtpplExt_lclosed" in
       let #var"RtpplExt_lopen" = nameSym "RtpplExt_lopen" in
       let #var"RtpplTask_lclosed" = nameSym "RtpplTask_lclosed" in
       let #var"RtpplTask_lopen" = nameSym "RtpplTask_lopen" in
       let #var"RtpplConnection_lclosed" = nameSym "RtpplConnection_lclosed"
       in
       let #var"RtpplConnection_lopen" = nameSym "RtpplConnection_lopen"
       in
       let #var"RtpplPortSpec_lclosed" = nameSym "RtpplPortSpec_lclosed"
       in
       let #var"RtpplPortSpec_lopen" = nameSym "RtpplPortSpec_lopen"
       in
       { start = #var"RtpplProgram",
         productions =
           let config =
             { parenAllowed = #frozen"parenAllowed_RtpplProgramOp",
               topAllowed = #frozen"topAllowed_RtpplProgramOp",
               leftAllowed = #frozen"leftAllowed_RtpplProgramOp",
               rightAllowed = #frozen"rightAllowed_RtpplProgramOp",
               groupingsAllowed = #frozen"groupingsAllowed_RtpplProgramOp" }
           in
           let reportConfig =
             { parenAllowed = #frozen"parenAllowed_RtpplProgramOp",
               topAllowed = #frozen"topAllowed_RtpplProgramOp",
               terminalInfos = #frozen"getTerms_RtpplProgramOp",
               getInfo = #frozen"getInfo_RtpplProgramOp",
               lpar = "(",
               rpar = ")" }
           in
           let addRtpplProgramOpAtom =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddAtom config x54) st
           in
           let addRtpplProgramOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (getInfo_RtpplProgramOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addRtpplProgramOpPrefix =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddPrefix config x54) st
           in
           let addRtpplProgramOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (getInfo_RtpplProgramOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeRtpplProgramOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res152 =
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
                          let res152 = unsplit_RtpplProgramOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res152
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res152.0, BadRtpplProgram
                                { info = res152.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished RtpplProgram")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadRtpplProgram
                     { info = NoInfo
                           {} })
                   res152
           in
           let config1 =
             { parenAllowed = #frozen"parenAllowed_RtpplTopOp",
               topAllowed = #frozen"topAllowed_RtpplTopOp",
               leftAllowed = #frozen"leftAllowed_RtpplTopOp",
               rightAllowed = #frozen"rightAllowed_RtpplTopOp",
               groupingsAllowed = #frozen"groupingsAllowed_RtpplTopOp" }
           in
           let reportConfig1 =
             { parenAllowed = #frozen"parenAllowed_RtpplTopOp",
               topAllowed = #frozen"topAllowed_RtpplTopOp",
               terminalInfos = #frozen"getTerms_RtpplTopOp",
               getInfo = #frozen"getInfo_RtpplTopOp",
               lpar = "(",
               rpar = ")" }
           in
           let addRtpplTopOpAtom =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddAtom config1 x54) st
           in
           let addRtpplTopOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config1 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc (deref p.errors) (getInfo_RtpplTopOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addRtpplTopOpPrefix =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddPrefix config1 x54) st
           in
           let addRtpplTopOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config1 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc (deref p.errors) (getInfo_RtpplTopOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeRtpplTopOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res152 =
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
                          let res152 = unsplit_RtpplTopOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res152
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res152.0, BadRtpplTop
                                { info = res152.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished RtpplTop")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadRtpplTop
                     { info = NoInfo
                           {} })
                   res152
           in
           let config2 =
             { parenAllowed = #frozen"parenAllowed_RtpplTopParamsOp",
               topAllowed = #frozen"topAllowed_RtpplTopParamsOp",
               leftAllowed = #frozen"leftAllowed_RtpplTopParamsOp",
               rightAllowed = #frozen"rightAllowed_RtpplTopParamsOp",
               groupingsAllowed = #frozen"groupingsAllowed_RtpplTopParamsOp" }
           in
           let reportConfig2 =
             { parenAllowed = #frozen"parenAllowed_RtpplTopParamsOp",
               topAllowed = #frozen"topAllowed_RtpplTopParamsOp",
               terminalInfos = #frozen"getTerms_RtpplTopParamsOp",
               getInfo = #frozen"getInfo_RtpplTopParamsOp",
               lpar = "(",
               rpar = ")" }
           in
           let addRtpplTopParamsOpAtom =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddAtom config2 x54) st
           in
           let addRtpplTopParamsOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config2 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (getInfo_RtpplTopParamsOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addRtpplTopParamsOpPrefix =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddPrefix config2 x54) st
           in
           let addRtpplTopParamsOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config2 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (getInfo_RtpplTopParamsOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeRtpplTopParamsOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res152 =
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
                          let res152 = unsplit_RtpplTopParamsOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res152
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res152.0, BadRtpplTopParams
                                { info = res152.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished RtpplTopParams")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadRtpplTopParams
                     { info = NoInfo
                           {} })
                   res152
           in
           let config3 =
             { parenAllowed = #frozen"parenAllowed_RtpplStmtOp",
               topAllowed = #frozen"topAllowed_RtpplStmtOp",
               leftAllowed = #frozen"leftAllowed_RtpplStmtOp",
               rightAllowed = #frozen"rightAllowed_RtpplStmtOp",
               groupingsAllowed = #frozen"groupingsAllowed_RtpplStmtOp" }
           in
           let reportConfig3 =
             { parenAllowed = #frozen"parenAllowed_RtpplStmtOp",
               topAllowed = #frozen"topAllowed_RtpplStmtOp",
               terminalInfos = #frozen"getTerms_RtpplStmtOp",
               getInfo = #frozen"getInfo_RtpplStmtOp",
               lpar = "(",
               rpar = ")" }
           in
           let addRtpplStmtOpAtom =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddAtom config3 x54) st
           in
           let addRtpplStmtOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config3 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc (deref p.errors) (getInfo_RtpplStmtOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addRtpplStmtOpPrefix =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddPrefix config3 x54) st
           in
           let addRtpplStmtOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config3 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc (deref p.errors) (getInfo_RtpplStmtOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeRtpplStmtOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res152 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse config3 st
                        with
                          Some (tops & ([ top ] ++ _))
                        then
                          let errs = breakableDefaultHighlight reportConfig3 p.content tops
                          in
                          let res152 = unsplit_RtpplStmtOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res152
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res152.0, BadRtpplStmt
                                { info = res152.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished RtpplStmt")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadRtpplStmt
                     { info = NoInfo
                           {} })
                   res152
           in
           let config4 =
             { parenAllowed = #frozen"parenAllowed_RtpplStmtNoIdentOp",
               topAllowed = #frozen"topAllowed_RtpplStmtNoIdentOp",
               leftAllowed = #frozen"leftAllowed_RtpplStmtNoIdentOp",
               rightAllowed = #frozen"rightAllowed_RtpplStmtNoIdentOp",
               groupingsAllowed =
                 #frozen"groupingsAllowed_RtpplStmtNoIdentOp" }
           in
           let reportConfig4 =
             { parenAllowed = #frozen"parenAllowed_RtpplStmtNoIdentOp",
               topAllowed = #frozen"topAllowed_RtpplStmtNoIdentOp",
               terminalInfos = #frozen"getTerms_RtpplStmtNoIdentOp",
               getInfo = #frozen"getInfo_RtpplStmtNoIdentOp",
               lpar = "(",
               rpar = ")" }
           in
           let addRtpplStmtNoIdentOpAtom =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddAtom config4 x54) st
           in
           let addRtpplStmtNoIdentOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config4 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (getInfo_RtpplStmtNoIdentOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addRtpplStmtNoIdentOpPrefix =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddPrefix config4 x54) st
           in
           let addRtpplStmtNoIdentOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config4 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (getInfo_RtpplStmtNoIdentOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeRtpplStmtNoIdentOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res152 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse config4 st
                        with
                          Some (tops & ([ top ] ++ _))
                        then
                          let errs = breakableDefaultHighlight reportConfig4 p.content tops
                          in
                          let res152 = unsplit_RtpplStmtNoIdentOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res152
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res152.0, BadRtpplStmtNoIdent
                                { info = res152.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished RtpplStmtNoIdent")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadRtpplStmtNoIdent
                     { info = NoInfo
                           {} })
                   res152
           in
           let config5 =
             { parenAllowed = #frozen"parenAllowed_RtpplExprOp",
               topAllowed = #frozen"topAllowed_RtpplExprOp",
               leftAllowed = #frozen"leftAllowed_RtpplExprOp",
               rightAllowed = #frozen"rightAllowed_RtpplExprOp",
               groupingsAllowed = #frozen"groupingsAllowed_RtpplExprOp" }
           in
           let reportConfig5 =
             { parenAllowed = #frozen"parenAllowed_RtpplExprOp",
               topAllowed = #frozen"topAllowed_RtpplExprOp",
               terminalInfos = #frozen"getTerms_RtpplExprOp",
               getInfo = #frozen"getInfo_RtpplExprOp",
               lpar = "(",
               rpar = ")" }
           in
           let addRtpplExprOpAtom =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddAtom config5 x54) st
           in
           let addRtpplExprOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config5 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc (deref p.errors) (getInfo_RtpplExprOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addRtpplExprOpPrefix =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddPrefix config5 x54) st
           in
           let addRtpplExprOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config5 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc (deref p.errors) (getInfo_RtpplExprOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeRtpplExprOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res152 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse config5 st
                        with
                          Some (tops & ([ top ] ++ _))
                        then
                          let errs = breakableDefaultHighlight reportConfig5 p.content tops
                          in
                          let res152 = unsplit_RtpplExprOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res152
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res152.0, BadRtpplExpr
                                { info = res152.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished RtpplExpr")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadRtpplExpr
                     { info = NoInfo
                           {} })
                   res152
           in
           let config6 =
             { parenAllowed = #frozen"parenAllowed_RtpplExprNoIdentOp",
               topAllowed = #frozen"topAllowed_RtpplExprNoIdentOp",
               leftAllowed = #frozen"leftAllowed_RtpplExprNoIdentOp",
               rightAllowed = #frozen"rightAllowed_RtpplExprNoIdentOp",
               groupingsAllowed =
                 #frozen"groupingsAllowed_RtpplExprNoIdentOp" }
           in
           let reportConfig6 =
             { parenAllowed = #frozen"parenAllowed_RtpplExprNoIdentOp",
               topAllowed = #frozen"topAllowed_RtpplExprNoIdentOp",
               terminalInfos = #frozen"getTerms_RtpplExprNoIdentOp",
               getInfo = #frozen"getInfo_RtpplExprNoIdentOp",
               lpar = "(",
               rpar = ")" }
           in
           let addRtpplExprNoIdentOpAtom =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddAtom config6 x54) st
           in
           let addRtpplExprNoIdentOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config6 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (getInfo_RtpplExprNoIdentOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addRtpplExprNoIdentOpPrefix =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddPrefix config6 x54) st
           in
           let addRtpplExprNoIdentOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config6 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (getInfo_RtpplExprNoIdentOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeRtpplExprNoIdentOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res152 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse config6 st
                        with
                          Some (tops & ([ top ] ++ _))
                        then
                          let errs = breakableDefaultHighlight reportConfig6 p.content tops
                          in
                          let res152 = unsplit_RtpplExprNoIdentOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res152
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res152.0, BadRtpplExprNoIdent
                                { info = res152.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished RtpplExprNoIdent")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadRtpplExprNoIdent
                     { info = NoInfo
                           {} })
                   res152
           in
           let config7 =
             { parenAllowed = #frozen"parenAllowed_RtpplTypeOp",
               topAllowed = #frozen"topAllowed_RtpplTypeOp",
               leftAllowed = #frozen"leftAllowed_RtpplTypeOp",
               rightAllowed = #frozen"rightAllowed_RtpplTypeOp",
               groupingsAllowed = #frozen"groupingsAllowed_RtpplTypeOp" }
           in
           let reportConfig7 =
             { parenAllowed = #frozen"parenAllowed_RtpplTypeOp",
               topAllowed = #frozen"topAllowed_RtpplTypeOp",
               terminalInfos = #frozen"getTerms_RtpplTypeOp",
               getInfo = #frozen"getInfo_RtpplTypeOp",
               lpar = "(",
               rpar = ")" }
           in
           let addRtpplTypeOpAtom =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddAtom config7 x54) st
           in
           let addRtpplTypeOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config7 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc (deref p.errors) (getInfo_RtpplTypeOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addRtpplTypeOpPrefix =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddPrefix config7 x54) st
           in
           let addRtpplTypeOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config7 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc (deref p.errors) (getInfo_RtpplTypeOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeRtpplTypeOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res152 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse config7 st
                        with
                          Some (tops & ([ top ] ++ _))
                        then
                          let errs = breakableDefaultHighlight reportConfig7 p.content tops
                          in
                          let res152 = unsplit_RtpplTypeOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res152
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res152.0, BadRtpplType
                                { info = res152.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished RtpplType")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadRtpplType
                     { info = NoInfo
                           {} })
                   res152
           in
           let config8 =
             { parenAllowed = #frozen"parenAllowed_RtpplTypeNoIdentOp",
               topAllowed = #frozen"topAllowed_RtpplTypeNoIdentOp",
               leftAllowed = #frozen"leftAllowed_RtpplTypeNoIdentOp",
               rightAllowed = #frozen"rightAllowed_RtpplTypeNoIdentOp",
               groupingsAllowed =
                 #frozen"groupingsAllowed_RtpplTypeNoIdentOp" }
           in
           let reportConfig8 =
             { parenAllowed = #frozen"parenAllowed_RtpplTypeNoIdentOp",
               topAllowed = #frozen"topAllowed_RtpplTypeNoIdentOp",
               terminalInfos = #frozen"getTerms_RtpplTypeNoIdentOp",
               getInfo = #frozen"getInfo_RtpplTypeNoIdentOp",
               lpar = "(",
               rpar = ")" }
           in
           let addRtpplTypeNoIdentOpAtom =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddAtom config8 x54) st
           in
           let addRtpplTypeNoIdentOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config8 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (getInfo_RtpplTypeNoIdentOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addRtpplTypeNoIdentOpPrefix =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddPrefix config8 x54) st
           in
           let addRtpplTypeNoIdentOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config8 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (getInfo_RtpplTypeNoIdentOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeRtpplTypeNoIdentOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res152 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse config8 st
                        with
                          Some (tops & ([ top ] ++ _))
                        then
                          let errs = breakableDefaultHighlight reportConfig8 p.content tops
                          in
                          let res152 = unsplit_RtpplTypeNoIdentOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res152
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res152.0, BadRtpplTypeNoIdent
                                { info = res152.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished RtpplTypeNoIdent")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadRtpplTypeNoIdent
                     { info = NoInfo
                           {} })
                   res152
           in
           let config9 =
             { parenAllowed = #frozen"parenAllowed_RtpplConstOp",
               topAllowed = #frozen"topAllowed_RtpplConstOp",
               leftAllowed = #frozen"leftAllowed_RtpplConstOp",
               rightAllowed = #frozen"rightAllowed_RtpplConstOp",
               groupingsAllowed = #frozen"groupingsAllowed_RtpplConstOp" }
           in
           let reportConfig9 =
             { parenAllowed = #frozen"parenAllowed_RtpplConstOp",
               topAllowed = #frozen"topAllowed_RtpplConstOp",
               terminalInfos = #frozen"getTerms_RtpplConstOp",
               getInfo = #frozen"getInfo_RtpplConstOp",
               lpar = "(",
               rpar = ")" }
           in
           let addRtpplConstOpAtom =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddAtom config9 x54) st
           in
           let addRtpplConstOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config9 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors) (getInfo_RtpplConstOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addRtpplConstOpPrefix =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddPrefix config9 x54) st
           in
           let addRtpplConstOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config9 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors) (getInfo_RtpplConstOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeRtpplConstOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res152 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse config9 st
                        with
                          Some (tops & ([ top ] ++ _))
                        then
                          let errs = breakableDefaultHighlight reportConfig9 p.content tops
                          in
                          let res152 = unsplit_RtpplConstOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res152
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res152.0, BadRtpplConst
                                { info = res152.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished RtpplConst")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadRtpplConst
                     { info = NoInfo
                           {} })
                   res152
           in
           let config10 =
             { parenAllowed = #frozen"parenAllowed_RtpplPortOp",
               topAllowed = #frozen"topAllowed_RtpplPortOp",
               leftAllowed = #frozen"leftAllowed_RtpplPortOp",
               rightAllowed = #frozen"rightAllowed_RtpplPortOp",
               groupingsAllowed = #frozen"groupingsAllowed_RtpplPortOp" }
           in
           let reportConfig10 =
             { parenAllowed = #frozen"parenAllowed_RtpplPortOp",
               topAllowed = #frozen"topAllowed_RtpplPortOp",
               terminalInfos = #frozen"getTerms_RtpplPortOp",
               getInfo = #frozen"getInfo_RtpplPortOp",
               lpar = "(",
               rpar = ")" }
           in
           let addRtpplPortOpAtom =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddAtom config10 x54) st
           in
           let addRtpplPortOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config10 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc (deref p.errors) (getInfo_RtpplPortOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addRtpplPortOpPrefix =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddPrefix config10 x54) st
           in
           let addRtpplPortOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config10 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc (deref p.errors) (getInfo_RtpplPortOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeRtpplPortOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res152 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse config10 st
                        with
                          Some (tops & ([ top ] ++ _))
                        then
                          let errs = breakableDefaultHighlight reportConfig10 p.content tops
                          in
                          let res152 = unsplit_RtpplPortOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res152
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res152.0, BadRtpplPort
                                { info = res152.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished RtpplPort")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadRtpplPort
                     { info = NoInfo
                           {} })
                   res152
           in
           let config11 =
             { parenAllowed = #frozen"parenAllowed_RtpplMainOp",
               topAllowed = #frozen"topAllowed_RtpplMainOp",
               leftAllowed = #frozen"leftAllowed_RtpplMainOp",
               rightAllowed = #frozen"rightAllowed_RtpplMainOp",
               groupingsAllowed = #frozen"groupingsAllowed_RtpplMainOp" }
           in
           let reportConfig11 =
             { parenAllowed = #frozen"parenAllowed_RtpplMainOp",
               topAllowed = #frozen"topAllowed_RtpplMainOp",
               terminalInfos = #frozen"getTerms_RtpplMainOp",
               getInfo = #frozen"getInfo_RtpplMainOp",
               lpar = "(",
               rpar = ")" }
           in
           let addRtpplMainOpAtom =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddAtom config11 x54) st
           in
           let addRtpplMainOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config11 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc (deref p.errors) (getInfo_RtpplMainOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addRtpplMainOpPrefix =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddPrefix config11 x54) st
           in
           let addRtpplMainOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config11 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc (deref p.errors) (getInfo_RtpplMainOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeRtpplMainOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res152 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse config11 st
                        with
                          Some (tops & ([ top ] ++ _))
                        then
                          let errs = breakableDefaultHighlight reportConfig11 p.content tops
                          in
                          let res152 = unsplit_RtpplMainOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res152
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res152.0, BadRtpplMain
                                { info = res152.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished RtpplMain")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadRtpplMain
                     { info = NoInfo
                           {} })
                   res152
           in
           let config12 =
             { parenAllowed = #frozen"parenAllowed_RtpplExtOp",
               topAllowed = #frozen"topAllowed_RtpplExtOp",
               leftAllowed = #frozen"leftAllowed_RtpplExtOp",
               rightAllowed = #frozen"rightAllowed_RtpplExtOp",
               groupingsAllowed = #frozen"groupingsAllowed_RtpplExtOp" }
           in
           let reportConfig12 =
             { parenAllowed = #frozen"parenAllowed_RtpplExtOp",
               topAllowed = #frozen"topAllowed_RtpplExtOp",
               terminalInfos = #frozen"getTerms_RtpplExtOp",
               getInfo = #frozen"getInfo_RtpplExtOp",
               lpar = "(",
               rpar = ")" }
           in
           let addRtpplExtOpAtom =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddAtom config12 x54) st
           in
           let addRtpplExtOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config12 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc (deref p.errors) (getInfo_RtpplExtOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addRtpplExtOpPrefix =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddPrefix config12 x54) st
           in
           let addRtpplExtOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config12 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc (deref p.errors) (getInfo_RtpplExtOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeRtpplExtOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res152 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse config12 st
                        with
                          Some (tops & ([ top ] ++ _))
                        then
                          let errs = breakableDefaultHighlight reportConfig12 p.content tops
                          in
                          let res152 = unsplit_RtpplExtOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res152
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res152.0, BadRtpplExt
                                { info = res152.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished RtpplExt")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadRtpplExt
                     { info = NoInfo
                           {} })
                   res152
           in
           let config13 =
             { parenAllowed = #frozen"parenAllowed_RtpplTaskOp",
               topAllowed = #frozen"topAllowed_RtpplTaskOp",
               leftAllowed = #frozen"leftAllowed_RtpplTaskOp",
               rightAllowed = #frozen"rightAllowed_RtpplTaskOp",
               groupingsAllowed = #frozen"groupingsAllowed_RtpplTaskOp" }
           in
           let reportConfig13 =
             { parenAllowed = #frozen"parenAllowed_RtpplTaskOp",
               topAllowed = #frozen"topAllowed_RtpplTaskOp",
               terminalInfos = #frozen"getTerms_RtpplTaskOp",
               getInfo = #frozen"getInfo_RtpplTaskOp",
               lpar = "(",
               rpar = ")" }
           in
           let addRtpplTaskOpAtom =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddAtom config13 x54) st
           in
           let addRtpplTaskOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config13 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc (deref p.errors) (getInfo_RtpplTaskOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addRtpplTaskOpPrefix =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddPrefix config13 x54) st
           in
           let addRtpplTaskOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config13 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc (deref p.errors) (getInfo_RtpplTaskOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeRtpplTaskOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res152 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse config13 st
                        with
                          Some (tops & ([ top ] ++ _))
                        then
                          let errs = breakableDefaultHighlight reportConfig13 p.content tops
                          in
                          let res152 = unsplit_RtpplTaskOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res152
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res152.0, BadRtpplTask
                                { info = res152.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished RtpplTask")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadRtpplTask
                     { info = NoInfo
                           {} })
                   res152
           in
           let config14 =
             { parenAllowed = #frozen"parenAllowed_RtpplConnectionOp",
               topAllowed = #frozen"topAllowed_RtpplConnectionOp",
               leftAllowed = #frozen"leftAllowed_RtpplConnectionOp",
               rightAllowed = #frozen"rightAllowed_RtpplConnectionOp",
               groupingsAllowed = #frozen"groupingsAllowed_RtpplConnectionOp" }
           in
           let reportConfig14 =
             { parenAllowed = #frozen"parenAllowed_RtpplConnectionOp",
               topAllowed = #frozen"topAllowed_RtpplConnectionOp",
               terminalInfos = #frozen"getTerms_RtpplConnectionOp",
               getInfo = #frozen"getInfo_RtpplConnectionOp",
               lpar = "(",
               rpar = ")" }
           in
           let addRtpplConnectionOpAtom =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddAtom config14 x54) st
           in
           let addRtpplConnectionOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config14 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (getInfo_RtpplConnectionOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addRtpplConnectionOpPrefix =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddPrefix config14 x54) st
           in
           let addRtpplConnectionOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config14 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (getInfo_RtpplConnectionOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeRtpplConnectionOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res152 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse config14 st
                        with
                          Some (tops & ([ top ] ++ _))
                        then
                          let errs = breakableDefaultHighlight reportConfig14 p.content tops
                          in
                          let res152 = unsplit_RtpplConnectionOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res152
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res152.0, BadRtpplConnection
                                { info = res152.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished RtpplConnection")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadRtpplConnection
                     { info = NoInfo
                           {} })
                   res152
           in
           let config15 =
             { parenAllowed = #frozen"parenAllowed_RtpplPortSpecOp",
               topAllowed = #frozen"topAllowed_RtpplPortSpecOp",
               leftAllowed = #frozen"leftAllowed_RtpplPortSpecOp",
               rightAllowed = #frozen"rightAllowed_RtpplPortSpecOp",
               groupingsAllowed = #frozen"groupingsAllowed_RtpplPortSpecOp" }
           in
           let reportConfig15 =
             { parenAllowed = #frozen"parenAllowed_RtpplPortSpecOp",
               topAllowed = #frozen"topAllowed_RtpplPortSpecOp",
               terminalInfos = #frozen"getTerms_RtpplPortSpecOp",
               getInfo = #frozen"getInfo_RtpplPortSpecOp",
               lpar = "(",
               rpar = ")" }
           in
           let addRtpplPortSpecOpAtom =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddAtom config15 x54) st
           in
           let addRtpplPortSpecOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddInfix config15 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (getInfo_RtpplPortSpecOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addRtpplPortSpecOpPrefix =
             lam #var"".
               lam x54.
                 lam st.
                   optionMap (breakableAddPrefix config15 x54) st
           in
           let addRtpplPortSpecOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x54.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st = breakableAddPostfix config15 x54 st in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref p.errors)
                               (getInfo_RtpplPortSpecOp x54, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeRtpplPortSpecOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res152 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse config15 st
                        with
                          Some (tops & ([ top ] ++ _))
                        then
                          let errs = breakableDefaultHighlight reportConfig15 p.content tops
                          in
                          let res152 = unsplit_RtpplPortSpecOp top in
                          match
                            null errs
                          with
                            true
                          then
                            Some
                              res152
                          else
                            (modref p.errors (concat (deref p.errors) errs))
                            ; Some
                              (res152.0, BadRtpplPortSpec
                                { info = res152.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref p.errors)
                                  (NoInfo
                                    {}, "Unfinished RtpplPortSpec")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadRtpplPortSpec
                     { info = NoInfo
                           {} })
                   res152
           in
           [ { nt = kleene,
               label = {},
               rhs =
                 [ ntSym #var"RtpplTop",
                   ntSym kleene ],
               action =
                 lam state: {errors: Ref [(Info, [Char])], content: String}.
                   lam res.
                     match
                       res
                     with
                       [ UserSym ntVal,
                         UserSym val1 ]
                     in
                     let ntVal: (Info, RtpplTop) = fromDyn ntVal in
                       let val1: {tops: [RtpplTop], __br_info: Info, __br_terms: [Info]} = fromDyn val1
                       in
                       asDyn
                         { __br_info = mergeInfo ntVal.0 val1.__br_info,
                           __br_terms = val1.__br_terms,
                           tops = concat [ ntVal.1 ] val1.tops } },
             { nt = kleene,
               label = {},
               rhs = "",
               action =
                 lam state1: {errors: Ref [(Info, [Char])], content: String}.
                   lam res1.
                     match
                       res1
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           tops = "" } },
             { nt = #var"RtpplProgramAtom",
               label = {},
               rhs =
                 [ ntSym kleene,
                   ntSym #var"RtpplMain" ],
               action =
                 lam state2: {errors: Ref [(Info, [Char])], content: String}.
                   lam res2.
                     match
                       res2
                     with
                       [ UserSym val1,
                         UserSym ntVal1 ]
                     in
                     let val1: {tops: [RtpplTop], __br_info: Info, __br_terms: [Info]} = fromDyn val1
                       in
                       let ntVal1: (Info, RtpplMain) = fromDyn ntVal1 in
                       asDyn
                         (ProgramRtpplProgramOp
                            { __br_info = mergeInfo val1.__br_info ntVal1.0,
                              __br_terms = val1.__br_terms,
                              tops = val1.tops,
                              main = [ ntVal1.1 ] }) },
             { nt = #var"RtpplTopAtom",
               label = {},
               rhs =
                 [ litSym "const",
                   tokSym (LIdentRepr
                        {}),
                   litSym ":",
                   ntSym #var"RtpplType",
                   litSym "=",
                   ntSym #var"RtpplExpr" ],
               action =
                 lam state3: {errors: Ref [(Info, [Char])], content: String}.
                   lam res3.
                     match
                       res3
                     with
                       [ LitParsed l,
                         TokParsed (LIdentTok x),
                         LitParsed l1,
                         UserSym ntVal2,
                         LitParsed l2,
                         UserSym ntVal3 ]
                     in
                     let ntVal2: (Info, RtpplType) = fromDyn ntVal2 in
                       let ntVal3: (Info, RtpplExpr) = fromDyn ntVal3 in
                       asDyn
                         (ConstantRtpplTopOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l.info
                                  [ x.info,
                                    l1.info,
                                    ntVal2.0,
                                    l2.info,
                                    ntVal3.0 ],
                              __br_terms =
                                join
                                  [ [ l.info ],
                                    [ x.info ],
                                    [ l1.info ],
                                    [ l2.info ] ],
                              e = [ ntVal3.1 ],
                              id = [ { v = nameSym x.val, i = x.info } ],
                              ty = [ ntVal2.1 ] }) },
             { nt = #var"RtpplTopAtom",
               label = {},
               rhs =
                 [ litSym "type",
                   tokSym (UIdentRepr
                        {}),
                   litSym "=",
                   ntSym #var"RtpplType" ],
               action =
                 lam state4: {errors: Ref [(Info, [Char])], content: String}.
                   lam res4.
                     match
                       res4
                     with
                       [ LitParsed l3,
                         TokParsed (UIdentTok x1),
                         LitParsed l4,
                         UserSym ntVal4 ]
                     in
                     let ntVal4: (Info, RtpplType) = fromDyn ntVal4 in
                       asDyn
                         (TypeAliasRtpplTopOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l3.info
                                  [ x1.info,
                                    l4.info,
                                    ntVal4.0 ],
                              __br_terms =
                                join
                                  [ [ l3.info ],
                                    [ x1.info ],
                                    [ l4.info ] ],
                              id = [ { v = nameSym x1.val, i = x1.info } ],
                              ty = [ ntVal4.1 ] }) },
             { nt = kleene1,
               label = {},
               rhs =
                 [ ntSym #var"RtpplStmt",
                   ntSym kleene1 ],
               action =
                 lam state5: {errors: Ref [(Info, [Char])], content: String}.
                   lam res5.
                     match
                       res5
                     with
                       [ UserSym ntVal5,
                         UserSym val2 ]
                     in
                     let ntVal5: (Info, RtpplStmt) = fromDyn ntVal5 in
                       let val2: {stmts: [RtpplStmt], __br_info: Info, __br_terms: [Info]} = fromDyn val2
                       in
                       asDyn
                         { __br_info = mergeInfo ntVal5.0 val2.__br_info,
                           __br_terms = val2.__br_terms,
                           stmts = concat [ ntVal5.1 ] val2.stmts } },
             { nt = kleene1,
               label = {},
               rhs = "",
               action =
                 lam state6: {errors: Ref [(Info, [Char])], content: String}.
                   lam res6.
                     match
                       res6
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           stmts = "" } },
             { nt = alt,
               label = {},
               rhs = "",
               action =
                 lam state7: {errors: Ref [(Info, [Char])], content: String}.
                   lam res7.
                     match
                       res7
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           ret = "" } },
             { nt = alt,
               label = {},
               rhs =
                 [ litSym "return",
                   ntSym #var"RtpplExpr" ],
               action =
                 lam state8: {errors: Ref [(Info, [Char])], content: String}.
                   lam res8.
                     match
                       res8
                     with
                       [ LitParsed l5,
                         UserSym ntVal6 ]
                     in
                     let ntVal6: (Info, RtpplExpr) = fromDyn ntVal6 in
                       asDyn
                         { __br_info = mergeInfo l5.info ntVal6.0,
                           __br_terms = [ l5.info ],
                           ret = [ ntVal6.1 ] } },
             { nt = #var"RtpplTopAtom",
               label = {},
               rhs =
                 [ litSym "def",
                   tokSym (LIdentRepr
                        {}),
                   litSym "(",
                   ntSym #var"RtpplTopParams",
                   litSym ")",
                   litSym ":",
                   ntSym #var"RtpplType",
                   litSym "{",
                   ntSym kleene1,
                   ntSym alt,
                   litSym "}" ],
               action =
                 lam state9: {errors: Ref [(Info, [Char])], content: String}.
                   lam res9.
                     match
                       res9
                     with
                       [ LitParsed l6,
                         TokParsed (LIdentTok x2),
                         LitParsed l7,
                         UserSym ntVal7,
                         LitParsed l8,
                         LitParsed l9,
                         UserSym ntVal8,
                         LitParsed l10,
                         UserSym val2,
                         UserSym val3,
                         LitParsed l11 ]
                     in
                     let ntVal7: (Info, RtpplTopParams) = fromDyn ntVal7 in
                       let ntVal8: (Info, RtpplType) = fromDyn ntVal8 in
                       let val2: {stmts: [RtpplStmt], __br_info: Info, __br_terms: [Info]} = fromDyn val2
                       in
                       let val3: {ret: [RtpplExpr], __br_info: Info, __br_terms: [Info]} = fromDyn val3
                       in
                       asDyn
                         (FunctionDefRtpplTopOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l6.info
                                  [ x2.info,
                                    l7.info,
                                    ntVal7.0,
                                    l8.info,
                                    l9.info,
                                    ntVal8.0,
                                    l10.info,
                                    val2.__br_info,
                                    val3.__br_info,
                                    l11.info ],
                              __br_terms =
                                join
                                  [ [ l6.info ],
                                    [ x2.info ],
                                    [ l7.info ],
                                    [ l8.info ],
                                    [ l9.info ],
                                    [ l10.info ],
                                    val2.__br_terms,
                                    val3.__br_terms,
                                    [ l11.info ] ],
                              id = [ { v = nameSym x2.val, i = x2.info } ],
                              ty = [ ntVal8.1 ],
                              body =
                                [ { ret =
                                      match
                                        val3.ret
                                      with
                                        [ x3 ] ++ _
                                      then
                                        Some
                                          x3
                                      else
                                        None
                                          {},
                                    stmts = val2.stmts } ],
                              params = [ ntVal7.1 ] }) },
             { nt = kleene2,
               label = {},
               rhs =
                 [ ntSym #var"RtpplStmt",
                   ntSym kleene2 ],
               action =
                 lam state10: {errors: Ref [(Info, [Char])], content: String}.
                   lam res10.
                     match
                       res10
                     with
                       [ UserSym ntVal9,
                         UserSym val4 ]
                     in
                     let ntVal9: (Info, RtpplStmt) = fromDyn ntVal9 in
                       let val4: {stmts: [RtpplStmt], __br_info: Info, __br_terms: [Info]} = fromDyn val4
                       in
                       asDyn
                         { __br_info = mergeInfo ntVal9.0 val4.__br_info,
                           __br_terms = val4.__br_terms,
                           stmts = concat [ ntVal9.1 ] val4.stmts } },
             { nt = kleene2,
               label = {},
               rhs = "",
               action =
                 lam state11: {errors: Ref [(Info, [Char])], content: String}.
                   lam res11.
                     match
                       res11
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           stmts = "" } },
             { nt = alt1,
               label = {},
               rhs = "",
               action =
                 lam state12: {errors: Ref [(Info, [Char])], content: String}.
                   lam res12.
                     match
                       res12
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           ret = "" } },
             { nt = alt1,
               label = {},
               rhs =
                 [ litSym "return",
                   ntSym #var"RtpplExpr" ],
               action =
                 lam state13: {errors: Ref [(Info, [Char])], content: String}.
                   lam res13.
                     match
                       res13
                     with
                       [ LitParsed l12,
                         UserSym ntVal10 ]
                     in
                     let ntVal10: (Info, RtpplExpr) = fromDyn ntVal10 in
                       asDyn
                         { __br_info = mergeInfo l12.info ntVal10.0,
                           __br_terms = [ l12.info ],
                           ret = [ ntVal10.1 ] } },
             { nt = #var"RtpplTopAtom",
               label = {},
               rhs =
                 [ litSym "model",
                   tokSym (LIdentRepr
                        {}),
                   litSym "(",
                   ntSym #var"RtpplTopParams",
                   litSym ")",
                   litSym ":",
                   ntSym #var"RtpplType",
                   litSym "{",
                   ntSym kleene2,
                   ntSym alt1,
                   litSym "}" ],
               action =
                 lam state14: {errors: Ref [(Info, [Char])], content: String}.
                   lam res14.
                     match
                       res14
                     with
                       [ LitParsed l13,
                         TokParsed (LIdentTok x4),
                         LitParsed l14,
                         UserSym ntVal11,
                         LitParsed l15,
                         LitParsed l16,
                         UserSym ntVal12,
                         LitParsed l17,
                         UserSym val4,
                         UserSym val5,
                         LitParsed l18 ]
                     in
                     let ntVal11: (Info, RtpplTopParams) = fromDyn ntVal11 in
                       let ntVal12: (Info, RtpplType) = fromDyn ntVal12 in
                       let val4: {stmts: [RtpplStmt], __br_info: Info, __br_terms: [Info]} = fromDyn val4
                       in
                       let val5: {ret: [RtpplExpr], __br_info: Info, __br_terms: [Info]} = fromDyn val5
                       in
                       asDyn
                         (ModelDefRtpplTopOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l13.info
                                  [ x4.info,
                                    l14.info,
                                    ntVal11.0,
                                    l15.info,
                                    l16.info,
                                    ntVal12.0,
                                    l17.info,
                                    val4.__br_info,
                                    val5.__br_info,
                                    l18.info ],
                              __br_terms =
                                join
                                  [ [ l13.info ],
                                    [ x4.info ],
                                    [ l14.info ],
                                    [ l15.info ],
                                    [ l16.info ],
                                    [ l17.info ],
                                    val4.__br_terms,
                                    val5.__br_terms,
                                    [ l18.info ] ],
                              id = [ { v = nameSym x4.val, i = x4.info } ],
                              ty = [ ntVal12.1 ],
                              body =
                                [ { ret =
                                      match
                                        val5.ret
                                      with
                                        [ x5 ] ++ _
                                      then
                                        Some
                                          x5
                                      else
                                        None
                                          {},
                                    stmts = val4.stmts } ],
                              params = [ ntVal11.1 ] }) },
             { nt = kleene3,
               label = {},
               rhs =
                 [ ntSym #var"RtpplPort",
                   ntSym kleene3 ],
               action =
                 lam state15: {errors: Ref [(Info, [Char])], content: String}.
                   lam res15.
                     match
                       res15
                     with
                       [ UserSym ntVal13,
                         UserSym val6 ]
                     in
                     let ntVal13: (Info, RtpplPort) = fromDyn ntVal13 in
                       let val6: {ports: [RtpplPort], __br_info: Info, __br_terms: [Info]} = fromDyn val6
                       in
                       asDyn
                         { __br_info = mergeInfo ntVal13.0 val6.__br_info,
                           __br_terms = val6.__br_terms,
                           ports = concat [ ntVal13.1 ] val6.ports } },
             { nt = kleene3,
               label = {},
               rhs = "",
               action =
                 lam state16: {errors: Ref [(Info, [Char])], content: String}.
                   lam res16.
                     match
                       res16
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           ports = "" } },
             { nt = kleene4,
               label = {},
               rhs =
                 [ ntSym #var"RtpplStmt",
                   ntSym kleene4 ],
               action =
                 lam state17: {errors: Ref [(Info, [Char])], content: String}.
                   lam res17.
                     match
                       res17
                     with
                       [ UserSym ntVal14,
                         UserSym val7 ]
                     in
                     let ntVal14: (Info, RtpplStmt) = fromDyn ntVal14 in
                       let val7: {body: [RtpplStmt], __br_info: Info, __br_terms: [Info]} = fromDyn val7
                       in
                       asDyn
                         { __br_info = mergeInfo ntVal14.0 val7.__br_info,
                           __br_terms = val7.__br_terms,
                           body = concat [ ntVal14.1 ] val7.body } },
             { nt = kleene4,
               label = {},
               rhs = "",
               action =
                 lam state18: {errors: Ref [(Info, [Char])], content: String}.
                   lam res18.
                     match
                       res18
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           body = "" } },
             { nt = #var"RtpplTopAtom",
               label = {},
               rhs =
                 [ litSym "template",
                   tokSym (UIdentRepr
                        {}),
                   litSym "(",
                   ntSym #var"RtpplTopParams",
                   litSym ")",
                   litSym "{",
                   ntSym kleene3,
                   ntSym kleene4,
                   litSym "}" ],
               action =
                 lam state19: {errors: Ref [(Info, [Char])], content: String}.
                   lam res19.
                     match
                       res19
                     with
                       [ LitParsed l19,
                         TokParsed (UIdentTok x6),
                         LitParsed l20,
                         UserSym ntVal15,
                         LitParsed l21,
                         LitParsed l22,
                         UserSym val6,
                         UserSym val7,
                         LitParsed l23 ]
                     in
                     let ntVal15: (Info, RtpplTopParams) = fromDyn ntVal15 in
                       let val6: {ports: [RtpplPort], __br_info: Info, __br_terms: [Info]} = fromDyn val6
                       in
                       let val7: {body: [RtpplStmt], __br_info: Info, __br_terms: [Info]} = fromDyn val7
                       in
                       asDyn
                         (TemplateDefRtpplTopOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l19.info
                                  [ x6.info,
                                    l20.info,
                                    ntVal15.0,
                                    l21.info,
                                    l22.info,
                                    val6.__br_info,
                                    val7.__br_info,
                                    l23.info ],
                              __br_terms =
                                join
                                  [ [ l19.info ],
                                    [ x6.info ],
                                    [ l20.info ],
                                    [ l21.info ],
                                    [ l22.info ],
                                    val6.__br_terms,
                                    val7.__br_terms,
                                    [ l23.info ] ],
                              id = [ { v = nameSym x6.val, i = x6.info } ],
                              body = [ { body = val7.body, ports = val6.ports } ],
                              params = [ ntVal15.1 ] }) },
             { nt = kleene5,
               label = {},
               rhs =
                 [ litSym ",",
                   tokSym (LIdentRepr
                        {}),
                   litSym ":",
                   ntSym #var"RtpplType",
                   ntSym kleene5 ],
               action =
                 lam state20: {errors: Ref [(Info, [Char])], content: String}.
                   lam res20.
                     match
                       res20
                     with
                       [ LitParsed l24,
                         TokParsed (LIdentTok x7),
                         LitParsed l25,
                         UserSym ntVal16,
                         UserSym val8 ]
                     in
                     let ntVal16: (Info, RtpplType) = fromDyn ntVal16 in
                       let val8: {params: [{id: {i: Info, v: Name}, ty: RtpplType}], __br_info: Info, __br_terms: [Info]} = fromDyn val8
                       in
                       asDyn
                         { __br_info =
                             foldl
                               mergeInfo
                               l24.info
                               [ x7.info,
                                 l25.info,
                                 ntVal16.0,
                                 val8.__br_info ],
                           __br_terms =
                             join
                               [ [ l24.info ],
                                 [ x7.info ],
                                 [ l25.info ],
                                 val8.__br_terms ],
                           params =
                             concat
                               [ { id =
                                     match
                                       [ { v = nameSym x7.val, i = x7.info } ]
                                     with
                                       [ x8 ] ++ _
                                     in
                                     x8,
                                   ty =
                                     match
                                       [ ntVal16.1 ]
                                     with
                                       [ x9 ] ++ _
                                     in
                                     x9 } ]
                               val8.params } },
             { nt = kleene5,
               label = {},
               rhs = "",
               action =
                 lam state21: {errors: Ref [(Info, [Char])], content: String}.
                   lam res21.
                     match
                       res21
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           params = "" } },
             { nt = alt2,
               label = {},
               rhs = "",
               action =
                 lam state22: {errors: Ref [(Info, [Char])], content: String}.
                   lam res22.
                     match
                       res22
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           params = "" } },
             { nt = alt2,
               label = {},
               rhs =
                 [ tokSym (LIdentRepr
                        {}),
                   litSym ":",
                   ntSym #var"RtpplType",
                   ntSym kleene5 ],
               action =
                 lam state23: {errors: Ref [(Info, [Char])], content: String}.
                   lam res23.
                     match
                       res23
                     with
                       [ TokParsed (LIdentTok x10),
                         LitParsed l26,
                         UserSym ntVal17,
                         UserSym val8 ]
                     in
                     let ntVal17: (Info, RtpplType) = fromDyn ntVal17 in
                       let val8: {params: [{id: {i: Info, v: Name}, ty: RtpplType}], __br_info: Info, __br_terms: [Info]} = fromDyn val8
                       in
                       asDyn
                         { __br_info =
                             foldl
                               mergeInfo
                               x10.info
                               [ l26.info,
                                 ntVal17.0,
                                 val8.__br_info ],
                           __br_terms =
                             join
                               [ [ x10.info ],
                                 [ l26.info ],
                                 val8.__br_terms ],
                           params =
                             concat
                               [ { id =
                                     match
                                       [ { v = nameSym x10.val, i = x10.info } ]
                                     with
                                       [ x11 ] ++ _
                                     in
                                     x11,
                                   ty =
                                     match
                                       [ ntVal17.1 ]
                                     with
                                       [ x12 ] ++ _
                                     in
                                     x12 } ]
                               val8.params } },
             { nt = #var"RtpplTopParamsAtom",
               label = {},
               rhs = [ ntSym alt2 ],
               action =
                 lam state24: {errors: Ref [(Info, [Char])], content: String}.
                   lam res24.
                     match
                       res24
                     with
                       [ UserSym val9 ]
                     in
                     let val9: {params: [{id: {i: Info, v: Name}, ty: RtpplType}], __br_info: Info, __br_terms: [Info]} = fromDyn val9
                       in
                       asDyn
                         (ParamsRtpplTopParamsOp
                            { __br_info = val9.__br_info,
                              __br_terms = val9.__br_terms,
                              params = val9.params }) },
             { nt = #var"RtpplPortAtom",
               label = {},
               rhs =
                 [ litSym "input",
                   tokSym (LIdentRepr
                        {}),
                   litSym ":",
                   ntSym #var"RtpplType" ],
               action =
                 lam state25: {errors: Ref [(Info, [Char])], content: String}.
                   lam res25.
                     match
                       res25
                     with
                       [ LitParsed l27,
                         TokParsed (LIdentTok x13),
                         LitParsed l28,
                         UserSym ntVal18 ]
                     in
                     let ntVal18: (Info, RtpplType) = fromDyn ntVal18 in
                       asDyn
                         (InputRtpplPortOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l27.info
                                  [ x13.info,
                                    l28.info,
                                    ntVal18.0 ],
                              __br_terms =
                                join
                                  [ [ l27.info ],
                                    [ x13.info ],
                                    [ l28.info ] ],
                              id = [ { v = x13.val, i = x13.info } ],
                              ty = [ ntVal18.1 ] }) },
             { nt = #var"RtpplPortAtom",
               label = {},
               rhs =
                 [ litSym "output",
                   tokSym (LIdentRepr
                        {}),
                   litSym ":",
                   ntSym #var"RtpplType" ],
               action =
                 lam state26: {errors: Ref [(Info, [Char])], content: String}.
                   lam res26.
                     match
                       res26
                     with
                       [ LitParsed l29,
                         TokParsed (LIdentTok x14),
                         LitParsed l30,
                         UserSym ntVal19 ]
                     in
                     let ntVal19: (Info, RtpplType) = fromDyn ntVal19 in
                       asDyn
                         (OutputRtpplPortOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l29.info
                                  [ x14.info,
                                    l30.info,
                                    ntVal19.0 ],
                              __br_terms =
                                join
                                  [ [ l29.info ],
                                    [ x14.info ],
                                    [ l30.info ] ],
                              id = [ { v = x14.val, i = x14.info } ],
                              ty = [ ntVal19.1 ] }) },
             { nt = alt3,
               label = {},
               rhs = "",
               action =
                 lam state27: {errors: Ref [(Info, [Char])], content: String}.
                   lam res27.
                     match
                       res27
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           ty = "" } },
             { nt = alt3,
               label = {},
               rhs = [ litSym ":",
                   ntSym #var"RtpplType" ],
               action =
                 lam state28: {errors: Ref [(Info, [Char])], content: String}.
                   lam res28.
                     match
                       res28
                     with
                       [ LitParsed l31,
                         UserSym ntVal20 ]
                     in
                     let ntVal20: (Info, RtpplType) = fromDyn ntVal20 in
                       asDyn
                         { __br_info = mergeInfo l31.info ntVal20.0,
                           __br_terms = [ l31.info ],
                           ty = [ ntVal20.1 ] } },
             { nt = alt4,
               label = {},
               rhs = "",
               action =
                 lam state29: {errors: Ref [(Info, [Char])], content: String}.
                   lam res29.
                     match
                       res29
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           e = "",
                           ty = "" } },
             { nt = alt4,
               label = {},
               rhs =
                 [ ntSym alt3,
                   litSym "=",
                   ntSym #var"RtpplExpr" ],
               action =
                 lam state30: {errors: Ref [(Info, [Char])], content: String}.
                   lam res30.
                     match
                       res30
                     with
                       [ UserSym val10,
                         LitParsed l32,
                         UserSym ntVal21 ]
                     in
                     let val10: {ty: [RtpplType], __br_info: Info, __br_terms: [Info]} = fromDyn val10
                       in
                       let ntVal21: (Info, RtpplExpr) = fromDyn ntVal21 in
                       asDyn
                         { __br_info =
                             foldl
                               mergeInfo
                               val10.__br_info
                               [ l32.info,
                                 ntVal21.0 ],
                           __br_terms = concat val10.__br_terms [ l32.info ],
                           e = [ ntVal21.1 ],
                           ty = val10.ty } },
             { nt = #var"RtpplStmtAtom",
               label = {},
               rhs =
                 [ litSym "var",
                   tokSym (LIdentRepr
                        {}),
                   ntSym alt4 ],
               action =
                 lam state31: {errors: Ref [(Info, [Char])], content: String}.
                   lam res31.
                     match
                       res31
                     with
                       [ LitParsed l33,
                         TokParsed (LIdentTok x15),
                         UserSym val11 ]
                     in
                     let val11: {e: [RtpplExpr], ty: [RtpplType], __br_info: Info, __br_terms: [Info]} = fromDyn val11
                       in
                       asDyn
                         (BindingRtpplStmtOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l33.info
                                  [ x15.info,
                                    val11.__br_info ],
                              __br_terms =
                                join
                                  [ [ l33.info ],
                                    [ x15.info ],
                                    val11.__br_terms ],
                              e = val11.e,
                              id = [ { v = nameSym x15.val, i = x15.info } ],
                              ty = val11.ty }) },
             { nt = #var"RtpplStmtAtom",
               label = {},
               rhs =
                 [ litSym "observe",
                   ntSym #var"RtpplExpr",
                   litSym "~",
                   ntSym #var"RtpplExpr" ],
               action =
                 lam state32: {errors: Ref [(Info, [Char])], content: String}.
                   lam res32.
                     match
                       res32
                     with
                       [ LitParsed l34,
                         UserSym ntVal22,
                         LitParsed l35,
                         UserSym ntVal23 ]
                     in
                     let ntVal22: (Info, RtpplExpr) = fromDyn ntVal22 in
                       let ntVal23: (Info, RtpplExpr) = fromDyn ntVal23 in
                       asDyn
                         (ObserveRtpplStmtOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l34.info
                                  [ ntVal22.0,
                                    l35.info,
                                    ntVal23.0 ],
                              __br_terms = concat [ l34.info ] [ l35.info ],
                              e = [ ntVal22.1 ],
                              d = [ ntVal23.1 ] }) },
             { nt = #var"RtpplStmtAtom",
               label = {},
               rhs =
                 [ litSym "sample",
                   tokSym (LIdentRepr
                        {}),
                   litSym "~",
                   ntSym #var"RtpplExpr" ],
               action =
                 lam state33: {errors: Ref [(Info, [Char])], content: String}.
                   lam res33.
                     match
                       res33
                     with
                       [ LitParsed l36,
                         TokParsed (LIdentTok x16),
                         LitParsed l37,
                         UserSym ntVal24 ]
                     in
                     let ntVal24: (Info, RtpplExpr) = fromDyn ntVal24 in
                       asDyn
                         (AssumeRtpplStmtOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l36.info
                                  [ x16.info,
                                    l37.info,
                                    ntVal24.0 ],
                              __br_terms =
                                join
                                  [ [ l36.info ],
                                    [ x16.info ],
                                    [ l37.info ] ],
                              id = [ { v = nameNoSym x16.val, i = x16.info } ],
                              d = [ ntVal24.1 ] }) },
             { nt = alt5,
               label = {},
               rhs = "",
               action =
                 lam state34: {errors: Ref [(Info, [Char])], content: String}.
                   lam res34.
                     match
                       res34
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           p = "" } },
             { nt = alt5,
               label = {},
               rhs =
                 [ litSym "particles",
                   ntSym #var"RtpplExpr" ],
               action =
                 lam state35: {errors: Ref [(Info, [Char])], content: String}.
                   lam res35.
                     match
                       res35
                     with
                       [ LitParsed l38,
                         UserSym ntVal25 ]
                     in
                     let ntVal25: (Info, RtpplExpr) = fromDyn ntVal25 in
                       asDyn
                         { __br_info = mergeInfo l38.info ntVal25.0,
                           __br_terms = [ l38.info ],
                           p = [ ntVal25.1 ] } },
             { nt = #var"RtpplStmtAtom",
               label = {},
               rhs =
                 [ litSym "infer",
                   ntSym #var"RtpplExpr",
                   litSym "to",
                   tokSym (LIdentRepr
                        {}),
                   ntSym alt5 ],
               action =
                 lam state36: {errors: Ref [(Info, [Char])], content: String}.
                   lam res36.
                     match
                       res36
                     with
                       [ LitParsed l39,
                         UserSym ntVal26,
                         LitParsed l40,
                         TokParsed (LIdentTok x17),
                         UserSym val12 ]
                     in
                     let ntVal26: (Info, RtpplExpr) = fromDyn ntVal26 in
                       let val12: {p: [RtpplExpr], __br_info: Info, __br_terms: [Info]} = fromDyn val12
                       in
                       asDyn
                         (InferRtpplStmtOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l39.info
                                  [ ntVal26.0,
                                    l40.info,
                                    x17.info,
                                    val12.__br_info ],
                              __br_terms =
                                join
                                  [ [ l39.info ],
                                    [ l40.info ],
                                    [ x17.info ],
                                    val12.__br_terms ],
                              id = [ { v = nameNoSym x17.val, i = x17.info } ],
                              p = val12.p,
                              model = [ ntVal26.1 ] }) },
             { nt = #var"RtpplStmtAtom",
               label = {},
               rhs = [ litSym "degenerate" ],
               action =
                 lam state37: {errors: Ref [(Info, [Char])], content: String}.
                   lam res37.
                     match
                       res37
                     with
                       [ LitParsed l41 ]
                     in
                     asDyn
                         (DegenerateRtpplStmtOp
                            { __br_info = l41.info, __br_terms = [ l41.info ] }) },
             { nt = #var"RtpplStmtAtom",
               label = {},
               rhs = [ litSym "resample" ],
               action =
                 lam state38: {errors: Ref [(Info, [Char])], content: String}.
                   lam res38.
                     match
                       res38
                     with
                       [ LitParsed l42 ]
                     in
                     asDyn
                         (ResampleRtpplStmtOp
                            { __br_info = l42.info, __br_terms = [ l42.info ] }) },
             { nt = alt6,
               label = {},
               rhs = "",
               action =
                 lam state39: {errors: Ref [(Info, [Char])], content: String}.
                   lam res39.
                     match
                       res39
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           proj = "" } },
             { nt = alt6,
               label = {},
               rhs =
                 [ litSym ".",
                   tokSym (LIdentRepr
                        {}) ],
               action =
                 lam state40: {errors: Ref [(Info, [Char])], content: String}.
                   lam res40.
                     match
                       res40
                     with
                       [ LitParsed l43,
                         TokParsed (LIdentTok x18) ]
                     in
                     asDyn
                         { __br_info = mergeInfo l43.info x18.info,
                           __br_terms = concat [ l43.info ] [ x18.info ],
                           proj = [ { v = x18.val, i = x18.info } ] } },
             { nt = #var"RtpplStmtAtom",
               label = {},
               rhs =
                 [ litSym "read",
                   tokSym (LIdentRepr
                        {}),
                   litSym "to",
                   tokSym (LIdentRepr
                        {}),
                   ntSym alt6 ],
               action =
                 lam state41: {errors: Ref [(Info, [Char])], content: String}.
                   lam res41.
                     match
                       res41
                     with
                       [ LitParsed l44,
                         TokParsed (LIdentTok x19),
                         LitParsed l45,
                         TokParsed (LIdentTok x20),
                         UserSym val13 ]
                     in
                     let val13: {proj: [{i: Info, v: String}], __br_info: Info, __br_terms: [Info]} = fromDyn val13
                       in
                       asDyn
                         (ReadRtpplStmtOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l44.info
                                  [ x19.info,
                                    l45.info,
                                    x20.info,
                                    val13.__br_info ],
                              __br_terms =
                                join
                                  [ [ l44.info ],
                                    [ x19.info ],
                                    [ l45.info ],
                                    [ x20.info ],
                                    val13.__br_terms ],
                              proj = val13.proj,
                              dst = [ { v = nameSym x20.val, i = x20.info } ],
                              port = [ { v = x19.val, i = x19.info } ] }) },
             { nt = alt7,
               label = {},
               rhs = "",
               action =
                 lam state42: {errors: Ref [(Info, [Char])], content: String}.
                   lam res42.
                     match
                       res42
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           delay = "" } },
             { nt = alt7,
               label = {},
               rhs =
                 [ litSym "offset",
                   ntSym #var"RtpplExpr" ],
               action =
                 lam state43: {errors: Ref [(Info, [Char])], content: String}.
                   lam res43.
                     match
                       res43
                     with
                       [ LitParsed l46,
                         UserSym ntVal27 ]
                     in
                     let ntVal27: (Info, RtpplExpr) = fromDyn ntVal27 in
                       asDyn
                         { __br_info = mergeInfo l46.info ntVal27.0,
                           __br_terms = [ l46.info ],
                           delay = [ ntVal27.1 ] } },
             { nt = #var"RtpplStmtAtom",
               label = {},
               rhs =
                 [ litSym "write",
                   ntSym #var"RtpplExpr",
                   litSym "to",
                   tokSym (LIdentRepr
                        {}),
                   ntSym alt7 ],
               action =
                 lam state44: {errors: Ref [(Info, [Char])], content: String}.
                   lam res44.
                     match
                       res44
                     with
                       [ LitParsed l47,
                         UserSym ntVal28,
                         LitParsed l48,
                         TokParsed (LIdentTok x21),
                         UserSym val14 ]
                     in
                     let ntVal28: (Info, RtpplExpr) = fromDyn ntVal28 in
                       let val14: {delay: [RtpplExpr], __br_info: Info, __br_terms: [Info]} = fromDyn val14
                       in
                       asDyn
                         (WriteRtpplStmtOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l47.info
                                  [ ntVal28.0,
                                    l48.info,
                                    x21.info,
                                    val14.__br_info ],
                              __br_terms =
                                join
                                  [ [ l47.info ],
                                    [ l48.info ],
                                    [ x21.info ],
                                    val14.__br_terms ],
                              port = [ { v = x21.val, i = x21.info } ],
                              delay = val14.delay,
                              src = [ ntVal28.1 ] }) },
             { nt = alt8,
               label = {},
               rhs = "",
               action =
                 lam state45: {errors: Ref [(Info, [Char])], content: String}.
                   lam res45.
                     match
                       res45
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           id = "" } },
             { nt = alt8,
               label = {},
               rhs =
                 [ litSym "update",
                   tokSym (LIdentRepr
                        {}) ],
               action =
                 lam state46: {errors: Ref [(Info, [Char])], content: String}.
                   lam res46.
                     match
                       res46
                     with
                       [ LitParsed l49,
                         TokParsed (LIdentTok x22) ]
                     in
                     asDyn
                         { __br_info = mergeInfo l49.info x22.info,
                           __br_terms = concat [ l49.info ] [ x22.info ],
                           id = [ { v = nameNoSym x22.val, i = x22.info } ] } },
             { nt = kleene6,
               label = {},
               rhs =
                 [ ntSym #var"RtpplStmt",
                   ntSym kleene6 ],
               action =
                 lam state47: {errors: Ref [(Info, [Char])], content: String}.
                   lam res47.
                     match
                       res47
                     with
                       [ UserSym ntVal29,
                         UserSym val15 ]
                     in
                     let ntVal29: (Info, RtpplStmt) = fromDyn ntVal29 in
                       let val15: {thn: [RtpplStmt], __br_info: Info, __br_terms: [Info]} = fromDyn val15
                       in
                       asDyn
                         { __br_info = mergeInfo ntVal29.0 val15.__br_info,
                           __br_terms = val15.__br_terms,
                           thn = concat [ ntVal29.1 ] val15.thn } },
             { nt = kleene6,
               label = {},
               rhs = "",
               action =
                 lam state48: {errors: Ref [(Info, [Char])], content: String}.
                   lam res48.
                     match
                       res48
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           thn = "" } },
             { nt = kleene7,
               label = {},
               rhs =
                 [ ntSym #var"RtpplStmt",
                   ntSym kleene7 ],
               action =
                 lam state49: {errors: Ref [(Info, [Char])], content: String}.
                   lam res49.
                     match
                       res49
                     with
                       [ UserSym ntVal30,
                         UserSym val16 ]
                     in
                     let ntVal30: (Info, RtpplStmt) = fromDyn ntVal30 in
                       let val16: {els: [RtpplStmt], __br_info: Info, __br_terms: [Info]} = fromDyn val16
                       in
                       asDyn
                         { __br_info = mergeInfo ntVal30.0 val16.__br_info,
                           __br_terms = val16.__br_terms,
                           els = concat [ ntVal30.1 ] val16.els } },
             { nt = kleene7,
               label = {},
               rhs = "",
               action =
                 lam state50: {errors: Ref [(Info, [Char])], content: String}.
                   lam res50.
                     match
                       res50
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           els = "" } },
             { nt = #var"RtpplStmtAtom",
               label = {},
               rhs =
                 [ litSym "if",
                   ntSym #var"RtpplExpr",
                   ntSym alt8,
                   litSym "{",
                   ntSym kleene6,
                   litSym "}",
                   litSym "else",
                   litSym "{",
                   ntSym kleene7,
                   litSym "}" ],
               action =
                 lam state51: {errors: Ref [(Info, [Char])], content: String}.
                   lam res51.
                     match
                       res51
                     with
                       [ LitParsed l50,
                         UserSym ntVal31,
                         UserSym val17,
                         LitParsed l51,
                         UserSym val15,
                         LitParsed l52,
                         LitParsed l53,
                         LitParsed l54,
                         UserSym val16,
                         LitParsed l55 ]
                     in
                     let ntVal31: (Info, RtpplExpr) = fromDyn ntVal31 in
                       let val17: {id: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} = fromDyn val17
                       in
                       let val15: {thn: [RtpplStmt], __br_info: Info, __br_terms: [Info]} = fromDyn val15
                       in
                       let val16: {els: [RtpplStmt], __br_info: Info, __br_terms: [Info]} = fromDyn val16
                       in
                       asDyn
                         (ConditionRtpplStmtOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l50.info
                                  [ ntVal31.0,
                                    val17.__br_info,
                                    l51.info,
                                    val15.__br_info,
                                    l52.info,
                                    l53.info,
                                    l54.info,
                                    val16.__br_info,
                                    l55.info ],
                              __br_terms =
                                join
                                  [ [ l50.info ],
                                    val17.__br_terms,
                                    [ l51.info ],
                                    val15.__br_terms,
                                    [ l52.info ],
                                    [ l53.info ],
                                    [ l54.info ],
                                    val16.__br_terms,
                                    [ l55.info ] ],
                              id = val17.id,
                              thn = val15.thn,
                              els = val16.els,
                              cond = [ ntVal31.1 ] }) },
             { nt = #var"RtpplStmtAtom",
               label = {},
               rhs =
                 [ litSym "delay",
                   ntSym #var"RtpplExpr" ],
               action =
                 lam state52: {errors: Ref [(Info, [Char])], content: String}.
                   lam res52.
                     match
                       res52
                     with
                       [ LitParsed l56,
                         UserSym ntVal32 ]
                     in
                     let ntVal32: (Info, RtpplExpr) = fromDyn ntVal32 in
                       asDyn
                         (DelayRtpplStmtOp
                            { __br_info = mergeInfo l56.info ntVal32.0,
                              __br_terms = [ l56.info ],
                              ns = [ ntVal32.1 ] }) },
             { nt = alt9,
               label = {},
               rhs = "",
               action =
                 lam state53: {errors: Ref [(Info, [Char])], content: String}.
                   lam res53.
                     match
                       res53
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           upd = "" } },
             { nt = alt9,
               label = {},
               rhs =
                 [ litSym "update",
                   tokSym (LIdentRepr
                        {}) ],
               action =
                 lam state54: {errors: Ref [(Info, [Char])], content: String}.
                   lam res54.
                     match
                       res54
                     with
                       [ LitParsed l57,
                         TokParsed (LIdentTok x23) ]
                     in
                     asDyn
                         { __br_info = mergeInfo l57.info x23.info,
                           __br_terms = concat [ l57.info ] [ x23.info ],
                           upd = [ { v = nameNoSym x23.val, i = x23.info } ] } },
             { nt = kleene8,
               label = {},
               rhs =
                 [ ntSym #var"RtpplStmt",
                   ntSym kleene8 ],
               action =
                 lam state55: {errors: Ref [(Info, [Char])], content: String}.
                   lam res55.
                     match
                       res55
                     with
                       [ UserSym ntVal33,
                         UserSym val18 ]
                     in
                     let ntVal33: (Info, RtpplStmt) = fromDyn ntVal33 in
                       let val18: {body: [RtpplStmt], __br_info: Info, __br_terms: [Info]} = fromDyn val18
                       in
                       asDyn
                         { __br_info = mergeInfo ntVal33.0 val18.__br_info,
                           __br_terms = val18.__br_terms,
                           body = concat [ ntVal33.1 ] val18.body } },
             { nt = kleene8,
               label = {},
               rhs = "",
               action =
                 lam state56: {errors: Ref [(Info, [Char])], content: String}.
                   lam res56.
                     match
                       res56
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           body = "" } },
             { nt = #var"RtpplStmtAtom",
               label = {},
               rhs =
                 [ litSym "for",
                   tokSym (LIdentRepr
                        {}),
                   litSym "in",
                   ntSym #var"RtpplExpr",
                   ntSym alt9,
                   litSym "{",
                   ntSym kleene8,
                   litSym "}" ],
               action =
                 lam state57: {errors: Ref [(Info, [Char])], content: String}.
                   lam res57.
                     match
                       res57
                     with
                       [ LitParsed l58,
                         TokParsed (LIdentTok x24),
                         LitParsed l59,
                         UserSym ntVal34,
                         UserSym val19,
                         LitParsed l60,
                         UserSym val18,
                         LitParsed l61 ]
                     in
                     let ntVal34: (Info, RtpplExpr) = fromDyn ntVal34 in
                       let val19: {upd: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} = fromDyn val19
                       in
                       let val18: {body: [RtpplStmt], __br_info: Info, __br_terms: [Info]} = fromDyn val18
                       in
                       asDyn
                         (ForLoopRtpplStmtOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l58.info
                                  [ x24.info,
                                    l59.info,
                                    ntVal34.0,
                                    val19.__br_info,
                                    l60.info,
                                    val18.__br_info,
                                    l61.info ],
                              __br_terms =
                                join
                                  [ [ l58.info ],
                                    [ x24.info ],
                                    [ l59.info ],
                                    val19.__br_terms,
                                    [ l60.info ],
                                    val18.__br_terms,
                                    [ l61.info ] ],
                              e = [ ntVal34.1 ],
                              id = [ { v = nameSym x24.val, i = x24.info } ],
                              body = val18.body,
                              upd = val19.upd }) },
             { nt = alt10,
               label = {},
               rhs = "",
               action =
                 lam state58: {errors: Ref [(Info, [Char])], content: String}.
                   lam res58.
                     match
                       res58
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           upd = "" } },
             { nt = alt10,
               label = {},
               rhs =
                 [ litSym "update",
                   tokSym (LIdentRepr
                        {}) ],
               action =
                 lam state59: {errors: Ref [(Info, [Char])], content: String}.
                   lam res59.
                     match
                       res59
                     with
                       [ LitParsed l62,
                         TokParsed (LIdentTok x25) ]
                     in
                     asDyn
                         { __br_info = mergeInfo l62.info x25.info,
                           __br_terms = concat [ l62.info ] [ x25.info ],
                           upd = [ { v = nameNoSym x25.val, i = x25.info } ] } },
             { nt = kleene9,
               label = {},
               rhs =
                 [ ntSym #var"RtpplStmt",
                   ntSym kleene9 ],
               action =
                 lam state60: {errors: Ref [(Info, [Char])], content: String}.
                   lam res60.
                     match
                       res60
                     with
                       [ UserSym ntVal35,
                         UserSym val20 ]
                     in
                     let ntVal35: (Info, RtpplStmt) = fromDyn ntVal35 in
                       let val20: {body: [RtpplStmt], __br_info: Info, __br_terms: [Info]} = fromDyn val20
                       in
                       asDyn
                         { __br_info = mergeInfo ntVal35.0 val20.__br_info,
                           __br_terms = val20.__br_terms,
                           body = concat [ ntVal35.1 ] val20.body } },
             { nt = kleene9,
               label = {},
               rhs = "",
               action =
                 lam state61: {errors: Ref [(Info, [Char])], content: String}.
                   lam res61.
                     match
                       res61
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           body = "" } },
             { nt = #var"RtpplStmtAtom",
               label = {},
               rhs =
                 [ litSym "while",
                   ntSym #var"RtpplExpr",
                   ntSym alt10,
                   litSym "{",
                   ntSym kleene9,
                   litSym "}" ],
               action =
                 lam state62: {errors: Ref [(Info, [Char])], content: String}.
                   lam res62.
                     match
                       res62
                     with
                       [ LitParsed l63,
                         UserSym ntVal36,
                         UserSym val21,
                         LitParsed l64,
                         UserSym val20,
                         LitParsed l65 ]
                     in
                     let ntVal36: (Info, RtpplExpr) = fromDyn ntVal36 in
                       let val21: {upd: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} = fromDyn val21
                       in
                       let val20: {body: [RtpplStmt], __br_info: Info, __br_terms: [Info]} = fromDyn val20
                       in
                       asDyn
                         (WhileLoopRtpplStmtOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l63.info
                                  [ ntVal36.0,
                                    val21.__br_info,
                                    l64.info,
                                    val20.__br_info,
                                    l65.info ],
                              __br_terms =
                                join
                                  [ [ l63.info ],
                                    val21.__br_terms,
                                    [ l64.info ],
                                    val20.__br_terms,
                                    [ l65.info ] ],
                              body = val20.body,
                              cond = [ ntVal36.1 ],
                              upd = val21.upd }) },
             { nt = alt11,
               label = {},
               rhs = "",
               action =
                 lam state63: {errors: Ref [(Info, [Char])], content: String}.
                   lam res63.
                     match
                       res63
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           next = "" } },
             { nt = alt11,
               label = {},
               rhs = [ ntSym #var"RtpplStmtNoIdent" ],
               action =
                 lam state64: {errors: Ref [(Info, [Char])], content: String}.
                   lam res64.
                     match
                       res64
                     with
                       [ UserSym ntVal37 ]
                     in
                     let ntVal37: (Info, RtpplStmtNoIdent) = fromDyn ntVal37 in
                       asDyn
                         { __br_info = ntVal37.0, __br_terms = "", next = [ ntVal37.1 ] } },
             { nt = #var"RtpplStmtAtom",
               label = {},
               rhs =
                 [ tokSym (LIdentRepr
                        {}),
                   ntSym alt11 ],
               action =
                 lam state65: {errors: Ref [(Info, [Char])], content: String}.
                   lam res65.
                     match
                       res65
                     with
                       [ TokParsed (LIdentTok x26),
                         UserSym val22 ]
                     in
                     let val22: {next: [RtpplStmtNoIdent], __br_info: Info, __br_terms: [Info]} = fromDyn val22
                       in
                       asDyn
                         (IdentPlusStmtRtpplStmtOp
                            { __br_info = mergeInfo x26.info val22.__br_info,
                              __br_terms = concat [ x26.info ] val22.__br_terms,
                              id = [ { v = nameNoSym x26.val, i = x26.info } ],
                              next = val22.next }) },
             { nt = alt12,
               label = {},
               rhs = "",
               action =
                 lam state66: {errors: Ref [(Info, [Char])], content: String}.
                   lam res66.
                     match
                       res66
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           proj = "" } },
             { nt = alt12,
               label = {},
               rhs =
                 [ litSym ".",
                   tokSym (LIdentRepr
                        {}) ],
               action =
                 lam state67: {errors: Ref [(Info, [Char])], content: String}.
                   lam res67.
                     match
                       res67
                     with
                       [ LitParsed l66,
                         TokParsed (LIdentTok x27) ]
                     in
                     asDyn
                         { __br_info = mergeInfo l66.info x27.info,
                           __br_terms = concat [ l66.info ] [ x27.info ],
                           proj = [ { v = x27.val, i = x27.info } ] } },
             { nt = #var"RtpplStmtNoIdentAtom",
               label = {},
               rhs =
                 [ ntSym alt12,
                   litSym "=",
                   ntSym #var"RtpplExpr" ],
               action =
                 lam state68: {errors: Ref [(Info, [Char])], content: String}.
                   lam res68.
                     match
                       res68
                     with
                       [ UserSym val23,
                         LitParsed l67,
                         UserSym ntVal38 ]
                     in
                     let val23: {proj: [{i: Info, v: String}], __br_info: Info, __br_terms: [Info]} = fromDyn val23
                       in
                       let ntVal38: (Info, RtpplExpr) = fromDyn ntVal38 in
                       asDyn
                         (ReassignRtpplStmtNoIdentOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  val23.__br_info
                                  [ l67.info,
                                    ntVal38.0 ],
                              __br_terms = concat val23.__br_terms [ l67.info ],
                              e = [ ntVal38.1 ],
                              proj = val23.proj }) },
             { nt = kleene10,
               label = {},
               rhs =
                 [ litSym ",",
                   ntSym #var"RtpplExpr",
                   ntSym kleene10 ],
               action =
                 lam state69: {errors: Ref [(Info, [Char])], content: String}.
                   lam res69.
                     match
                       res69
                     with
                       [ LitParsed l68,
                         UserSym ntVal39,
                         UserSym val24 ]
                     in
                     let ntVal39: (Info, RtpplExpr) = fromDyn ntVal39 in
                       let val24: {args: [RtpplExpr], __br_info: Info, __br_terms: [Info]} = fromDyn val24
                       in
                       asDyn
                         { __br_info =
                             foldl
                               mergeInfo
                               l68.info
                               [ ntVal39.0,
                                 val24.__br_info ],
                           __br_terms = concat [ l68.info ] val24.__br_terms,
                           args = concat [ ntVal39.1 ] val24.args } },
             { nt = kleene10,
               label = {},
               rhs = "",
               action =
                 lam state70: {errors: Ref [(Info, [Char])], content: String}.
                   lam res70.
                     match
                       res70
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           args = "" } },
             { nt = alt13,
               label = {},
               rhs = "",
               action =
                 lam state71: {errors: Ref [(Info, [Char])], content: String}.
                   lam res71.
                     match
                       res71
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           args = "" } },
             { nt = alt13,
               label = {},
               rhs =
                 [ ntSym #var"RtpplExpr",
                   ntSym kleene10 ],
               action =
                 lam state72: {errors: Ref [(Info, [Char])], content: String}.
                   lam res72.
                     match
                       res72
                     with
                       [ UserSym ntVal40,
                         UserSym val24 ]
                     in
                     let ntVal40: (Info, RtpplExpr) = fromDyn ntVal40 in
                       let val24: {args: [RtpplExpr], __br_info: Info, __br_terms: [Info]} = fromDyn val24
                       in
                       asDyn
                         { __br_info = mergeInfo ntVal40.0 val24.__br_info,
                           __br_terms = val24.__br_terms,
                           args = concat [ ntVal40.1 ] val24.args } },
             { nt = #var"RtpplStmtNoIdentAtom",
               label = {},
               rhs =
                 [ litSym "(",
                   ntSym alt13,
                   litSym ")" ],
               action =
                 lam state73: {errors: Ref [(Info, [Char])], content: String}.
                   lam res73.
                     match
                       res73
                     with
                       [ LitParsed l69,
                         UserSym val25,
                         LitParsed l70 ]
                     in
                     let val25: {args: [RtpplExpr], __br_info: Info, __br_terms: [Info]} = fromDyn val25
                       in
                       asDyn
                         (FunctionCallSRtpplStmtNoIdentOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l69.info
                                  [ val25.__br_info,
                                    l70.info ],
                              __br_terms =
                                join
                                  [ [ l69.info ],
                                    val25.__br_terms,
                                    [ l70.info ] ],
                              args = val25.args }) },
             { nt = #var"RtpplExprAtom",
               label = {},
               rhs =
                 [ tokSym (LIdentRepr
                        {}),
                   ntSym #var"RtpplExprNoIdent" ],
               action =
                 lam state74: {errors: Ref [(Info, [Char])], content: String}.
                   lam res74.
                     match
                       res74
                     with
                       [ TokParsed (LIdentTok x28),
                         UserSym ntVal41 ]
                     in
                     let ntVal41: (Info, RtpplExprNoIdent) = fromDyn ntVal41 in
                       asDyn
                         (IdentPlusExprRtpplExprOp
                            { __br_info = mergeInfo x28.info ntVal41.0,
                              __br_terms = [ x28.info ],
                              id = [ { v = nameNoSym x28.val, i = x28.info } ],
                              next = [ ntVal41.1 ] }) },
             { nt = #var"RtpplExprNoIdentAtom",
               label = {},
               rhs = "",
               action =
                 lam state75: {errors: Ref [(Info, [Char])], content: String}.
                   lam res75.
                     match
                       res75
                     with
                       ""
                     in
                     asDyn
                         (VariableRtpplExprNoIdentOp
                            { __br_info = NoInfo
                                  {},
                              __br_terms = "" }) },
             { nt = kleene11,
               label = {},
               rhs =
                 [ litSym ",",
                   ntSym #var"RtpplExpr",
                   ntSym kleene11 ],
               action =
                 lam state76: {errors: Ref [(Info, [Char])], content: String}.
                   lam res76.
                     match
                       res76
                     with
                       [ LitParsed l71,
                         UserSym ntVal42,
                         UserSym val26 ]
                     in
                     let ntVal42: (Info, RtpplExpr) = fromDyn ntVal42 in
                       let val26: {args: [RtpplExpr], __br_info: Info, __br_terms: [Info]} = fromDyn val26
                       in
                       asDyn
                         { __br_info =
                             foldl
                               mergeInfo
                               l71.info
                               [ ntVal42.0,
                                 val26.__br_info ],
                           __br_terms = concat [ l71.info ] val26.__br_terms,
                           args = concat [ ntVal42.1 ] val26.args } },
             { nt = kleene11,
               label = {},
               rhs = "",
               action =
                 lam state77: {errors: Ref [(Info, [Char])], content: String}.
                   lam res77.
                     match
                       res77
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           args = "" } },
             { nt = alt14,
               label = {},
               rhs = "",
               action =
                 lam state78: {errors: Ref [(Info, [Char])], content: String}.
                   lam res78.
                     match
                       res78
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           args = "" } },
             { nt = alt14,
               label = {},
               rhs =
                 [ ntSym #var"RtpplExpr",
                   ntSym kleene11 ],
               action =
                 lam state79: {errors: Ref [(Info, [Char])], content: String}.
                   lam res79.
                     match
                       res79
                     with
                       [ UserSym ntVal43,
                         UserSym val26 ]
                     in
                     let ntVal43: (Info, RtpplExpr) = fromDyn ntVal43 in
                       let val26: {args: [RtpplExpr], __br_info: Info, __br_terms: [Info]} = fromDyn val26
                       in
                       asDyn
                         { __br_info = mergeInfo ntVal43.0 val26.__br_info,
                           __br_terms = val26.__br_terms,
                           args = concat [ ntVal43.1 ] val26.args } },
             { nt = #var"RtpplExprNoIdentAtom",
               label = {},
               rhs =
                 [ litSym "(",
                   ntSym alt14,
                   litSym ")" ],
               action =
                 lam state80: {errors: Ref [(Info, [Char])], content: String}.
                   lam res80.
                     match
                       res80
                     with
                       [ LitParsed l72,
                         UserSym val27,
                         LitParsed l73 ]
                     in
                     let val27: {args: [RtpplExpr], __br_info: Info, __br_terms: [Info]} = fromDyn val27
                       in
                       asDyn
                         (FunctionCallERtpplExprNoIdentOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l72.info
                                  [ val27.__br_info,
                                    l73.info ],
                              __br_terms =
                                join
                                  [ [ l72.info ],
                                    val27.__br_terms,
                                    [ l73.info ] ],
                              args = val27.args }) },
             { nt = #var"RtpplExprNoIdentAtom",
               label = {},
               rhs =
                 [ litSym ".",
                   tokSym (LIdentRepr
                        {}) ],
               action =
                 lam state81: {errors: Ref [(Info, [Char])], content: String}.
                   lam res81.
                     match
                       res81
                     with
                       [ LitParsed l74,
                         TokParsed (LIdentTok x29) ]
                     in
                     asDyn
                         (ProjectionRtpplExprNoIdentOp
                            { __br_info = mergeInfo l74.info x29.info,
                              __br_terms = concat [ l74.info ] [ x29.info ],
                              id = [ { v = x29.val, i = x29.info } ] }) },
             { nt = #var"RtpplExprPostfix",
               label = {},
               rhs =
                 [ litSym "[",
                   ntSym #var"RtpplExpr",
                   litSym "]" ],
               action =
                 lam state82: {errors: Ref [(Info, [Char])], content: String}.
                   lam res82.
                     match
                       res82
                     with
                       [ LitParsed l75,
                         UserSym ntVal44,
                         LitParsed l76 ]
                     in
                     let ntVal44: (Info, RtpplExpr) = fromDyn ntVal44 in
                       asDyn
                         (ArrayAccessRtpplExprOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l75.info
                                  [ ntVal44.0,
                                    l76.info ],
                              __br_terms = concat [ l75.info ] [ l76.info ],
                              idx = [ ntVal44.1 ] }) },
             { nt = kleene12,
               label = {},
               rhs =
                 [ litSym ",",
                   ntSym #var"RtpplExpr",
                   ntSym kleene12 ],
               action =
                 lam state83: {errors: Ref [(Info, [Char])], content: String}.
                   lam res83.
                     match
                       res83
                     with
                       [ LitParsed l77,
                         UserSym ntVal45,
                         UserSym val28 ]
                     in
                     let ntVal45: (Info, RtpplExpr) = fromDyn ntVal45 in
                       let val28: {elems: [RtpplExpr], __br_info: Info, __br_terms: [Info]} = fromDyn val28
                       in
                       asDyn
                         { __br_info =
                             foldl
                               mergeInfo
                               l77.info
                               [ ntVal45.0,
                                 val28.__br_info ],
                           __br_terms = concat [ l77.info ] val28.__br_terms,
                           elems = concat [ ntVal45.1 ] val28.elems } },
             { nt = kleene12,
               label = {},
               rhs = "",
               action =
                 lam state84: {errors: Ref [(Info, [Char])], content: String}.
                   lam res84.
                     match
                       res84
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           elems = "" } },
             { nt = alt15,
               label = {},
               rhs = "",
               action =
                 lam state85: {errors: Ref [(Info, [Char])], content: String}.
                   lam res85.
                     match
                       res85
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           elems = "" } },
             { nt = alt15,
               label = {},
               rhs =
                 [ ntSym #var"RtpplExpr",
                   ntSym kleene12 ],
               action =
                 lam state86: {errors: Ref [(Info, [Char])], content: String}.
                   lam res86.
                     match
                       res86
                     with
                       [ UserSym ntVal46,
                         UserSym val28 ]
                     in
                     let ntVal46: (Info, RtpplExpr) = fromDyn ntVal46 in
                       let val28: {elems: [RtpplExpr], __br_info: Info, __br_terms: [Info]} = fromDyn val28
                       in
                       asDyn
                         { __br_info = mergeInfo ntVal46.0 val28.__br_info,
                           __br_terms = val28.__br_terms,
                           elems = concat [ ntVal46.1 ] val28.elems } },
             { nt = #var"RtpplExprAtom",
               label = {},
               rhs =
                 [ litSym "[",
                   ntSym alt15,
                   litSym "]" ],
               action =
                 lam state87: {errors: Ref [(Info, [Char])], content: String}.
                   lam res87.
                     match
                       res87
                     with
                       [ LitParsed l78,
                         UserSym val29,
                         LitParsed l79 ]
                     in
                     let val29: {elems: [RtpplExpr], __br_info: Info, __br_terms: [Info]} = fromDyn val29
                       in
                       asDyn
                         (ArrayLitRtpplExprOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l78.info
                                  [ val29.__br_info,
                                    l79.info ],
                              __br_terms =
                                join
                                  [ [ l78.info ],
                                    val29.__br_terms,
                                    [ l79.info ] ],
                              elems = val29.elems }) },
             { nt = kleene13,
               label = {},
               rhs =
                 [ litSym ",",
                   tokSym (LIdentRepr
                        {}),
                   litSym "=",
                   ntSym #var"RtpplExpr",
                   ntSym kleene13 ],
               action =
                 lam state88: {errors: Ref [(Info, [Char])], content: String}.
                   lam res88.
                     match
                       res88
                     with
                       [ LitParsed l80,
                         TokParsed (LIdentTok x30),
                         LitParsed l81,
                         UserSym ntVal47,
                         UserSym val30 ]
                     in
                     let ntVal47: (Info, RtpplExpr) = fromDyn ntVal47 in
                       let val30: {fields: [{e: RtpplExpr, id: {i: Info, v: String}}], __br_info: Info, __br_terms: [Info]} = fromDyn val30
                       in
                       asDyn
                         { __br_info =
                             foldl
                               mergeInfo
                               l80.info
                               [ x30.info,
                                 l81.info,
                                 ntVal47.0,
                                 val30.__br_info ],
                           __br_terms =
                             join
                               [ [ l80.info ],
                                 [ x30.info ],
                                 [ l81.info ],
                                 val30.__br_terms ],
                           fields =
                             concat
                               [ { e =
                                     match
                                       [ ntVal47.1 ]
                                     with
                                       [ x31 ] ++ _
                                     in
                                     x31,
                                   id =
                                     match
                                       [ { v = x30.val, i = x30.info } ]
                                     with
                                       [ x32 ] ++ _
                                     in
                                     x32 } ]
                               val30.fields } },
             { nt = kleene13,
               label = {},
               rhs = "",
               action =
                 lam state89: {errors: Ref [(Info, [Char])], content: String}.
                   lam res89.
                     match
                       res89
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           fields = "" } },
             { nt = alt16,
               label = {},
               rhs = "",
               action =
                 lam state90: {errors: Ref [(Info, [Char])], content: String}.
                   lam res90.
                     match
                       res90
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           fields = "" } },
             { nt = alt16,
               label = {},
               rhs =
                 [ tokSym (LIdentRepr
                        {}),
                   litSym "=",
                   ntSym #var"RtpplExpr",
                   ntSym kleene13 ],
               action =
                 lam state91: {errors: Ref [(Info, [Char])], content: String}.
                   lam res91.
                     match
                       res91
                     with
                       [ TokParsed (LIdentTok x33),
                         LitParsed l82,
                         UserSym ntVal48,
                         UserSym val30 ]
                     in
                     let ntVal48: (Info, RtpplExpr) = fromDyn ntVal48 in
                       let val30: {fields: [{e: RtpplExpr, id: {i: Info, v: String}}], __br_info: Info, __br_terms: [Info]} = fromDyn val30
                       in
                       asDyn
                         { __br_info =
                             foldl
                               mergeInfo
                               x33.info
                               [ l82.info,
                                 ntVal48.0,
                                 val30.__br_info ],
                           __br_terms =
                             join
                               [ [ x33.info ],
                                 [ l82.info ],
                                 val30.__br_terms ],
                           fields =
                             concat
                               [ { e =
                                     match
                                       [ ntVal48.1 ]
                                     with
                                       [ x34 ] ++ _
                                     in
                                     x34,
                                   id =
                                     match
                                       [ { v = x33.val, i = x33.info } ]
                                     with
                                       [ x35 ] ++ _
                                     in
                                     x35 } ]
                               val30.fields } },
             { nt = #var"RtpplExprAtom",
               label = {},
               rhs =
                 [ litSym "{",
                   ntSym alt16,
                   litSym "}" ],
               action =
                 lam state92: {errors: Ref [(Info, [Char])], content: String}.
                   lam res92.
                     match
                       res92
                     with
                       [ LitParsed l83,
                         UserSym val31,
                         LitParsed l84 ]
                     in
                     let val31: {fields: [{e: RtpplExpr, id: {i: Info, v: String}}], __br_info: Info, __br_terms: [Info]} = fromDyn val31
                       in
                       asDyn
                         (RecordLitRtpplExprOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l83.info
                                  [ val31.__br_info,
                                    l84.info ],
                              __br_terms =
                                join
                                  [ [ l83.info ],
                                    val31.__br_terms,
                                    [ l84.info ] ],
                              fields = val31.fields }) },
             { nt = #var"RtpplExprAtom",
               label = {},
               rhs = [ ntSym #var"RtpplConst" ],
               action =
                 lam state93: {errors: Ref [(Info, [Char])], content: String}.
                   lam res93.
                     match
                       res93
                     with
                       [ UserSym ntVal49 ]
                     in
                     let ntVal49: (Info, RtpplConst) = fromDyn ntVal49 in
                       asDyn
                         (LiteralRtpplExprOp
                            { __br_info = ntVal49.0, __br_terms = "", const = [ ntVal49.1 ] }) },
             { nt = #var"RtpplExprAtom",
               label = {},
               rhs =
                 [ litSym "|",
                   ntSym #var"RtpplExpr",
                   litSym "|" ],
               action =
                 lam state94: {errors: Ref [(Info, [Char])], content: String}.
                   lam res94.
                     match
                       res94
                     with
                       [ LitParsed l85,
                         UserSym ntVal50,
                         LitParsed l86 ]
                     in
                     let ntVal50: (Info, RtpplExpr) = fromDyn ntVal50 in
                       asDyn
                         (LengthRtpplExprOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l85.info
                                  [ ntVal50.0,
                                    l86.info ],
                              __br_terms = concat [ l85.info ] [ l86.info ],
                              e = [ ntVal50.1 ] }) },
             { nt = #var"RtpplExprPrefix",
               label = {},
               rhs = [ litSym "samples" ],
               action =
                 lam state95: {errors: Ref [(Info, [Char])], content: String}.
                   lam res95.
                     match
                       res95
                     with
                       [ LitParsed l87 ]
                     in
                     asDyn
                         (DistSamplesRtpplExprOp
                            { __br_info = l87.info, __br_terms = [ l87.info ] }) },
             { nt = #var"RtpplExprAtom",
               label = {},
               rhs =
                 [ litSym "Gaussian",
                   litSym "(",
                   ntSym #var"RtpplExpr",
                   litSym ",",
                   ntSym #var"RtpplExpr",
                   litSym ")" ],
               action =
                 lam state96: {errors: Ref [(Info, [Char])], content: String}.
                   lam res96.
                     match
                       res96
                     with
                       [ LitParsed l88,
                         LitParsed l89,
                         UserSym ntVal51,
                         LitParsed l90,
                         UserSym ntVal52,
                         LitParsed l91 ]
                     in
                     let ntVal51: (Info, RtpplExpr) = fromDyn ntVal51 in
                       let ntVal52: (Info, RtpplExpr) = fromDyn ntVal52 in
                       asDyn
                         (GaussianDistRtpplExprOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l88.info
                                  [ l89.info,
                                    ntVal51.0,
                                    l90.info,
                                    ntVal52.0,
                                    l91.info ],
                              __br_terms =
                                join
                                  [ [ l88.info ],
                                    [ l89.info ],
                                    [ l90.info ],
                                    [ l91.info ] ],
                              mu = [ ntVal51.1 ],
                              sigma = [ ntVal52.1 ] }) },
             { nt = #var"RtpplExprAtom",
               label = {},
               rhs =
                 [ litSym "Uniform",
                   litSym "(",
                   ntSym #var"RtpplExpr",
                   litSym ",",
                   ntSym #var"RtpplExpr",
                   litSym ")" ],
               action =
                 lam state97: {errors: Ref [(Info, [Char])], content: String}.
                   lam res97.
                     match
                       res97
                     with
                       [ LitParsed l92,
                         LitParsed l93,
                         UserSym ntVal53,
                         LitParsed l94,
                         UserSym ntVal54,
                         LitParsed l95 ]
                     in
                     let ntVal53: (Info, RtpplExpr) = fromDyn ntVal53 in
                       let ntVal54: (Info, RtpplExpr) = fromDyn ntVal54 in
                       asDyn
                         (UniformDistRtpplExprOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l92.info
                                  [ l93.info,
                                    ntVal53.0,
                                    l94.info,
                                    ntVal54.0,
                                    l95.info ],
                              __br_terms =
                                join
                                  [ [ l92.info ],
                                    [ l93.info ],
                                    [ l94.info ],
                                    [ l95.info ] ],
                              hi = [ ntVal54.1 ],
                              lo = [ ntVal53.1 ] }) },
             { nt = #var"RtpplExprAtom",
               label = {},
               rhs =
                 [ litSym "Bernoulli",
                   litSym "(",
                   ntSym #var"RtpplExpr",
                   litSym ")" ],
               action =
                 lam state98: {errors: Ref [(Info, [Char])], content: String}.
                   lam res98.
                     match
                       res98
                     with
                       [ LitParsed l96,
                         LitParsed l97,
                         UserSym ntVal55,
                         LitParsed l98 ]
                     in
                     let ntVal55: (Info, RtpplExpr) = fromDyn ntVal55 in
                       asDyn
                         (BernoulliDistRtpplExprOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l96.info
                                  [ l97.info,
                                    ntVal55.0,
                                    l98.info ],
                              __br_terms =
                                join
                                  [ [ l96.info ],
                                    [ l97.info ],
                                    [ l98.info ] ],
                              p = [ ntVal55.1 ] }) },
             { nt = #var"RtpplExprAtom",
               label = {},
               rhs =
                 [ litSym "Gamma",
                   litSym "(",
                   ntSym #var"RtpplExpr",
                   litSym ",",
                   ntSym #var"RtpplExpr",
                   litSym ")" ],
               action =
                 lam state99: {errors: Ref [(Info, [Char])], content: String}.
                   lam res99.
                     match
                       res99
                     with
                       [ LitParsed l99,
                         LitParsed l100,
                         UserSym ntVal56,
                         LitParsed l101,
                         UserSym ntVal57,
                         LitParsed l102 ]
                     in
                     let ntVal56: (Info, RtpplExpr) = fromDyn ntVal56 in
                       let ntVal57: (Info, RtpplExpr) = fromDyn ntVal57 in
                       asDyn
                         (GammaDistRtpplExprOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l99.info
                                  [ l100.info,
                                    ntVal56.0,
                                    l101.info,
                                    ntVal57.0,
                                    l102.info ],
                              __br_terms =
                                join
                                  [ [ l99.info ],
                                    [ l100.info ],
                                    [ l101.info ],
                                    [ l102.info ] ],
                              k = [ ntVal56.1 ],
                              theta = [ ntVal57.1 ] }) },
             { nt = #var"RtpplExprInfix",
               label = {},
               rhs = [ litSym "+" ],
               action =
                 lam state100: {errors: Ref [(Info, [Char])], content: String}.
                   lam res100.
                     match
                       res100
                     with
                       [ LitParsed l103 ]
                     in
                     asDyn
                         (AddRtpplExprOp
                            { __br_info = l103.info, __br_terms = [ l103.info ] }) },
             { nt = #var"RtpplExprInfix",
               label = {},
               rhs = [ litSym "-" ],
               action =
                 lam state101: {errors: Ref [(Info, [Char])], content: String}.
                   lam res101.
                     match
                       res101
                     with
                       [ LitParsed l104 ]
                     in
                     asDyn
                         (SubRtpplExprOp
                            { __br_info = l104.info, __br_terms = [ l104.info ] }) },
             { nt = #var"RtpplExprInfix",
               label = {},
               rhs = [ litSym "*" ],
               action =
                 lam state102: {errors: Ref [(Info, [Char])], content: String}.
                   lam res102.
                     match
                       res102
                     with
                       [ LitParsed l105 ]
                     in
                     asDyn
                         (MulRtpplExprOp
                            { __br_info = l105.info, __br_terms = [ l105.info ] }) },
             { nt = #var"RtpplExprInfix",
               label = {},
               rhs = [ litSym "/" ],
               action =
                 lam state103: {errors: Ref [(Info, [Char])], content: String}.
                   lam res103.
                     match
                       res103
                     with
                       [ LitParsed l106 ]
                     in
                     asDyn
                         (DivRtpplExprOp
                            { __br_info = l106.info, __br_terms = [ l106.info ] }) },
             { nt = #var"RtpplExprInfix",
               label = {},
               rhs = [ litSym "==" ],
               action =
                 lam state104: {errors: Ref [(Info, [Char])], content: String}.
                   lam res104.
                     match
                       res104
                     with
                       [ LitParsed l107 ]
                     in
                     asDyn
                         (EqRtpplExprOp
                            { __br_info = l107.info, __br_terms = [ l107.info ] }) },
             { nt = #var"RtpplExprInfix",
               label = {},
               rhs = [ litSym "!=" ],
               action =
                 lam state105: {errors: Ref [(Info, [Char])], content: String}.
                   lam res105.
                     match
                       res105
                     with
                       [ LitParsed l108 ]
                     in
                     asDyn
                         (NeqRtpplExprOp
                            { __br_info = l108.info, __br_terms = [ l108.info ] }) },
             { nt = #var"RtpplExprInfix",
               label = {},
               rhs = [ litSym "<" ],
               action =
                 lam state106: {errors: Ref [(Info, [Char])], content: String}.
                   lam res106.
                     match
                       res106
                     with
                       [ LitParsed l109 ]
                     in
                     asDyn
                         (LtRtpplExprOp
                            { __br_info = l109.info, __br_terms = [ l109.info ] }) },
             { nt = #var"RtpplExprInfix",
               label = {},
               rhs = [ litSym ">" ],
               action =
                 lam state107: {errors: Ref [(Info, [Char])], content: String}.
                   lam res107.
                     match
                       res107
                     with
                       [ LitParsed l110 ]
                     in
                     asDyn
                         (GtRtpplExprOp
                            { __br_info = l110.info, __br_terms = [ l110.info ] }) },
             { nt = #var"RtpplExprInfix",
               label = {},
               rhs = [ litSym "<=" ],
               action =
                 lam state108: {errors: Ref [(Info, [Char])], content: String}.
                   lam res108.
                     match
                       res108
                     with
                       [ LitParsed l111 ]
                     in
                     asDyn
                         (LeqRtpplExprOp
                            { __br_info = l111.info, __br_terms = [ l111.info ] }) },
             { nt = #var"RtpplExprInfix",
               label = {},
               rhs = [ litSym ">=" ],
               action =
                 lam state109: {errors: Ref [(Info, [Char])], content: String}.
                   lam res109.
                     match
                       res109
                     with
                       [ LitParsed l112 ]
                     in
                     asDyn
                         (GeqRtpplExprOp
                            { __br_info = l112.info, __br_terms = [ l112.info ] }) },
             { nt = #var"RtpplExprInfix",
               label = {},
               rhs = [ litSym "&&" ],
               action =
                 lam state110: {errors: Ref [(Info, [Char])], content: String}.
                   lam res110.
                     match
                       res110
                     with
                       [ LitParsed l113 ]
                     in
                     asDyn
                         (AndRtpplExprOp
                            { __br_info = l113.info, __br_terms = [ l113.info ] }) },
             { nt = #var"RtpplExprInfix",
               label = {},
               rhs = [ litSym "||" ],
               action =
                 lam state111: {errors: Ref [(Info, [Char])], content: String}.
                   lam res111.
                     match
                       res111
                     with
                       [ LitParsed l114 ]
                     in
                     asDyn
                         (OrRtpplExprOp
                            { __br_info = l114.info, __br_terms = [ l114.info ] }) },
             { nt = kleene14,
               label = {},
               rhs =
                 [ ntSym #var"RtpplExt",
                   ntSym kleene14 ],
               action =
                 lam state112: {errors: Ref [(Info, [Char])], content: String}.
                   lam res112.
                     match
                       res112
                     with
                       [ UserSym ntVal58,
                         UserSym val32 ]
                     in
                     let ntVal58: (Info, RtpplExt) = fromDyn ntVal58 in
                       let val32: {ext: [RtpplExt], __br_info: Info, __br_terms: [Info]} = fromDyn val32
                       in
                       asDyn
                         { __br_info = mergeInfo ntVal58.0 val32.__br_info,
                           __br_terms = val32.__br_terms,
                           ext = concat [ ntVal58.1 ] val32.ext } },
             { nt = kleene14,
               label = {},
               rhs = "",
               action =
                 lam state113: {errors: Ref [(Info, [Char])], content: String}.
                   lam res113.
                     match
                       res113
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           ext = "" } },
             { nt = kleene15,
               label = {},
               rhs =
                 [ ntSym #var"RtpplTask",
                   ntSym kleene15 ],
               action =
                 lam state114: {errors: Ref [(Info, [Char])], content: String}.
                   lam res114.
                     match
                       res114
                     with
                       [ UserSym ntVal59,
                         UserSym val33 ]
                     in
                     let ntVal59: (Info, RtpplTask) = fromDyn ntVal59 in
                       let val33: {tasks: [RtpplTask], __br_info: Info, __br_terms: [Info]} = fromDyn val33
                       in
                       asDyn
                         { __br_info = mergeInfo ntVal59.0 val33.__br_info,
                           __br_terms = val33.__br_terms,
                           tasks = concat [ ntVal59.1 ] val33.tasks } },
             { nt = kleene15,
               label = {},
               rhs = "",
               action =
                 lam state115: {errors: Ref [(Info, [Char])], content: String}.
                   lam res115.
                     match
                       res115
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           tasks = "" } },
             { nt = kleene16,
               label = {},
               rhs =
                 [ ntSym #var"RtpplConnection",
                   ntSym kleene16 ],
               action =
                 lam state116: {errors: Ref [(Info, [Char])], content: String}.
                   lam res116.
                     match
                       res116
                     with
                       [ UserSym ntVal60,
                         UserSym val34 ]
                     in
                     let ntVal60: (Info, RtpplConnection) = fromDyn ntVal60 in
                       let val34: {__br_info: Info, __br_terms: [Info], connections: [RtpplConnection]} = fromDyn val34
                       in
                       asDyn
                         { __br_info = mergeInfo ntVal60.0 val34.__br_info,
                           __br_terms = val34.__br_terms,
                           connections = concat [ ntVal60.1 ] val34.connections } },
             { nt = kleene16,
               label = {},
               rhs = "",
               action =
                 lam state117: {errors: Ref [(Info, [Char])], content: String}.
                   lam res117.
                     match
                       res117
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           connections = "" } },
             { nt = #var"RtpplMainAtom",
               label = {},
               rhs =
                 [ litSym "system",
                   litSym "{",
                   ntSym kleene14,
                   ntSym kleene15,
                   ntSym kleene16,
                   litSym "}" ],
               action =
                 lam state118: {errors: Ref [(Info, [Char])], content: String}.
                   lam res118.
                     match
                       res118
                     with
                       [ LitParsed l115,
                         LitParsed l116,
                         UserSym val32,
                         UserSym val33,
                         UserSym val34,
                         LitParsed l117 ]
                     in
                     let val32: {ext: [RtpplExt], __br_info: Info, __br_terms: [Info]} = fromDyn val32
                       in
                       let val33: {tasks: [RtpplTask], __br_info: Info, __br_terms: [Info]} = fromDyn val33
                       in
                       let val34: {__br_info: Info, __br_terms: [Info], connections: [RtpplConnection]} = fromDyn val34
                       in
                       asDyn
                         (MainRtpplMainOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l115.info
                                  [ l116.info,
                                    val32.__br_info,
                                    val33.__br_info,
                                    val34.__br_info,
                                    l117.info ],
                              __br_terms =
                                join
                                  [ [ l115.info ],
                                    [ l116.info ],
                                    val32.__br_terms,
                                    val33.__br_terms,
                                    val34.__br_terms,
                                    [ l117.info ] ],
                              ext = val32.ext,
                              tasks = val33.tasks,
                              connections = val34.connections }) },
             { nt = #var"RtpplExtAtom",
               label = {},
               rhs =
                 [ litSym "sensor",
                   tokSym (LIdentRepr
                        {}),
                   litSym ":",
                   ntSym #var"RtpplType",
                   litSym "rate",
                   ntSym #var"RtpplExpr" ],
               action =
                 lam state119: {errors: Ref [(Info, [Char])], content: String}.
                   lam res119.
                     match
                       res119
                     with
                       [ LitParsed l118,
                         TokParsed (LIdentTok x36),
                         LitParsed l119,
                         UserSym ntVal61,
                         LitParsed l120,
                         UserSym ntVal62 ]
                     in
                     let ntVal61: (Info, RtpplType) = fromDyn ntVal61 in
                       let ntVal62: (Info, RtpplExpr) = fromDyn ntVal62 in
                       asDyn
                         (SensorRtpplExtOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l118.info
                                  [ x36.info,
                                    l119.info,
                                    ntVal61.0,
                                    l120.info,
                                    ntVal62.0 ],
                              __br_terms =
                                join
                                  [ [ l118.info ],
                                    [ x36.info ],
                                    [ l119.info ],
                                    [ l120.info ] ],
                              id = [ { v = nameNoSym x36.val, i = x36.info } ],
                              ty = [ ntVal61.1 ],
                              r = [ ntVal62.1 ] }) },
             { nt = #var"RtpplExtAtom",
               label = {},
               rhs =
                 [ litSym "actuator",
                   tokSym (LIdentRepr
                        {}),
                   litSym ":",
                   ntSym #var"RtpplType",
                   litSym "rate",
                   ntSym #var"RtpplExpr" ],
               action =
                 lam state120: {errors: Ref [(Info, [Char])], content: String}.
                   lam res120.
                     match
                       res120
                     with
                       [ LitParsed l121,
                         TokParsed (LIdentTok x37),
                         LitParsed l122,
                         UserSym ntVal63,
                         LitParsed l123,
                         UserSym ntVal64 ]
                     in
                     let ntVal63: (Info, RtpplType) = fromDyn ntVal63 in
                       let ntVal64: (Info, RtpplExpr) = fromDyn ntVal64 in
                       asDyn
                         (ActuatorRtpplExtOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l121.info
                                  [ x37.info,
                                    l122.info,
                                    ntVal63.0,
                                    l123.info,
                                    ntVal64.0 ],
                              __br_terms =
                                join
                                  [ [ l121.info ],
                                    [ x37.info ],
                                    [ l122.info ],
                                    [ l123.info ] ],
                              id = [ { v = nameNoSym x37.val, i = x37.info } ],
                              ty = [ ntVal63.1 ],
                              r = [ ntVal64.1 ] }) },
             { nt = kleene17,
               label = {},
               rhs =
                 [ litSym ",",
                   ntSym #var"RtpplExpr",
                   ntSym kleene17 ],
               action =
                 lam state121: {errors: Ref [(Info, [Char])], content: String}.
                   lam res121.
                     match
                       res121
                     with
                       [ LitParsed l124,
                         UserSym ntVal65,
                         UserSym val35 ]
                     in
                     let ntVal65: (Info, RtpplExpr) = fromDyn ntVal65 in
                       let val35: {args: [RtpplExpr], __br_info: Info, __br_terms: [Info]} = fromDyn val35
                       in
                       asDyn
                         { __br_info =
                             foldl
                               mergeInfo
                               l124.info
                               [ ntVal65.0,
                                 val35.__br_info ],
                           __br_terms = concat [ l124.info ] val35.__br_terms,
                           args = concat [ ntVal65.1 ] val35.args } },
             { nt = kleene17,
               label = {},
               rhs = "",
               action =
                 lam state122: {errors: Ref [(Info, [Char])], content: String}.
                   lam res122.
                     match
                       res122
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           args = "" } },
             { nt = alt17,
               label = {},
               rhs = "",
               action =
                 lam state123: {errors: Ref [(Info, [Char])], content: String}.
                   lam res123.
                     match
                       res123
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           args = "" } },
             { nt = alt17,
               label = {},
               rhs =
                 [ ntSym #var"RtpplExpr",
                   ntSym kleene17 ],
               action =
                 lam state124: {errors: Ref [(Info, [Char])], content: String}.
                   lam res124.
                     match
                       res124
                     with
                       [ UserSym ntVal66,
                         UserSym val35 ]
                     in
                     let ntVal66: (Info, RtpplExpr) = fromDyn ntVal66 in
                       let val35: {args: [RtpplExpr], __br_info: Info, __br_terms: [Info]} = fromDyn val35
                       in
                       asDyn
                         { __br_info = mergeInfo ntVal66.0 val35.__br_info,
                           __br_terms = val35.__br_terms,
                           args = concat [ ntVal66.1 ] val35.args } },
             { nt = #var"RtpplTaskAtom",
               label = {},
               rhs =
                 [ litSym "task",
                   tokSym (LIdentRepr
                        {}),
                   litSym "=",
                   tokSym (UIdentRepr
                        {}),
                   litSym "(",
                   ntSym alt17,
                   litSym ")",
                   litSym "importance",
                   tokSym (IntRepr
                        {}) ],
               action =
                 lam state125: {errors: Ref [(Info, [Char])], content: String}.
                   lam res125.
                     match
                       res125
                     with
                       [ LitParsed l125,
                         TokParsed (LIdentTok x38),
                         LitParsed l126,
                         TokParsed (UIdentTok x39),
                         LitParsed l127,
                         UserSym val36,
                         LitParsed l128,
                         LitParsed l129,
                         TokParsed (IntTok x40) ]
                     in
                     let val36: {args: [RtpplExpr], __br_info: Info, __br_terms: [Info]} = fromDyn val36
                       in
                       asDyn
                         (TaskRtpplTaskOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l125.info
                                  [ x38.info,
                                    l126.info,
                                    x39.info,
                                    l127.info,
                                    val36.__br_info,
                                    l128.info,
                                    l129.info,
                                    x40.info ],
                              __br_terms =
                                join
                                  [ [ l125.info ],
                                    [ x38.info ],
                                    [ l126.info ],
                                    [ x39.info ],
                                    [ l127.info ],
                                    val36.__br_terms,
                                    [ l128.info ],
                                    [ l129.info ],
                                    [ x40.info ] ],
                              id = [ { v = nameNoSym x38.val, i = x38.info } ],
                              p = [ { v = x40.val, i = x40.info } ],
                              args = val36.args,
                              templateId = [ { v = nameNoSym x39.val, i = x39.info } ] }) },
             { nt = #var"RtpplConnectionAtom",
               label = {},
               rhs =
                 [ ntSym #var"RtpplPortSpec",
                   litSym "->",
                   ntSym #var"RtpplPortSpec" ],
               action =
                 lam state126: {errors: Ref [(Info, [Char])], content: String}.
                   lam res126.
                     match
                       res126
                     with
                       [ UserSym ntVal67,
                         LitParsed l130,
                         UserSym ntVal68 ]
                     in
                     let ntVal67: (Info, RtpplPortSpec) = fromDyn ntVal67 in
                       let ntVal68: (Info, RtpplPortSpec) = fromDyn ntVal68 in
                       asDyn
                         (ConnectionRtpplConnectionOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  ntVal67.0
                                  [ l130.info,
                                    ntVal68.0 ],
                              __br_terms = [ l130.info ],
                              to = [ ntVal68.1 ],
                              from = [ ntVal67.1 ] }) },
             { nt = alt18,
               label = {},
               rhs = "",
               action =
                 lam state127: {errors: Ref [(Info, [Char])], content: String}.
                   lam res127.
                     match
                       res127
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           id = "" } },
             { nt = alt18,
               label = {},
               rhs =
                 [ litSym ".",
                   tokSym (LIdentRepr
                        {}) ],
               action =
                 lam state128: {errors: Ref [(Info, [Char])], content: String}.
                   lam res128.
                     match
                       res128
                     with
                       [ LitParsed l131,
                         TokParsed (LIdentTok x41) ]
                     in
                     asDyn
                         { __br_info = mergeInfo l131.info x41.info,
                           __br_terms = concat [ l131.info ] [ x41.info ],
                           id = [ { v = x41.val, i = x41.info } ] } },
             { nt = #var"RtpplPortSpecAtom",
               label = {},
               rhs =
                 [ tokSym (LIdentRepr
                        {}),
                   ntSym alt18 ],
               action =
                 lam state129: {errors: Ref [(Info, [Char])], content: String}.
                   lam res129.
                     match
                       res129
                     with
                       [ TokParsed (LIdentTok x42),
                         UserSym val37 ]
                     in
                     let val37: {id: [{i: Info, v: String}], __br_info: Info, __br_terms: [Info]} = fromDyn val37
                       in
                       asDyn
                         (PortSpecRtpplPortSpecOp
                            { __br_info = mergeInfo x42.info val37.__br_info,
                              __br_terms = concat [ x42.info ] val37.__br_terms,
                              id = val37.id,
                              port = [ { v = nameNoSym x42.val, i = x42.info } ] }) },
             { nt = #var"RtpplTypeAtom",
               label = {},
               rhs = [ litSym "Int" ],
               action =
                 lam state130: {errors: Ref [(Info, [Char])], content: String}.
                   lam res130.
                     match
                       res130
                     with
                       [ LitParsed l132 ]
                     in
                     asDyn
                         (IntRtpplTypeOp
                            { __br_info = l132.info, __br_terms = [ l132.info ] }) },
             { nt = #var"RtpplTypeAtom",
               label = {},
               rhs = [ litSym "Float" ],
               action =
                 lam state131: {errors: Ref [(Info, [Char])], content: String}.
                   lam res131.
                     match
                       res131
                     with
                       [ LitParsed l133 ]
                     in
                     asDyn
                         (FloatRtpplTypeOp
                            { __br_info = l133.info, __br_terms = [ l133.info ] }) },
             { nt = #var"RtpplTypeAtom",
               label = {},
               rhs = [ litSym "Bool" ],
               action =
                 lam state132: {errors: Ref [(Info, [Char])], content: String}.
                   lam res132.
                     match
                       res132
                     with
                       [ LitParsed l134 ]
                     in
                     asDyn
                         (BoolRtpplTypeOp
                            { __br_info = l134.info, __br_terms = [ l134.info ] }) },
             { nt = #var"RtpplTypeAtom",
               label = {},
               rhs = [ litSym "String" ],
               action =
                 lam state133: {errors: Ref [(Info, [Char])], content: String}.
                   lam res133.
                     match
                       res133
                     with
                       [ LitParsed l135 ]
                     in
                     asDyn
                         (StringRtpplTypeOp
                            { __br_info = l135.info, __br_terms = [ l135.info ] }) },
             { nt = #var"RtpplTypeAtom",
               label = {},
               rhs = [ litSym "Unit" ],
               action =
                 lam state134: {errors: Ref [(Info, [Char])], content: String}.
                   lam res134.
                     match
                       res134
                     with
                       [ LitParsed l136 ]
                     in
                     asDyn
                         (UnitRtpplTypeOp
                            { __br_info = l136.info, __br_terms = [ l136.info ] }) },
             { nt = #var"RtpplTypeAtom",
               label = {},
               rhs =
                 [ litSym "[",
                   ntSym #var"RtpplType",
                   litSym "]" ],
               action =
                 lam state135: {errors: Ref [(Info, [Char])], content: String}.
                   lam res135.
                     match
                       res135
                     with
                       [ LitParsed l137,
                         UserSym ntVal69,
                         LitParsed l138 ]
                     in
                     let ntVal69: (Info, RtpplType) = fromDyn ntVal69 in
                       asDyn
                         (SeqRtpplTypeOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l137.info
                                  [ ntVal69.0,
                                    l138.info ],
                              __br_terms = concat [ l137.info ] [ l138.info ],
                              ty = [ ntVal69.1 ] }) },
             { nt = kleene18,
               label = {},
               rhs =
                 [ litSym ",",
                   tokSym (LIdentRepr
                        {}),
                   litSym ":",
                   ntSym #var"RtpplType",
                   ntSym kleene18 ],
               action =
                 lam state136: {errors: Ref [(Info, [Char])], content: String}.
                   lam res136.
                     match
                       res136
                     with
                       [ LitParsed l139,
                         TokParsed (LIdentTok x43),
                         LitParsed l140,
                         UserSym ntVal70,
                         UserSym val38 ]
                     in
                     let ntVal70: (Info, RtpplType) = fromDyn ntVal70 in
                       let val38: {fields: [{id: {i: Info, v: String}, ty: RtpplType}], __br_info: Info, __br_terms: [Info]} = fromDyn val38
                       in
                       asDyn
                         { __br_info =
                             foldl
                               mergeInfo
                               l139.info
                               [ x43.info,
                                 l140.info,
                                 ntVal70.0,
                                 val38.__br_info ],
                           __br_terms =
                             join
                               [ [ l139.info ],
                                 [ x43.info ],
                                 [ l140.info ],
                                 val38.__br_terms ],
                           fields =
                             concat
                               [ { id =
                                     match
                                       [ { v = x43.val, i = x43.info } ]
                                     with
                                       [ x44 ] ++ _
                                     in
                                     x44,
                                   ty =
                                     match
                                       [ ntVal70.1 ]
                                     with
                                       [ x45 ] ++ _
                                     in
                                     x45 } ]
                               val38.fields } },
             { nt = kleene18,
               label = {},
               rhs = "",
               action =
                 lam state137: {errors: Ref [(Info, [Char])], content: String}.
                   lam res137.
                     match
                       res137
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           fields = "" } },
             { nt = alt19,
               label = {},
               rhs = "",
               action =
                 lam state138: {errors: Ref [(Info, [Char])], content: String}.
                   lam res138.
                     match
                       res138
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           fields = "" } },
             { nt = alt19,
               label = {},
               rhs =
                 [ tokSym (LIdentRepr
                        {}),
                   litSym ":",
                   ntSym #var"RtpplType",
                   ntSym kleene18 ],
               action =
                 lam state139: {errors: Ref [(Info, [Char])], content: String}.
                   lam res139.
                     match
                       res139
                     with
                       [ TokParsed (LIdentTok x46),
                         LitParsed l141,
                         UserSym ntVal71,
                         UserSym val38 ]
                     in
                     let ntVal71: (Info, RtpplType) = fromDyn ntVal71 in
                       let val38: {fields: [{id: {i: Info, v: String}, ty: RtpplType}], __br_info: Info, __br_terms: [Info]} = fromDyn val38
                       in
                       asDyn
                         { __br_info =
                             foldl
                               mergeInfo
                               x46.info
                               [ l141.info,
                                 ntVal71.0,
                                 val38.__br_info ],
                           __br_terms =
                             join
                               [ [ x46.info ],
                                 [ l141.info ],
                                 val38.__br_terms ],
                           fields =
                             concat
                               [ { id =
                                     match
                                       [ { v = x46.val, i = x46.info } ]
                                     with
                                       [ x47 ] ++ _
                                     in
                                     x47,
                                   ty =
                                     match
                                       [ ntVal71.1 ]
                                     with
                                       [ x48 ] ++ _
                                     in
                                     x48 } ]
                               val38.fields } },
             { nt = #var"RtpplTypeAtom",
               label = {},
               rhs =
                 [ litSym "{",
                   ntSym alt19,
                   litSym "}" ],
               action =
                 lam state140: {errors: Ref [(Info, [Char])], content: String}.
                   lam res140.
                     match
                       res140
                     with
                       [ LitParsed l142,
                         UserSym val39,
                         LitParsed l143 ]
                     in
                     let val39: {fields: [{id: {i: Info, v: String}, ty: RtpplType}], __br_info: Info, __br_terms: [Info]} = fromDyn val39
                       in
                       asDyn
                         (RecordRtpplTypeOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l142.info
                                  [ val39.__br_info,
                                    l143.info ],
                              __br_terms =
                                join
                                  [ [ l142.info ],
                                    val39.__br_terms,
                                    [ l143.info ] ],
                              fields = val39.fields }) },
             { nt = #var"RtpplTypeInfix",
               label = {},
               rhs = [ litSym "->" ],
               action =
                 lam state141: {errors: Ref [(Info, [Char])], content: String}.
                   lam res141.
                     match
                       res141
                     with
                       [ LitParsed l144 ]
                     in
                     asDyn
                         (FunctionRtpplTypeOp
                            { __br_info = l144.info, __br_terms = [ l144.info ] }) },
             { nt = #var"RtpplTypeAtom",
               label = {},
               rhs =
                 [ litSym "Dist",
                   litSym "(",
                   ntSym #var"RtpplType",
                   litSym ")" ],
               action =
                 lam state142: {errors: Ref [(Info, [Char])], content: String}.
                   lam res142.
                     match
                       res142
                     with
                       [ LitParsed l145,
                         LitParsed l146,
                         UserSym ntVal72,
                         LitParsed l147 ]
                     in
                     let ntVal72: (Info, RtpplType) = fromDyn ntVal72 in
                       asDyn
                         (DistRtpplTypeOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l145.info
                                  [ l146.info,
                                    ntVal72.0,
                                    l147.info ],
                              __br_terms =
                                join
                                  [ [ l145.info ],
                                    [ l146.info ],
                                    [ l147.info ] ],
                              ty = [ ntVal72.1 ] }) },
             { nt = #var"RtpplTypeAtom",
               label = {},
               rhs =
                 [ tokSym (UIdentRepr
                        {}),
                   ntSym #var"RtpplTypeNoIdent" ],
               action =
                 lam state143: {errors: Ref [(Info, [Char])], content: String}.
                   lam res143.
                     match
                       res143
                     with
                       [ TokParsed (UIdentTok x49),
                         UserSym ntVal73 ]
                     in
                     let ntVal73: (Info, RtpplTypeNoIdent) = fromDyn ntVal73 in
                       asDyn
                         (AliasRtpplTypeOp
                            { __br_info = mergeInfo x49.info ntVal73.0,
                              __br_terms = [ x49.info ],
                              id = [ { v = nameNoSym x49.val, i = x49.info } ],
                              next = [ ntVal73.1 ] }) },
             { nt = #var"RtpplTypeNoIdentAtom",
               label = {},
               rhs = "",
               action =
                 lam state144: {errors: Ref [(Info, [Char])], content: String}.
                   lam res144.
                     match
                       res144
                     with
                       ""
                     in
                     asDyn
                         (DirectRtpplTypeNoIdentOp
                            { __br_info = NoInfo
                                  {},
                              __br_terms = "" }) },
             { nt = kleene19,
               label = {},
               rhs =
                 [ litSym ",",
                   ntSym #var"RtpplType",
                   ntSym kleene19 ],
               action =
                 lam state145: {errors: Ref [(Info, [Char])], content: String}.
                   lam res145.
                     match
                       res145
                     with
                       [ LitParsed l148,
                         UserSym ntVal74,
                         UserSym val40 ]
                     in
                     let ntVal74: (Info, RtpplType) = fromDyn ntVal74 in
                       let val40: {args: [RtpplType], __br_info: Info, __br_terms: [Info]} = fromDyn val40
                       in
                       asDyn
                         { __br_info =
                             foldl
                               mergeInfo
                               l148.info
                               [ ntVal74.0,
                                 val40.__br_info ],
                           __br_terms = concat [ l148.info ] val40.__br_terms,
                           args = concat [ ntVal74.1 ] val40.args } },
             { nt = kleene19,
               label = {},
               rhs = "",
               action =
                 lam state146: {errors: Ref [(Info, [Char])], content: String}.
                   lam res146.
                     match
                       res146
                     with
                       ""
                     in
                     asDyn
                         { __br_info = NoInfo
                               {},
                           __br_terms = "",
                           args = "" } },
             { nt = #var"RtpplTypeNoIdentAtom",
               label = {},
               rhs =
                 [ litSym "(",
                   ntSym #var"RtpplType",
                   ntSym kleene19,
                   litSym ")" ],
               action =
                 lam state147: {errors: Ref [(Info, [Char])], content: String}.
                   lam res147.
                     match
                       res147
                     with
                       [ LitParsed l149,
                         UserSym ntVal75,
                         UserSym val40,
                         LitParsed l150 ]
                     in
                     let ntVal75: (Info, RtpplType) = fromDyn ntVal75 in
                       let val40: {args: [RtpplType], __br_info: Info, __br_terms: [Info]} = fromDyn val40
                       in
                       asDyn
                         (ApplicationRtpplTypeNoIdentOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l149.info
                                  [ ntVal75.0,
                                    val40.__br_info,
                                    l150.info ],
                              __br_terms =
                                join
                                  [ [ l149.info ],
                                    val40.__br_terms,
                                    [ l150.info ] ],
                              args = concat [ ntVal75.1 ] val40.args }) },
             { nt = #var"RtpplConstAtom",
               label = {},
               rhs = [ tokSym (IntRepr
                        {}) ],
               action =
                 lam state148: {errors: Ref [(Info, [Char])], content: String}.
                   lam res148.
                     match
                       res148
                     with
                       [ TokParsed (IntTok x50) ]
                     in
                     asDyn
                         (LitIntRtpplConstOp
                            { __br_info = x50.info,
                              __br_terms = [ x50.info ],
                              value = [ { v = x50.val, i = x50.info } ] }) },
             { nt = #var"RtpplConstAtom",
               label = {},
               rhs = [ tokSym (FloatRepr
                        {}) ],
               action =
                 lam state149: {errors: Ref [(Info, [Char])], content: String}.
                   lam res149.
                     match
                       res149
                     with
                       [ TokParsed (FloatTok x51) ]
                     in
                     asDyn
                         (LitFloatRtpplConstOp
                            { __br_info = x51.info,
                              __br_terms = [ x51.info ],
                              value = [ { v = x51.val, i = x51.info } ] }) },
             { nt = #var"RtpplConstAtom",
               label = {},
               rhs = [ tokSym (BoolRepr
                        {}) ],
               action =
                 lam state150: {errors: Ref [(Info, [Char])], content: String}.
                   lam res150.
                     match
                       res150
                     with
                       [ TokParsed (BoolTok x52) ]
                     in
                     asDyn
                         (LitBoolRtpplConstOp
                            { __br_info = x52.info,
                              __br_terms = [ x52.info ],
                              value = [ { v = x52.val, i = x52.info } ] }) },
             { nt = #var"RtpplConstAtom",
               label = {},
               rhs = [ tokSym (StringRepr
                        {}) ],
               action =
                 lam state151: {errors: Ref [(Info, [Char])], content: String}.
                   lam res151.
                     match
                       res151
                     with
                       [ TokParsed (StringTok x53) ]
                     in
                     asDyn
                         (LitStringRtpplConstOp
                            { __br_info = x53.info,
                              __br_terms = [ x53.info ],
                              value = [ { v = x53.val, i = x53.info } ] }) },
             { nt = #var"RtpplExprAtom",
               label = {},
               rhs =
                 [ litSym "(",
                   ntSym #var"RtpplExpr",
                   litSym ")" ],
               action =
                 lam #var"".
                   lam seq.
                     match
                       seq
                     with
                       [ LitParsed l151,
                         UserSym ntVal76,
                         LitParsed l152 ]
                     in
                     let ntVal76: (Info, RtpplExpr) = fromDyn ntVal76 in
                       asDyn
                         (RtpplExprGrouping
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l151.info
                                  [ ntVal76.0,
                                    l152.info ],
                              __br_terms =
                                [ l151.info,
                                  l152.info ],
                              inner =
                                match
                                  [ ntVal76.1 ]
                                with
                                  [ x54 ]
                                in
                                x54 }) },
             { nt = #var"RtpplTypeAtom",
               label = {},
               rhs =
                 [ litSym "(",
                   ntSym #var"RtpplType",
                   litSym ")" ],
               action =
                 lam #var"".
                   lam seq1.
                     match
                       seq1
                     with
                       [ LitParsed l153,
                         UserSym ntVal77,
                         LitParsed l154 ]
                     in
                     let ntVal77: (Info, RtpplType) = fromDyn ntVal77 in
                       asDyn
                         (RtpplTypeGrouping
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l153.info
                                  [ ntVal77.0,
                                    l154.info ],
                              __br_terms =
                                [ l153.info,
                                  l154.info ],
                              inner =
                                match
                                  [ ntVal77.1 ]
                                with
                                  [ x54 ]
                                in
                                x54 }) },
             { nt = #var"RtpplProgram",
               label = {},
               rhs = [ ntSym #var"RtpplProgram_lclosed" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})) },
             { nt = #var"RtpplProgram_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplProgramAtom",
                   ntSym #var"RtpplProgram_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplProgramOpAtom p (fromDyn x54) st)) },
             { nt = #var"RtpplProgram_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplProgramInfix",
                   ntSym #var"RtpplProgram_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplProgramOpInfix p (fromDyn x54) st)) },
             { nt = #var"RtpplProgram_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplProgramPrefix",
                   ntSym #var"RtpplProgram_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplProgramOpPrefix p (fromDyn x54) st)) },
             { nt = #var"RtpplProgram_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplProgramPostfix",
                   ntSym #var"RtpplProgram_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplProgramOpPostfix p (fromDyn x54) st)) },
             { nt = #var"RtpplProgram_lopen",
               label = {},
               rhs = "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeRtpplProgramOp p st) },
             { nt = #var"RtpplTop",
               label = {},
               rhs = [ ntSym #var"RtpplTop_lclosed" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})) },
             { nt = #var"RtpplTop_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTopAtom",
                   ntSym #var"RtpplTop_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTopOpAtom p (fromDyn x54) st)) },
             { nt = #var"RtpplTop_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTopInfix",
                   ntSym #var"RtpplTop_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTopOpInfix p (fromDyn x54) st)) },
             { nt = #var"RtpplTop_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTopPrefix",
                   ntSym #var"RtpplTop_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTopOpPrefix p (fromDyn x54) st)) },
             { nt = #var"RtpplTop_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTopPostfix",
                   ntSym #var"RtpplTop_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTopOpPostfix p (fromDyn x54) st)) },
             { nt = #var"RtpplTop_lopen",
               label = {},
               rhs = "",
               action =
                 lam p.
                   lam #var"".
                     asDyn (lam st.
                          finalizeRtpplTopOp p st) },
             { nt = #var"RtpplTopParams",
               label = {},
               rhs = [ ntSym #var"RtpplTopParams_lclosed" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})) },
             { nt = #var"RtpplTopParams_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTopParamsAtom",
                   ntSym #var"RtpplTopParams_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTopParamsOpAtom p (fromDyn x54) st)) },
             { nt = #var"RtpplTopParams_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTopParamsInfix",
                   ntSym #var"RtpplTopParams_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTopParamsOpInfix p (fromDyn x54) st)) },
             { nt = #var"RtpplTopParams_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTopParamsPrefix",
                   ntSym #var"RtpplTopParams_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTopParamsOpPrefix p (fromDyn x54) st)) },
             { nt = #var"RtpplTopParams_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTopParamsPostfix",
                   ntSym #var"RtpplTopParams_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTopParamsOpPostfix p (fromDyn x54) st)) },
             { nt = #var"RtpplTopParams_lopen",
               label = {},
               rhs = "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeRtpplTopParamsOp p st) },
             { nt = #var"RtpplStmt",
               label = {},
               rhs = [ ntSym #var"RtpplStmt_lclosed" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})) },
             { nt = #var"RtpplStmt_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplStmtAtom",
                   ntSym #var"RtpplStmt_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplStmtOpAtom p (fromDyn x54) st)) },
             { nt = #var"RtpplStmt_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplStmtInfix",
                   ntSym #var"RtpplStmt_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplStmtOpInfix p (fromDyn x54) st)) },
             { nt = #var"RtpplStmt_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplStmtPrefix",
                   ntSym #var"RtpplStmt_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplStmtOpPrefix p (fromDyn x54) st)) },
             { nt = #var"RtpplStmt_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplStmtPostfix",
                   ntSym #var"RtpplStmt_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplStmtOpPostfix p (fromDyn x54) st)) },
             { nt = #var"RtpplStmt_lopen",
               label = {},
               rhs = "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeRtpplStmtOp p st) },
             { nt = #var"RtpplStmtNoIdent",
               label = {},
               rhs = [ ntSym #var"RtpplStmtNoIdent_lclosed" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})) },
             { nt = #var"RtpplStmtNoIdent_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplStmtNoIdentAtom",
                   ntSym #var"RtpplStmtNoIdent_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplStmtNoIdentOpAtom p (fromDyn x54) st)) },
             { nt = #var"RtpplStmtNoIdent_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplStmtNoIdentInfix",
                   ntSym #var"RtpplStmtNoIdent_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplStmtNoIdentOpInfix p (fromDyn x54) st)) },
             { nt = #var"RtpplStmtNoIdent_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplStmtNoIdentPrefix",
                   ntSym #var"RtpplStmtNoIdent_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplStmtNoIdentOpPrefix p (fromDyn x54) st)) },
             { nt = #var"RtpplStmtNoIdent_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplStmtNoIdentPostfix",
                   ntSym #var"RtpplStmtNoIdent_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplStmtNoIdentOpPostfix p (fromDyn x54) st)) },
             { nt = #var"RtpplStmtNoIdent_lopen",
               label = {},
               rhs = "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeRtpplStmtNoIdentOp p st) },
             { nt = #var"RtpplExpr",
               label = {},
               rhs = [ ntSym #var"RtpplExpr_lclosed" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})) },
             { nt = #var"RtpplExpr_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplExprAtom",
                   ntSym #var"RtpplExpr_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplExprOpAtom p (fromDyn x54) st)) },
             { nt = #var"RtpplExpr_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplExprInfix",
                   ntSym #var"RtpplExpr_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplExprOpInfix p (fromDyn x54) st)) },
             { nt = #var"RtpplExpr_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplExprPrefix",
                   ntSym #var"RtpplExpr_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplExprOpPrefix p (fromDyn x54) st)) },
             { nt = #var"RtpplExpr_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplExprPostfix",
                   ntSym #var"RtpplExpr_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplExprOpPostfix p (fromDyn x54) st)) },
             { nt = #var"RtpplExpr_lopen",
               label = {},
               rhs = "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeRtpplExprOp p st) },
             { nt = #var"RtpplExprNoIdent",
               label = {},
               rhs = [ ntSym #var"RtpplExprNoIdent_lclosed" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})) },
             { nt = #var"RtpplExprNoIdent_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplExprNoIdentAtom",
                   ntSym #var"RtpplExprNoIdent_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplExprNoIdentOpAtom p (fromDyn x54) st)) },
             { nt = #var"RtpplExprNoIdent_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplExprNoIdentInfix",
                   ntSym #var"RtpplExprNoIdent_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplExprNoIdentOpInfix p (fromDyn x54) st)) },
             { nt = #var"RtpplExprNoIdent_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplExprNoIdentPrefix",
                   ntSym #var"RtpplExprNoIdent_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplExprNoIdentOpPrefix p (fromDyn x54) st)) },
             { nt = #var"RtpplExprNoIdent_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplExprNoIdentPostfix",
                   ntSym #var"RtpplExprNoIdent_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplExprNoIdentOpPostfix p (fromDyn x54) st)) },
             { nt = #var"RtpplExprNoIdent_lopen",
               label = {},
               rhs = "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeRtpplExprNoIdentOp p st) },
             { nt = #var"RtpplType",
               label = {},
               rhs = [ ntSym #var"RtpplType_lclosed" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})) },
             { nt = #var"RtpplType_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTypeAtom",
                   ntSym #var"RtpplType_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTypeOpAtom p (fromDyn x54) st)) },
             { nt = #var"RtpplType_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTypeInfix",
                   ntSym #var"RtpplType_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTypeOpInfix p (fromDyn x54) st)) },
             { nt = #var"RtpplType_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTypePrefix",
                   ntSym #var"RtpplType_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTypeOpPrefix p (fromDyn x54) st)) },
             { nt = #var"RtpplType_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTypePostfix",
                   ntSym #var"RtpplType_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTypeOpPostfix p (fromDyn x54) st)) },
             { nt = #var"RtpplType_lopen",
               label = {},
               rhs = "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeRtpplTypeOp p st) },
             { nt = #var"RtpplTypeNoIdent",
               label = {},
               rhs = [ ntSym #var"RtpplTypeNoIdent_lclosed" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})) },
             { nt = #var"RtpplTypeNoIdent_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTypeNoIdentAtom",
                   ntSym #var"RtpplTypeNoIdent_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTypeNoIdentOpAtom p (fromDyn x54) st)) },
             { nt = #var"RtpplTypeNoIdent_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTypeNoIdentInfix",
                   ntSym #var"RtpplTypeNoIdent_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTypeNoIdentOpInfix p (fromDyn x54) st)) },
             { nt = #var"RtpplTypeNoIdent_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTypeNoIdentPrefix",
                   ntSym #var"RtpplTypeNoIdent_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTypeNoIdentOpPrefix p (fromDyn x54) st)) },
             { nt = #var"RtpplTypeNoIdent_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTypeNoIdentPostfix",
                   ntSym #var"RtpplTypeNoIdent_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTypeNoIdentOpPostfix p (fromDyn x54) st)) },
             { nt = #var"RtpplTypeNoIdent_lopen",
               label = {},
               rhs = "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeRtpplTypeNoIdentOp p st) },
             { nt = #var"RtpplConst",
               label = {},
               rhs = [ ntSym #var"RtpplConst_lclosed" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})) },
             { nt = #var"RtpplConst_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplConstAtom",
                   ntSym #var"RtpplConst_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplConstOpAtom p (fromDyn x54) st)) },
             { nt = #var"RtpplConst_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplConstInfix",
                   ntSym #var"RtpplConst_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplConstOpInfix p (fromDyn x54) st)) },
             { nt = #var"RtpplConst_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplConstPrefix",
                   ntSym #var"RtpplConst_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplConstOpPrefix p (fromDyn x54) st)) },
             { nt = #var"RtpplConst_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplConstPostfix",
                   ntSym #var"RtpplConst_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplConstOpPostfix p (fromDyn x54) st)) },
             { nt = #var"RtpplConst_lopen",
               label = {},
               rhs = "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeRtpplConstOp p st) },
             { nt = #var"RtpplPort",
               label = {},
               rhs = [ ntSym #var"RtpplPort_lclosed" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})) },
             { nt = #var"RtpplPort_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplPortAtom",
                   ntSym #var"RtpplPort_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplPortOpAtom p (fromDyn x54) st)) },
             { nt = #var"RtpplPort_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplPortInfix",
                   ntSym #var"RtpplPort_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplPortOpInfix p (fromDyn x54) st)) },
             { nt = #var"RtpplPort_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplPortPrefix",
                   ntSym #var"RtpplPort_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplPortOpPrefix p (fromDyn x54) st)) },
             { nt = #var"RtpplPort_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplPortPostfix",
                   ntSym #var"RtpplPort_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplPortOpPostfix p (fromDyn x54) st)) },
             { nt = #var"RtpplPort_lopen",
               label = {},
               rhs = "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeRtpplPortOp p st) },
             { nt = #var"RtpplMain",
               label = {},
               rhs = [ ntSym #var"RtpplMain_lclosed" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})) },
             { nt = #var"RtpplMain_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplMainAtom",
                   ntSym #var"RtpplMain_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplMainOpAtom p (fromDyn x54) st)) },
             { nt = #var"RtpplMain_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplMainInfix",
                   ntSym #var"RtpplMain_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplMainOpInfix p (fromDyn x54) st)) },
             { nt = #var"RtpplMain_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplMainPrefix",
                   ntSym #var"RtpplMain_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplMainOpPrefix p (fromDyn x54) st)) },
             { nt = #var"RtpplMain_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplMainPostfix",
                   ntSym #var"RtpplMain_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplMainOpPostfix p (fromDyn x54) st)) },
             { nt = #var"RtpplMain_lopen",
               label = {},
               rhs = "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeRtpplMainOp p st) },
             { nt = #var"RtpplExt",
               label = {},
               rhs = [ ntSym #var"RtpplExt_lclosed" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})) },
             { nt = #var"RtpplExt_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplExtAtom",
                   ntSym #var"RtpplExt_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplExtOpAtom p (fromDyn x54) st)) },
             { nt = #var"RtpplExt_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplExtInfix",
                   ntSym #var"RtpplExt_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplExtOpInfix p (fromDyn x54) st)) },
             { nt = #var"RtpplExt_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplExtPrefix",
                   ntSym #var"RtpplExt_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplExtOpPrefix p (fromDyn x54) st)) },
             { nt = #var"RtpplExt_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplExtPostfix",
                   ntSym #var"RtpplExt_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplExtOpPostfix p (fromDyn x54) st)) },
             { nt = #var"RtpplExt_lopen",
               label = {},
               rhs = "",
               action =
                 lam p.
                   lam #var"".
                     asDyn (lam st.
                          finalizeRtpplExtOp p st) },
             { nt = #var"RtpplTask",
               label = {},
               rhs = [ ntSym #var"RtpplTask_lclosed" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})) },
             { nt = #var"RtpplTask_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTaskAtom",
                   ntSym #var"RtpplTask_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTaskOpAtom p (fromDyn x54) st)) },
             { nt = #var"RtpplTask_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTaskInfix",
                   ntSym #var"RtpplTask_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTaskOpInfix p (fromDyn x54) st)) },
             { nt = #var"RtpplTask_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTaskPrefix",
                   ntSym #var"RtpplTask_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTaskOpPrefix p (fromDyn x54) st)) },
             { nt = #var"RtpplTask_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplTaskPostfix",
                   ntSym #var"RtpplTask_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplTaskOpPostfix p (fromDyn x54) st)) },
             { nt = #var"RtpplTask_lopen",
               label = {},
               rhs = "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeRtpplTaskOp p st) },
             { nt = #var"RtpplConnection",
               label = {},
               rhs = [ ntSym #var"RtpplConnection_lclosed" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})) },
             { nt = #var"RtpplConnection_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplConnectionAtom",
                   ntSym #var"RtpplConnection_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplConnectionOpAtom p (fromDyn x54) st)) },
             { nt = #var"RtpplConnection_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplConnectionInfix",
                   ntSym #var"RtpplConnection_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplConnectionOpInfix p (fromDyn x54) st)) },
             { nt = #var"RtpplConnection_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplConnectionPrefix",
                   ntSym #var"RtpplConnection_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplConnectionOpPrefix p (fromDyn x54) st)) },
             { nt = #var"RtpplConnection_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplConnectionPostfix",
                   ntSym #var"RtpplConnection_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplConnectionOpPostfix p (fromDyn x54) st)) },
             { nt = #var"RtpplConnection_lopen",
               label = {},
               rhs = "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeRtpplConnectionOp p st) },
             { nt = #var"RtpplPortSpec",
               label = {},
               rhs = [ ntSym #var"RtpplPortSpec_lclosed" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState {})) },
             { nt = #var"RtpplPortSpec_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplPortSpecAtom",
                   ntSym #var"RtpplPortSpec_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplPortSpecOpAtom p (fromDyn x54) st)) },
             { nt = #var"RtpplPortSpec_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplPortSpecInfix",
                   ntSym #var"RtpplPortSpec_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplPortSpecOpInfix p (fromDyn x54) st)) },
             { nt = #var"RtpplPortSpec_lclosed",
               label = {},
               rhs =
                 [ ntSym #var"RtpplPortSpecPrefix",
                   ntSym #var"RtpplPortSpec_lclosed" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplPortSpecOpPrefix p (fromDyn x54) st)) },
             { nt = #var"RtpplPortSpec_lopen",
               label = {},
               rhs =
                 [ ntSym #var"RtpplPortSpecPostfix",
                   ntSym #var"RtpplPortSpec_lopen" ],
               action =
                 lam p.
                   lam seq2.
                     match
                       seq2
                     with
                       [ UserSym x54,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn cont (addRtpplPortSpecOpPostfix p (fromDyn x54) st)) },
             { nt = #var"RtpplPortSpec_lopen",
               label = {},
               rhs = "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeRtpplPortSpecOp p st) } ] })
  in
  match
    target
  with
    Right table
  in
  table
let parseRtppl =
  lam filename.
    lam content.
      use ParseRtppl
      in
      let config16 = { errors = ref "", content = content } in
      let res152 = parseWithTable _table filename config16 content in
      let #var"X" = (res152, deref config16.errors) in
      match
        #var"X"
      with
        (Right dyn, "")
      then
        match
          fromDyn dyn
        with
          (_, res152)
        in
        Right
            res152
      else match
        #var"X"
      with
        (Left err, errors)
      then
        let err = ll1DefaultHighlight content (ll1ToErrorHighlightSpec err)
        in
        Left
          (snoc errors err)
      else match
        #var"X"
      with
        (_, errors)
      in
      Left
          errors
let parseRtpplExn =
  lam filename.
    lam content.
      let #var"X" = parseRtppl filename content in
      match
        #var"X"
      with
        Left errors
      then
        (for_
             errors
             (lam x54.
                match
                  x54
                with
                  (info, msg)
                in
                printLn (infoErrorString info msg)))
        ; exit 1
      else match
        #var"X"
      with
        Right file
      in
      file
mexpr
{}