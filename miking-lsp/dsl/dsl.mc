include "common.mc"
include "mexpr/info.mc"
include "string.mc"
include "ext/file-ext.mc"

include "./ast-gen.mc"

-- Helpers to convert between floats and Exprs

let exprToFloat = use NumExprAst in
  lam e. match e with NumExpr x in x.val.v
let floatToExpr = use NumExprAst in
  lam f. NumExpr {info = NoInfo (), val = {v = f, i = NoInfo()}}


-- Language fragments implementing 'eval'

lang EvalBase = CalcBaseAst
  sem eval : Map String Expr -> Expr -> Expr
end

lang NumExprEval = EvalBase + NumExprAst
  sem eval env =
  | e & NumExpr _ -> e
end

lang TermEval = EvalBase + AddExprAst + SubExprAst
  sem eval env =
  | AddExpr x ->
    let l = exprToFloat (eval env x.left) in
    let r = exprToFloat (eval env x.right) in
    floatToExpr (addf l r)
  | SubExpr x ->
    let l = exprToFloat (eval env x.left) in
    let r = exprToFloat (eval env x.right) in
    floatToExpr (subf l r)
end

lang FactorEval = EvalBase + MulExprAst + DivExprAst
  sem eval env =
  | MulExpr x ->
    let l = exprToFloat (eval env x.left) in
    let r = exprToFloat (eval env x.right) in
    floatToExpr (mulf l r)
  | DivExpr x ->
    let l = exprToFloat (eval env x.left) in
    let r = exprToFloat (eval env x.right) in
    floatToExpr (divf l r)
end


-- Language fragments implementing 'toString'

lang ToStringBase
  sem toString : Expr -> String
end

lang NumToString = ToStringBase + NumExprAst
  sem toString =
  | NumExpr x -> float2string x.val.v
end

lang TermToString = ToStringBase + AddExprAst + SubExprAst
  sem toString =
  | AddExpr x -> join ["(", toString x.left, " + ", toString x.right, ")"]
  | SubExpr x -> join ["(", toString x.left, " - ", toString x.right, ")"]
end

lang FactorToString = ToStringBase + MulExprAst + DivExprAst
  sem toString =
  | MulExpr x -> join ["(", toString x.left, " * ", toString x.right, ")"]
  | DivExpr x -> join ["(", toString x.left, " / ", toString x.right, ")"]
end

-- Composed languages

lang Eval = TermEval + NumExprEval + FactorEval end
lang ToString = NumToString + TermToString + FactorToString end

lang Complete = CalcAst + Eval + ToString
  sem fileToExpr: File -> Expr
  sem fileToExpr =
  | File1 record -> record.e
end