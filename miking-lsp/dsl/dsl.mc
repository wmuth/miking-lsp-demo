include "common.mc"
include "mexpr/info.mc"
include "mexpr/mexpr.mc"
include "mexpr/pprint.mc"
include "string.mc"
include "ext/file-ext.mc"

include "./ast-gen.mc"
include "./utils.mc"

-- Language fragment implementing 'compileToMexpr'

lang CalcCompileBase = CalcAst + MExprAst
  sem compileToMexpr : DSLExpr -> Expr

  sem _tyuk : Info -> Type
  sem _tyuk =
  | info -> TyUnknown {info = info}
end

lang NumCompile = CalcCompileBase + NumDSLExprAst
  sem compileToMexpr =
  | NumDSLExpr x -> TmConst {
		val = CFloat {
      val = x.val.v
    },
		ty = TyUnknown {info = NoInfo ()},
		info = NoInfo ()
	}
end

lang BinaryCompile = CalcCompileBase
  sem getBinaryAST l r info = 
  | op -> 
    let l = compileToMexpr l in
    let r = compileToMexpr r in
    TmApp {
      lhs = TmApp {
        lhs = TmConst {
          val = op,
          ty = _tyuk info,
          info = info
        },
        rhs = l,
        ty = _tyuk info,
        info = info
      },
      rhs = r,
      ty = _tyuk info,
      info = info
    }
end

lang FactorCompile = CalcCompileBase + BinaryCompile + MulDSLExprAst + DivDSLExprAst
  sem compileToMexpr =
  | MulDSLExpr x -> getBinaryAST x.left x.right x.info (CMulf ())
  | DivDSLExpr x -> getBinaryAST x.left x.right x.info (CDivf ())
end

lang TermCompile = CalcCompileBase + BinaryCompile + AddDSLExprAst + SubDSLExprAst
  sem compileToMexpr =
  | AddDSLExpr x -> getBinaryAST x.left x.right x.info (CAddf ())
  | SubDSLExpr x -> getBinaryAST x.left x.right x.info (CSubf ())
end

-- Composed languages

lang Complete = CalcAst + TermCompile + FactorCompile + NumCompile
  sem fileToExpr: File -> DSLExpr
  sem fileToExpr =
  | File1 record -> record.e
end

mexpr
use Complete in

let emptyEnv = mapEmpty cmpString in

let example = parseCalcExn "example" "2.0 / 4.0" in
let evaluated = evalExpr (compileToMexpr (fileToExpr example)) in
use MExprPrettyPrint in
eprintln (expr2str evaluated);
()