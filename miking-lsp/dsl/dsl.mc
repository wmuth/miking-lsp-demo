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

  sem _tyfloat : Info -> Type
  sem _tyfloat =
  | info -> TyFloat {info = info}
end

lang NumCompile = CalcCompileBase + NumDSLExprAst
  sem compileToMexpr =
  | NumDSLExpr x -> TmConst {
		val = CFloat {
      val = x.val.v
    },
		ty = _tyfloat x.info,
		info = x.info
	}
end

lang BinaryCompile
= CalcCompileBase
+ MulDSLExprAst + DivDSLExprAst
+ AddDSLExprAst + SubDSLExprAst
  sem getBinaryAST l r info = 
  | op -> 
    let lc = compileToMexpr l in
    let rc = compileToMexpr r in
    let linfo = get_DSLExpr_info l in
    let rinfo = get_DSLExpr_info r in

    -- use mergeInfo maybe?

    TmApp {
      lhs = TmApp {
        lhs = TmConst {
          val = op,
          ty = _tyfloat info,
          info = info
        },
        rhs = lc,
        ty = _tyfloat linfo,
        info = info
      },
      rhs = rc,
      ty = _tyfloat rinfo,
      info = info
    }

  sem compileToMexpr =
  | MulDSLExpr x -> getBinaryAST x.left x.right x.info (CMulf ())
  | DivDSLExpr x -> getBinaryAST x.left x.right x.info (CDivf ())
  | AddDSLExpr x -> getBinaryAST x.left x.right x.info (CAddf ())
  | SubDSLExpr x -> getBinaryAST x.left x.right x.info (CSubf ())
end

-- Composed languages

lang Complete = CalcAst + BinaryCompile + NumCompile
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
dprint evaluated;
()

-- to get better dprint
-- boot eval miking-lsp/dsl/dsl.mc