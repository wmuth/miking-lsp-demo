include "mexpr/info.mc"
include "mexpr/mexpr.mc"
include "mexpr/pprint.mc"

include "name.mc"
include "common.mc"
include "string.mc"
include "ext/file-ext.mc"

include "./ast-gen.mc"
include "./utils.mc"

-- Language fragment implementing 'compileToMexpr'

lang CalcCompileBase = CalcAst + MExprAst
  sem compileStatementsToMexpr : [DSLStatement] -> Expr
  sem compileExprToMexpr : DSLExpr -> Expr

  sem _tyuk : Info -> Type
  sem _tyuk =
  | info -> TyUnknown {info = info}

  sem _tyfloat : Info -> Type
  sem _tyfloat =
  | info -> TyFloat {info = info}
end

lang TerminatorCompile = CalcCompileBase
  -- sem compileStatementsToMexpr =
  -- | [] -> TmConst {
  --     val = CFloat {
  --       val = 0.0
  --     },
  --     ty = _tyuk (NoInfo ()),
  --     info = NoInfo ()
  -- }

  -- CFloat2string

  -- Temporary, converts the float variable "result"
  -- to string and prints it in the end
  sem compileStatementsToMexpr =
  | [] -> TmApp {
    lhs = TmConst {
      val = CPrint (),
      ty = TyUnknown {info = NoInfo ()},
      info = NoInfo ()
    },
    rhs = TmApp {
      lhs = TmConst {
        val = CFloat2string (),
        ty = TyUnknown {info = NoInfo ()},
        info = NoInfo ()
      },
      rhs = TmVar {
        ident = nameNoSym "result",
        ty = _tyuk (NoInfo ()),
        info = NoInfo (),
        frozen = false
      },
      ty = TyUnknown {info = NoInfo ()},
      info = NoInfo ()
    },
    ty = TyUnknown {info = NoInfo ()},
    info = NoInfo ()
  }
end

lang AssignmentCompile = CalcCompileBase + AssignmentDSLStatementAst
  sem compileStatementsToMexpr =
  | [AssignmentDSLStatement x] ++ rest -> TmLet {
    -- compileExprToMexpr x.val
    ident = nameNoSym x.ident.v,
    tyAnnot = _tyuk x.info,
    tyBody = _tyuk x.info,
    body = compileExprToMexpr x.val,
    inexpr = compileStatementsToMexpr rest,
    ty = _tyuk x.info,
    info = x.info
  }
end

lang VariableCompile = CalcCompileBase + VariableDSLExprAst
  sem compileExprToMexpr =
  | VariableDSLExpr x -> TmVar {
    ident = nameNoSym x.ident.v,
    ty = _tyuk x.info,
    info = x.info,
    frozen = false
  }
end

lang NumCompile = CalcCompileBase + NumDSLExprAst
  sem compileExprToMexpr =
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
    let lc = compileExprToMexpr l in
    let rc = compileExprToMexpr r in
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

  sem compileExprToMexpr =
  | MulDSLExpr x -> getBinaryAST x.left x.right x.info (CMulf ())
  | DivDSLExpr x -> getBinaryAST x.left x.right x.info (CDivf ())
  | AddDSLExpr x -> getBinaryAST x.left x.right x.info (CAddf ())
  | SubDSLExpr x -> getBinaryAST x.left x.right x.info (CSubf ())
end

-- Composed languages

lang Complete = CalcAst

-- Statements
+ TerminatorCompile + AssignmentCompile

-- Expressions
+ BinaryCompile + NumCompile + VariableCompile
  sem fileToStatements: File -> [DSLStatement]
  sem fileToStatements =
  | File1 record -> record.s
end

mexpr
use Complete in

let emptyEnv = mapEmpty cmpString in

let example = parseCalcExn "example" "let a = 2.0 + 4.0; let result = a + 1.0;" in
let evaluated = evalExpr (compileStatementsToMexpr (fileToStatements example)) in
use MExprPrettyPrint in
eprintln (expr2str evaluated);
-- dprint evaluated;
()

-- to get better dprint
-- boot eval miking-lsp/dsl/dsl.mc