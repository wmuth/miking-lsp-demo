include "mexpr/info.mc"
include "mexpr/mexpr.mc"
include "mexpr/symbolize.mc"
include "mexpr/pprint.mc"

include "name.mc"
include "common.mc"
include "string.mc"
include "ext/file-ext.mc"

include "./ast-gen.mc"
include "./utils.mc"
include "./mexpr.mc"
include "./lsp/utils.mc"
include "./lsp/lsp.mc"

-- Language fragment implementing 'compileToMexpr'

type LSPContext = {}

lang CalcCompileBase = CalcAst + MExprAst
  sem compileStatementsToMexpr : [DSLStatement] -> Expr
  sem stmtToLSP: LSPContext -> [DSLStatement] -> [LSPImplementations]
  sem stmtToLSP context =
  | [] -> []

  sem compileExprToMexpr : DSLExpr -> Expr
  sem exprToLSP: LSPContext -> DSLExpr -> [LSPImplementations]
  sem exprToLSP context =
  | _ -> []

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

  -- sem stmtToLSP context =
  -- | [] -> []

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
  sem stmtToLSP context =
  | [AssignmentDSLStatement x] ++ rest ->
    let restRes = stmtToLSP context rest in
    let child = exprToLSP context x.val in
    let parent = [
      { lsp with
        hover = [
          {
            info = x.ident.i,
            content = join ["Assignment: ", x.ident.v]
          }
        ]
      }
    ] in
    join [child, parent, restRes]

  sem compileStatementsToMexpr =
  | [AssignmentDSLStatement x] ++ rest -> TmLet {
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
  sem exprToLSP context =
  | VariableDSLExpr x -> [{ lsp with
    hover = [
      {
        info = x.info,
        content = join ["Variable: ", x.ident.v]
      }
    ]
  }]

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

  sem exprToLSP context =
  | AddDSLExpr x
  | MulDSLExpr x
  | AddDSLExpr x
  | SubDSLExpr x -> join [exprToLSP context x.left, exprToLSP context x.right]

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

let example = parseCalcExn "example" "let a = 2.0 + 4.0; let b = 2.0 + 4.0; let a = a + b; let result = (a + 1.0) + b;" in
-- dprint example;
let context = {} in
-- let lsp = stmtToLSP context (fileToStatements example) in

-- let implementations = foldl (
--   lam acc. lam x.
--     let rest = foldl (
--       lam acc. lam x.
--         join [acc, [x]]
--     ) [] x.hover in
--     join [acc, rest]
-- ) [] lsp in

-- eprintln (
--   strJoin ", " (map (lam x.
--     let info = getFileInfo x.info in
--     join [
--       "<",
--       x.content,
--       " @ ",
--       int2string info.colStart,
--       ":",
--       int2string info.lineStart,
--       "-",
--       int2string info.colEnd,
--       ":",
--       int2string info.lineEnd,
--       ">"
--     ]) implementations
--   )
-- );

-- eprintln implementations;

-- eprintln (foldl (lam acc. lam x. join [
--   "(",
--     acc,
--     ", ",
--     (foldl (lam acc. lam x. join [
--       "[",
--         acc,
--         ", <",
--         x.content,
--         ">",
--       "]"
--     ]) "" x.hover),
--   ")"
-- ]) "" lsp);

-- (fileToStatements example)


let mexprAst = compileStatementsToMexpr (fileToStatements example) in

use MExprLSP in
let definitionTree = buildDefinitionTree mexprAst in
(
  match (mapLookup "a" definitionTree) with Some info
    then eprintln (join ["Found a:", info2str info])
    else eprintln "Not found"
);

(
  match (mapLookup "b" definitionTree) with Some info
    then eprintln (join ["Found b:", info2str info])
    else eprintln "Not found"
);

eprintln "Symbolized:";
let symbolized = use MExpr in symbolize mexprAst in
eprintln (use MExprPrettyPrint in expr2str symbolized);

eprintln "Evaluated:";
let evaluated = evalExpr mexprAst in
use MExprPrettyPrint in
eprintln (expr2str evaluated);
-- dprint evaluated;
()

-- to get better dprint
-- boot eval miking-lsp/dsl/dsl.mc