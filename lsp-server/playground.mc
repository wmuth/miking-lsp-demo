include "utils.mc"
include "dsl.mc"
include "ast-gen.mc"
-- include "mexpr/eval.mc"
include "mexpr/ast.mc"
-- include "mexpr/utils.mc"
include "mexpr/mexpr.mc"

-- type Person = {
-- 	name: String,
-- 	age: Int
-- }

-- let __String_toString: String -> String = lam s. s
-- let __Int_toString: Int -> String = int2string
-- let __Person_toString: Person -> String = lam person.
-- 	join ["Person { name: ", __String_toString person.name, ", age: ", __Int_toString person.age, " }"]

-- mexpr

-- let person = {
-- 	name = "John",
-- 	age2 = 30
-- } in

-- eprintln (__Person_toString person)

mexpr
use MExprAst in

let expr = TmApp {
	lhs = TmConst {
		val = CPrint (),
		ty = TyUnknown {info = NoInfo ()},
		info = NoInfo ()
	},
	rhs = TmSeq {
		tms = [
			TmConst {
				val = CChar {
					val = 'a'
				},
				ty = TyInt {info = NoInfo ()},
				info = NoInfo ()
			}
		],
		info = NoInfo (),
		ty = TyUnknown {info = NoInfo ()}
	},
	ty = TyUnknown {info = NoInfo ()},
	info = NoInfo ()
} in

let a = evalExpr expr in

()