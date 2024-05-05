include "utils.mc"

type Person = {
	name: String,
	age: Int
}

let __String_toString: String -> String = lam s. s
let __Int_toString: Int -> String = int2string
let __Person_toString: Person -> String = lam person.
	join ["Person { name: ", __String_toString person.name, ", age: ", __Int_toString person.age, " }"]

mexpr

let person = {
	name = "John",
	age2 = 30
} in

eprintln (__Person_toString person)