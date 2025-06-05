package zzz

import (
	"testing"
)

func TestDoctorFactorial(t *testing.T) {
	// regular
	xs, err := Lex("import print in io; basically fact(int) -> int; func fact(<=1) return 1; func fact(n) return fact(n-1) * n; func main() print(fact(5));")
	if err != nil {
		t.Fatal(err)
	}
	ast, err := Parse(xs)
	if err != nil {
		t.Fatal(err)
	}
	err = Doctor(ast)
	if err != nil {
		t.Fatal(err)
	}
	println(ast.String())
}
