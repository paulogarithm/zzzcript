package zzz

import (
	"testing"
)

func TestDoctorFactorial(t *testing.T) {
	// should not fail: factorial
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

	// should fail
	xs, err = Lex("basically foo(int) -> string; func foo(n) return n;")
	if err != nil {
		t.Fatal(err)
	}
	ast, err = Parse(xs)
	if err != nil {
		t.Fatal(err)
	}
	err = Doctor(ast)
	if err == nil {
		t.Fatal("doctor should fail")
	}
	println(err.Error())

	// should not fail
	xs, err = Lex("basically foo(int) -> int; func foo(n) return n == 1;")
	if err != nil {
		t.Fatal(err)
	}
	ast, err = Parse(xs)
	if err != nil {
		t.Fatal(err)
	}
	err = Doctor(ast)
	if err != nil {
		t.Fatal(err)
	}

	// should not fail
	xs, err = Lex("basically foo(int) -> boolean; func foo(n) return n == 1;")
	if err != nil {
		t.Fatal(err)
	}
	ast, err = Parse(xs)
	if err != nil {
		t.Fatal(err)
	}
	err = Doctor(ast)
	if err != nil {
		t.Fatal(err)
	}

	// should not fail
	xs, err = Lex("basically foo(int, int) -> number; func foo(x, y) return x + y + 1;")
	if err != nil {
		t.Fatal(err)
	}
	ast, err = Parse(xs)
	if err != nil {
		t.Fatal(err)
	}
	err = Doctor(ast)
	if err != nil {
		t.Fatal(err)
	}

	// should not fail
	xs, err = Lex("basically foo(int) -> string; func foo(x) return \"hello\" * x;")
	if err != nil {
		t.Fatal(err)
	}
	ast, err = Parse(xs)
	if err != nil {
		t.Fatal(err)
	}
	err = Doctor(ast)
	if err != nil {
		t.Fatal(err)
	}

	// should fail
	xs, err = Lex("basically foo(int) -> int; func foo(x) return \"hello\" * x;")
	if err != nil {
		t.Fatal(err)
	}
	ast, err = Parse(xs)
	if err != nil {
		t.Fatal(err)
	}
	err = Doctor(ast)
	if err == nil {
		t.Fatal("doctor should fail")
	}
	println(err.Error())

	// should not fail
	xs, err = Lex("basically foo(int); func foo(x) return 3;")
	if err != nil {
		t.Fatal(err)
	}
	ast, err = Parse(xs)
	if err != nil {
		t.Fatal(err)
	}
	err = Doctor(ast)
	if err != nil {
		t.Fatal(err)
	}

	// should fail
	xs, err = Lex("basically bar(); func bar() return; basically foo(int) -> int; func foo(x) return bar();")
	if err != nil {
		t.Fatal(err)
	}
	ast, err = Parse(xs)
	if err != nil {
		t.Fatal(err)
	}
	err = Doctor(ast)
	if err == nil {
		t.Fatal("doctor should fail")
	}
	println(err.Error())

	// should fail
	xs, err = Lex("basically bar(int); func bar() return;")
	if err != nil {
		t.Fatal(err)
	}
	ast, err = Parse(xs)
	if err != nil {
		t.Fatal(err)
	}
	err = Doctor(ast)
	if err == nil {
		t.Fatal("doctor should fail")
	}
	println(err.Error())

	// should fail
	xs, err = Lex("basically foo(int) -> int; func foo(0) return 0; func foo(n) return foo(\"hello\");")
	if err != nil {
		t.Fatal(err)
	}
	ast, err = Parse(xs)
	if err != nil {
		t.Fatal(err)
	}
	err = Doctor(ast)
	if err == nil {
		t.Fatal("doctor should fail")
	}
	println(err.Error())
}
