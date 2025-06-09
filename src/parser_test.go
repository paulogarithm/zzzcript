package zzz

import (
	"testing"
)

func TestParseGoodImport(t *testing.T) {
	n := &Node{}
	xs, err := Lex("import hello in test;")
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseImport(&xs) {
		t.Fatal("expected to work")
	}

	xs, err = Lex("import hello, world in test;")
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseImport(&xs) {
		t.Fatal("expected to work")
	}
}

func TestParseBadImport(t *testing.T) {
	n := &Node{}
	xs, err := Lex("import in world;") // no data
	if err != nil {
		t.Fatal(err)
	}
	if n.parseImport(&xs) {
		t.Fatal("not expected to work")
	}

	xs, err = Lex("import func in world;") // keyword
	if err != nil {
		t.Fatal(err)
	}
	if n.parseImport(&xs) {
		t.Fatal("not expected to work")
	}

	xs, err = Lex("import hello in func;") // keyword 2
	if err != nil {
		t.Fatal(err)
	}
	if n.parseImport(&xs) {
		t.Fatal("not expected to work")
	}

	xs, err = Lex("import foo bar in world;") // no comma
	if err != nil {
		t.Fatal(err)
	}
	if n.parseImport(&xs) {
		t.Fatal("not expected to work")
	}

	xs, err = Lex("import foo, func in world;") // one keyword
	if err != nil {
		t.Fatal(err)
	}
	if n.parseImport(&xs) {
		t.Fatal("not expected to work")
	}
}

func TestParseGoodBasicallyFunction(t *testing.T) {
	n := &Node{}

	n.makeMeta()
	xs, err := Lex("basically foo();") // no return, no arg
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseBasically(&xs) {
		t.Fatal("expected to work")
	}

	n.makeMeta()
	xs, err = Lex("basically foo() -> int;") // no arg, one return
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseBasically(&xs) {
		t.Fatal("expected to work")
	}

	n.makeMeta()
	xs, err = Lex("basically foo(int);") // one arg, no return
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseBasically(&xs) {
		t.Fatal("expected to work")
	}

	n.makeMeta()
	xs, err = Lex("basically foo(int) -> int;") // one arg, one return
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseBasically(&xs) {
		t.Fatal("expected to work")
	}

	n.makeMeta()
	xs, err = Lex("basically foo(int, int);") // 2 args, no return
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseBasically(&xs) {
		t.Fatal("expected to work")
	}

	n.makeMeta()
	xs, err = Lex("basically foo(int, int, int, int, int, int, int, int, int, int, int, int);") // alot of args, no return
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseBasically(&xs) {
		t.Fatal("expected to work")
	}

	n.makeMeta()
	xs, err = Lex("basically foo(int, number, string, boolean);") // all types
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseBasically(&xs) {
		t.Fatal("expected to work")
	}
}

func TestParseValue(t *testing.T) {
	n := &Node{}

	// GOOD
	n.makeMeta()
	xs, err := Lex("3") // int
	if err != nil {
		t.Fatal(err)
	}
	if xs[0].GetType() != symInt {
		t.Fatal("expected int, got " + xs[0].String())
	}
	if !n.parseValue(&xs) {
		t.Fatal("expected to work")
	}

	n.makeMeta()
	xs, err = Lex("\"hello\"") // string
	if err != nil {
		t.Fatal(err)
	}
	if xs[0].GetType() != symString {
		t.Fatal("expected sring, got " + xs[0].String())
	}
	if !n.parseValue(&xs) {
		t.Fatal("expected to work")
	}

	n.makeMeta()
	xs, err = Lex("true") // bool
	if err != nil {
		t.Fatal(err)
	}
	if xs[0].GetType() != symKWTrue {
		t.Fatal("expected bool, got " + xs[0].String())
	}
	if !n.parseValue(&xs) {
		t.Fatal("expected to work")
	}

	n.makeMeta()
	xs, err = Lex("prout") // def
	if err != nil {
		t.Fatal(err)
	}
	if xs[0].GetType() != symDef {
		t.Fatal("expected def, got " + xs[0].String())
	}
	if !n.parseValue(&xs) {
		t.Fatal("expected to work")
	}

	// BAD
	n.makeMeta()
	xs, err = Lex("func") // keyword
	if err != nil {
		t.Fatal(err)
	}
	if n.parseValue(&xs) {
		t.Fatal("not expected to work")
	}
}

func TestParseUnary(t *testing.T) {
	n := nodeFactory[tokProcedure]("test")

	xs, err := Lex("~3") // unary operator
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseLeaf(&xs) {
		t.Fatal("expected to work")
	}
}

func TestParsePairFromLeaf(t *testing.T) {
	n := nodeFactory[tokProcedure]("prout")
	n.makeMeta()
	xs, err := Lex("1 + 1")
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseLeaf(&xs) {
		t.Fatal("expected to work")
	}

	n = nodeFactory[tokProcedure]("prout")
	n.makeMeta()
	xs, err = Lex("1 < 1")
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseLeaf(&xs) {
		t.Fatal("expected to work")
	}

	n = nodeFactory[tokProcedure]("prout")
	n.makeMeta()
	xs, err = Lex("1 + 2 + 3")
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseLeaf(&xs) {
		t.Fatal("expected to work")
	}
}

func TestParseFunc(t *testing.T) {
	n := nodeFactory[tokProcedure]("prout")
	n.makeMeta()

	xs, err := Lex("func foo(<3, hello, 42) return 1;")
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseFunction(&xs) {
		t.Fatal("expected to work")
	}
}

func TestParseCall(t *testing.T) {
	n := nodeFactory[tokProcedure]("prout")
	n.makeMeta()

	// regular
	xs, err := Lex("hello()")
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseLeaf(&xs) {
		t.Fatal("expected to work")
	}

	// some arguments
	xs, err = Lex("hello(1, 2, 3)")
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseLeaf(&xs) {
		t.Fatal("expected to work")
	}

	// call in call
	xs, err = Lex("hello(foo(1), bar(2, baz()), 3)")
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseLeaf(&xs) {
		t.Fatal("expected to work")
	}
}

func TestParseReturn(t *testing.T) {
	n := nodeFactory[tokProcedure]("prout")
	n.makeMeta()

	// regular
	xs, err := Lex("return 3")
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseReturn(&xs) {
		t.Fatal("expected to work")
	}

	// return expression
	xs, err = Lex("return 1 + 1")
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseReturn(&xs) {
		t.Fatal("expected to work")
	}

	// return expression 2
	xs, err = Lex("return not 1 * ~3")
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseReturn(&xs) {
		t.Fatal("expected to work")
	}
}

func TestParseFactorialFullCode(t *testing.T) {
	// regular
	xs, err := Lex("import print in io; basically fact(int) -> int; func fact(<=1) return 1; func fact(n) return fact(n-1) * n;")
	if err != nil {
		t.Fatal(err)
	}
	n, err := Parse(xs)
	if err != nil {
		t.Fatal(err)
	}
	_=n
}

func TestParsePrio(t *testing.T) {
	xs, err := Lex("basically foo() -> int; func foo() return 3 * 2 + 1;")
	if err != nil {
		t.Fatal(err)
	}
	ast, err := Parse(xs)
	if err != nil {
		t.Fatal(err)
	}
	println(ast.String())
}
