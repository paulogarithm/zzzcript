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

	xs, err = Lex("1 < 1")
	if err != nil {
		t.Fatal(err)
	}
	if !n.parseLeaf(&xs) {
		t.Fatal("expected to work")
	}
}
