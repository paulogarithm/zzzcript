package main

import (
	"fmt"
	"log"
	"zzz/src"
)

type Boo struct {
	X string
}

func test() {
	x := "hello"
	e := Boo{x}
	x = "haha"
	fmt.Println(e)
}

func main() {
	// xs, err := zzz.Symbolize(`
	// 	import print in io;

	// 	basically fact(int) -> int;
	// 	func fact(<1) return 1;
	// 	func fact(n) return fact(n - 1) * n;

	// 	func main() {
	// 		print(fact(5) == 120);
	// 		print(8 && !(3 >> 2.5)+~(  3*.3))
	// 	}
	// `)

	// xs, err := zzz.Symbolize(`
	// 	import print, getLine in io;

	// 	basically fact(int) -> int;
	// `)

	// xs, err := zzz.Symbolize(`
	// 	import print in io;

	// 	basically struct foo -> {number a, number b};
	// 	basically hello(struct foo) -> number;

	// 	func hello(x) return x.a + x.b;

	// 	func main() {
	// 		print(hello(foo{3, 4}));
	// 	}
	// `)

	// xs, err := zzz.Lex(`
	// 	import print in io;

	// 	func main() print("hello");
	// `)

	// if err != nil {
	// 	log.Fatal(err)
	// }
	// zzz.DisplaySymbols(xs)
	// ast, err := zzz.Parse(xs)
	// if err != nil {
	// 	log.Fatal(err)
	// }
	// fmt.Println(ast)

	// err := zzz.Run("import print in io; basically succ(int) -> int; func succ(n) return n +1; func main() { print(succ(42 + 1)); }")
	err := zzz.Run(`
	import print in io;

basically fact(int) -> int;
func fact(<=1) return 1;
func fact(n) return fact(n - 1) * n;

func main() {
    print(fact(5) == 120, fact(5));
}
	`)
	if err != nil {
		log.Fatal(err)
	}
}