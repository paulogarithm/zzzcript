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
	// err := zzz.Run(`
	// import print in io;

	// basically fact(int) -> int;
	// func fact(<=1) return 1;
	// func fact(n) return fact(n - 1) * n;

	// func main() {
	// 	print(fact(5) == 120, fact(5));
	// }
	// `)
	// err := zzz.Run(`
	// import print in io;

	// basically matching(string) -> string;
	// func matching("hello") return "what";
	// func matching("hello":rest) return rest;
	// func matching('h':rest) return matching(rest);
	// func matching("") return "none";
	// func matching(_) return "unknown";

	// func main() {
	// 	print(matching("hello world"));
	// 	print(matching("hello"));
	// 	print(matching("hhello"));
	// 	print(matching("hhh"));
	// }

	// `)
	// err := zzz.Run(`
	// 	import print in io;

	// 	basically fib(int) -> int;
	// 	func fib(0) return 0;
	// 	func fib(1) return 1;
	// 	func fib(n) return fib(n - 1) + fib(n - 2);

	// 	func main() print(fib(25));
	// `)
	// err := zzz.Run(`
	// 	import print in io;

	// 	basically adder(int) -> int;
	// 	func adder(0) return 0;
	// 	func adder(n) return adder(n-1) + n;

	// 	func main() print(adder(3.14));
	// `)
	err := zzz.Run(`
		import print in io;

		basically show(number) -> null;
		func show(n) print(n * .1);

		func main() show(3);
	`)
	// err := zzz.Run(`
	// 	import print in io;

	// 	func main() print(3.14);
	// `)
	if err != nil {
		log.Fatal(err)
	}
}
