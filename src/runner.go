package zzz

import (
    "fmt"
    "time"
)

func Run(code string) error {
    startLex := time.Now()
    xs, err := Lex(code)
    lexTime := time.Since(startLex)
    if err != nil {
        return err
    }
    fmt.Printf("Lex(): %v µs\n", lexTime.Microseconds())
	DisplaySymbols(xs)

    startParse := time.Now()
    ast, err := Parse(xs)
    parseTime := time.Since(startParse)
    if err != nil {
        return err
    }
	fmt.Println(ast.String())
    fmt.Printf("Parse(): %v µs\n", parseTime.Microseconds())

    startInterp := time.Now()
    err = Interpret(ast)
    interpTime := time.Since(startInterp)
    if err != nil {
        return err
    }
    fmt.Printf("Interpret(): %v µs\n", interpTime.Microseconds())

    return nil
}
