package zzz

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
)

type symbolType uint

const (
	symString symbolType = iota	// "hello"
	symParOpen					// (
	symParClose					// )
	symSquareOpen				// [
	symSquareClose				// ]
	symCurlyOpen				// {
	symCurlyClose				// }
	symSemicolon				// ;
	symDef						// hello
	symNumber					// 12.2, 3.14, ...
	symInt						// 12, 10, ...
	symSet						// =
	symPlus						// +
	symMinus					// -
	symDiv						// /
	symMul						// *
	symGT						// >
	symLT						// <
	symGE						// >=
	symLE						// <=
	symIsEqual 					// ==
	symIsDiff 					// !=
	symComma 					// ,
	symDot 						// .
	symBand 					// &
	symBor 						// |
	symBxor 					// ^
	symBnot 					// ~
	symArrow					// ->
	symShl						// <<
	symShr						// >>
	symAnd 						// &&, and
	symNot 						// !, not
	symOr 						// ||, or

	symKWIn 					// in
	symKWReturn 				// return
	symKWFunc 					// func
	symKWWhere 					// where
	symKWStruct 				// struct
	symKWBasically				// basically
	symKWTrue					// true
	symKWFalse					// false
	symKWIf						// if
	symKWElse					// else
	symKWImport					// import
	symKWNull					// null

	symTInt 					// int
	symTNumber 					// number
	symTString 					// string
	symTBool					// bool

	symEnd
)

var char2sym = map[rune]symbolType{
	'(': symParOpen,
	')': symParClose,
	'{': symCurlyOpen,
	'}': symCurlyClose,
	'[': symSquareOpen,
	']': symSquareClose,
	';': symSemicolon,
	'=': symSet,
	'<': symLT,
	'>': symGT,
	'+': symPlus,
	'-': symMinus,
	'/': symDiv,
	'*': symMul,
	',': symComma,
	'.': symDot,
	'&': symBand,
	'|': symBor,
	'^': symBxor,
	'~': symBnot,
	'!': symNot,
}

const dualityChars = "=<>-&|!"

var dualchar2sym = map[string]symbolType{
	"==": symIsEqual,
	"!=": symIsDiff,
	"<=": symLE,
	">=": symGE,
	"->": symArrow,
	"<<": symShl,
	">>": symShr,
	"&&": symAnd,
	"||": symOr,
}

var keyword2sym = map[string]symbolType{
	"and": symAnd,
	"not": symNot,
	"or": symOr,

	"return": symKWReturn,
	"func": symKWFunc,
	"in": symKWIn,
	"where": symKWWhere,
	"basically": symKWBasically,
	"struct": symKWStruct,
	"false": symKWFalse,
	"true": symKWTrue,
	"if": symKWIf,
	"else": symKWElse,
	"import": symKWImport,

	"int": symTInt,
	"number": symTNumber,
	"string": symTString,
	"boolean": symTBool,
	"null": symKWNull,
}

type Symbol interface {
	GetType() symbolType
	String() string
}

// normal Symbol

type simpleSymbol struct {
	Symbol symbolType
}

func (t simpleSymbol) GetType() symbolType {
	return t.Symbol
}

func (t simpleSymbol) String() string {
	switch t.Symbol {
	case symParOpen: return "("
	case symParClose: return ")"
	case symSquareOpen: return "["
	case symSquareClose: return "]"
	case symCurlyOpen: return "{"
	case symCurlyClose: return "}"
	case symSemicolon: return ";"
	case symComma: return ","
	case symDot: return "."
	case symDiv: return "/"
	case symPlus: return "+"
	case symMinus: return "-"
	case symMul: return "*"
	case symSet: return "="
	case symIsEqual: return "=="
	case symIsDiff: return "!="
	case symLT: return "<"
	case symGT: return ">"
	case symLE: return "<="
	case symGE: return ">="
	case symBand: return "&"
	case symBor: return "|"
	case symBxor: return "^"
	case symBnot: return "~"
	case symArrow: return "->"
	case symShr: return ">>"
	case symShl: return "<<"
	case symAnd: return "and"
	case symNot: return "not"
	case symOr: return "or"

	case symKWIn: return "in"
	case symKWFunc: return "func"
	case symKWWhere: return "where"
	case symKWStruct: return "struct"
	case symKWBasically: return "basically"
	case symKWReturn: return "return"
	case symKWFalse: return "false"
	case symKWTrue: return "true"
	case symKWIf: return "if"
	case symKWElse: return "else"
	case symKWImport: return "import"
	case symKWNull: return "null"
	
	case symTString: return "typeString"
	case symTInt: return "typeInt"
	case symTNumber: return "typeNumber"
	case symTBool: return "typeBoolean"
	}
	return "undefined"
}

// string Symbol

type strSymbol struct {
	simpleSymbol
	Content string
}

func (t strSymbol) String() string {
	switch t.Symbol {
	case symString: return "STRING(" + t.Content + ")"
	case symDef: return "SYMBOL(" + t.Content + ")"
	}
	return "undefined"
}

// number Symbol

type numSymbol struct {
	simpleSymbol
	Content zzzNumber
}

func (t numSymbol) String() string {
	return "NUMBER(" + strconv.FormatFloat(float64(t.Content), 'f', -1, 64) + ")"
}

// int Symbol

// number Symbol

type intSymbol struct {
	simpleSymbol
	Content zzzInt
}

func (t intSymbol) String() string {
	return "INT(" + strconv.FormatInt(int64(t.Content), 10) + ")"
}

func DisplaySymbols(xs []Symbol) {
	for _, x := range xs {
		fmt.Printf("%s ", x.String())
	}
	fmt.Println()
}

func getSymbolToAppend(isNum, isFlt, isStr bool, buf string) (Symbol, error) {
	var t Symbol

	switch {
	case isNum && isFlt:
		n, err := strconv.ParseFloat(buf, 64)
		if err != nil {
			return t, err
		}
		t = numSymbol{simpleSymbol{symNumber}, zzzNumber(n)}
	case isNum && !isFlt:
		n, err := strconv.ParseInt(buf, 10, 64)
		if err != nil {
			return t, err
		}
		t = intSymbol{simpleSymbol{symInt}, zzzInt(n)}
	case isStr:
		t = strSymbol{simpleSymbol{symString}, buf}
	default:
		symbolType, ok := keyword2sym[buf]
		if ok {
			t = simpleSymbol{symbolType}
		} else {
			t = strSymbol{simpleSymbol{symDef}, buf}
		}
	}
	return t, nil
}

func Lex(stream string) ([]Symbol, error) {
	var (
		isFlt = false
		isNum = false
		isStr = false
		isSep = false
		inStr = false
		toPush Symbol
	)
	list := []Symbol{}
	buf := ""
	runes := []rune(stream)
	lenRune := len(runes)

	for i := 0; i < lenRune; i++ {
		toPush = simpleSymbol{symEnd}
		c := runes[i]
		isSep = !inStr
		switch c {
		case '"':
			inStr = !inStr
			isStr = !inStr
			isSep = inStr
		case '(', ')', '{', '}', '+', '-', '/', '*', '=', '<', '>', ';', '&', '|', '^', '~', ',', '!', '.', '[', ']':
			if inStr {
				buf += string(c)
			} else if strings.ContainsRune(dualityChars, c) && i < lenRune - 1 {
				if sym, ok := dualchar2sym[string(c) + string(runes[i + 1])]; ok {
					toPush = simpleSymbol{sym}
					i++
				} else {
					toPush = simpleSymbol{char2sym[c]}
				}
			} else {
				toPush = simpleSymbol{char2sym[c]}
			}
		case ' ', '\t', '\n':
			if inStr {
				buf += string(c)
			}
		default:
			isSep = false
			switch {
			case inStr || unicode.IsLetter(c):
				buf += string(c)
			case unicode.IsNumber(c):
				isNum = true
				buf += string(c)
			case c == '.' && !isFlt:
				isFlt = true
				isNum = true
				buf += string(c)
			default:
				return list, fmt.Errorf("invalid character: %c", c)
			}
		}
		if isSep && len(buf) != 0 {
			t, err := getSymbolToAppend(isNum, isFlt, isStr, buf)
			if err != nil {
				return list, err
			}
			list = append(list, t)
			buf = ""
			isNum = false
			isStr = false
			isFlt = false	
		}
		if toPush.GetType() != symEnd {
			list = append(list, toPush)
		}
	}
	if len(buf) != 0 {
		t, err := getSymbolToAppend(isNum, isFlt, isStr, buf)
		if err != nil {
			return list, err
		}
		list = append(list, t)
	}
	return list, nil
}