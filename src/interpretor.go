package zzz

import (
	"fmt"
	"strconv"
)

// usefull

type argument interface{}

func getTypeOfArgument(arg argument) zzzType {
	switch arg.(type) {
	case zzzInt:
		return typInt
	case zzzNumber:
		return typNumber
	case string:
		return typString
	case bool:
		return typBoolean
	default:
		return typNull
	}
}

type compareState uint

const (
	uncomparable = iota
	same
	smaller
	bigger
)

// compare a node (tokInt<3>) with an argument (3::any) ONLY if they are the same zzzType
func compareNodeAndArgument(tok Token, arg argument) compareState {
	switch getTypeOfArgument(arg) {
	case typInt:
		x := tok.(*intToken).Data
		y := arg.(zzzInt)
		if x == y {
			return same
		} else if x < y {
			return smaller
		} else if x > y {
			return bigger
		}
	case typNumber:
		x := tok.(*numberToken).Data
		y := arg.(zzzNumber)
		if x == y {
			return same
		} else if x < y {
			return smaller
		} else if x > y {
			return bigger
		}
	case typString:
		x := tok.(*strToken).Data
		y := arg.(string)
		if x == y {
			return same
		}
		return smaller
	case typBoolean:
		x := tok.(*boolToken).Data
		y := arg.(bool)
		if x == y {
			return same
		}
		return smaller
	case typNull:
		return same
	}
	return uncomparable
}

// builtins

var builtinFunctions = map[string]map[string]func([]argument) argument{
	"io": {
		"print": func(args []argument) argument {
			buf := ""
			for n, arg := range args {
				if n != 0 {
					buf += " "
				}
				switch x := arg.(type) {
				case zzzInt:
					buf += strconv.FormatInt(int64(x), 10)
				case zzzNumber:
					buf += strconv.FormatFloat(float64(x), 'f', -1, 64)
				case string:
					buf += x
				case bool:
					buf += strconv.FormatBool(x)
				case nil:
					buf += "null"
				default:
					buf += "<unknown>"
				}
			}
			fmt.Println(buf)
			return nil
		},
	},
}

// find and execute

// give me an argument in an <args> node, and ill tell ya if it's OK with the arg
func argumentMatch(node *Node, arg argument) bool {
	// check for litterals ( ex: 'func foo(42)...;' )
	nodeType, ok := convTok2Typ[node.tok.GetTokenType()]
	argType := getTypeOfArgument(arg)
	if ok {
		if nodeType == argType {
			return compareNodeAndArgument(node.tok, arg) == same
		}
		return false
	}

	// check for everything else ( ex: 'func foo(n)...; func foo(<3)...;' )
	switch node.tok.GetTokenType() {
	case tokTest:
		test := node.tok.(*testToken)
		if len(node.Children) == 0 {
			return false // should not be empty lol
		}
		nodeType, ok = convTok2Typ[node.Children[0].tok.GetTokenType()]
		if !ok || nodeType != argType {
			return false // should be the same type + be a litteral
		}
		res := compareNodeAndArgument(node.Children[0].tok, arg)
		switch test.Test {
		case testDiff, testNot: // '!=' and '!' and 'not' because im inclusive <3
			return res == smaller || res == bigger
		case testEq:
			return res == same
		case testGt:
			return res == bigger
		case testGte:
			return res == bigger || res == same
		case testLt:
			return res == smaller
		case testLte:
			return res == smaller || res == same
		}
	case tokDef:
		return true
	}

	// should not be anything else
	return false
}

// give me a node of <args> and ill tell ya if the function matches
func argumentsMatch(what *Node, args []argument) bool {
	// simple check
	if len(what.Children) != len(args) {
		return false
	}

	// now check each args, one mismatch = no
	for n, arg := range args {
		if !argumentMatch(what.Children[n], arg) {
			return false
		}
	}
	return true
}

// find function with name and arguments
func findFunction(ast *Node, name string, args []argument) *Node {
	for _, child := range ast.Children {
		def, ok := child.tok.(*strToken)
		if !ok || child.tok.GetTokenType() != tokFunction || def.Data != name || child.Children[0].tok.GetTokenType() != tokArgs {
			continue
		}
		if argumentsMatch(child.Children[0], args) {
			return child
		}
	}
	return nil
}

func callBuiltin(name string, meta functionMeta, args []argument) (argument, error) {
	if meta.holder == nil {
		return nil, fmt.Errorf("expected holder in the builtin")
	}
	libName, ok := meta.holder.tok.(*strToken)
	if !ok {
		return nil, fmt.Errorf("expected holder to have a string inside")
	}
	lib, ok := builtinFunctions[libName.Data]
	if !ok {
		return nil, fmt.Errorf("standard library doesnt exists: %s", libName.Data)
	}
	f, ok := lib[name]
	if !ok {
		return nil, fmt.Errorf("%s is not a valid import for %s", name, libName.Data)
	}
	return f(args), nil
}

var doPairOperation = map[zzzType]map[operatorType]func(x, y argument) argument{
	typInt: {
		opeAnd:    func(x, y argument) argument { return x.(zzzInt) != 0 && y.(zzzInt) != 0 },
		opeBitAnd: func(x, y argument) argument { return x.(zzzInt) & y.(zzzInt) },
		opeBitOr:  func(x, y argument) argument { return x.(zzzInt) | y.(zzzInt) },
		opeBitXor: func(x, y argument) argument { return x.(zzzInt) ^ y.(zzzInt) },
		opeDiv: func(x, y argument) argument {
			n := y.(zzzInt)
			if n == 0 {
				return nil
			}
			return x.(zzzInt) / n
		},
		opeMinus: func(x, y argument) argument { return x.(zzzInt) - y.(zzzInt) },
		opeMul:   func(x, y argument) argument { return x.(zzzInt) * y.(zzzInt) },
		opePlus:  func(x, y argument) argument { return x.(zzzInt) + y.(zzzInt) },
		opeOr:    func(x, y argument) argument { return x.(zzzInt) != 0 || y.(zzzInt) != 0 },
		opeShl:   func(x, y argument) argument { return x.(zzzInt) << y.(zzzInt) },
		opeShr:   func(x, y argument) argument { return x.(zzzInt) >> y.(zzzInt) },
	},
	typNumber: {
		opeAnd: func(x, y argument) argument { return x.(zzzNumber) != 0 && y.(zzzNumber) != 0 },
		opeDiv: func(x, y argument) argument {
			n := y.(zzzNumber)
			if n == 0 {
				return nil
			}
			return x.(zzzNumber) / n
		},
		opeMinus: func(x, y argument) argument { return x.(zzzNumber) - y.(zzzNumber) },
		opeMul:   func(x, y argument) argument { return x.(zzzNumber) * y.(zzzNumber) },
		opePlus:  func(x, y argument) argument { return x.(zzzNumber) + y.(zzzNumber) },
		opeOr:    func(x, y argument) argument { return x.(zzzNumber) != 0 || y.(zzzNumber) != 0 },
	},
	typString: {
		opePlus: func(x, y argument) argument { return x.(string) + y.(string) },
	},
	typBoolean: {
		opeAnd: func(x, y argument) argument { return x.(bool) && y.(bool) },
		opeOr:  func(x, y argument) argument { return x.(bool) || y.(bool) },
	},
}

var doUnaryOperation = map[zzzType]map[operatorType]func(x argument) argument{
	typInt: {
		opeBitNot: func(x argument) argument { return ^(x.(zzzInt)) },
		opeMinus: func(x argument) argument { return -(x.(zzzInt)) },
	},
	typNumber: {
		opeMinus: func(x argument) argument { return -(x.(zzzNumber)) },
	},
}

var doPairTest = map[zzzType]map[testType]func(x, y argument) argument{
	typNull: {
		testEq:   func(_, _ argument) argument { return true },
		testDiff: func(_, _ argument) argument { return false },
	},
	typInt: {
		testEq:   func(x, y argument) argument { return x.(zzzInt) == y.(zzzInt) },
		testDiff: func(x, y argument) argument { return x.(zzzInt) != y.(zzzInt) },
		testGt:   func(x, y argument) argument { return x.(zzzInt) > y.(zzzInt) },
		testGte:  func(x, y argument) argument { return x.(zzzInt) >= y.(zzzInt) },
		testLt:   func(x, y argument) argument { return x.(zzzInt) < y.(zzzInt) },
		testLte:  func(x, y argument) argument { return x.(zzzInt) <= y.(zzzInt) },
	},
	typNumber: {
		testEq:   func(x, y argument) argument { return x.(zzzNumber) == y.(zzzNumber) },
		testDiff: func(x, y argument) argument { return x.(zzzNumber) != y.(zzzNumber) },
		testGt:   func(x, y argument) argument { return x.(zzzNumber) > y.(zzzNumber) },
		testGte:  func(x, y argument) argument { return x.(zzzNumber) >= y.(zzzNumber) },
		testLt:   func(x, y argument) argument { return x.(zzzNumber) < y.(zzzNumber) },
		testLte:  func(x, y argument) argument { return x.(zzzNumber) <= y.(zzzNumber) },
	},
	typBoolean: {
		testEq:   func(x, y argument) argument { return x.(bool) == y.(bool) },
		testDiff: func(x, y argument) argument { return x.(bool) != y.(bool) },
	},
	typString: {
		testEq:   func(x, y argument) argument { return x.(string) == y.(string) },
		testDiff: func(x, y argument) argument { return x.(string) != y.(string) },
		testGt:   func(x, y argument) argument { return x.(string) > y.(string) },
		testGte:  func(x, y argument) argument { return x.(string) >= y.(string) },
		testLt:   func(x, y argument) argument { return x.(string) < y.(string) },
		testLte:  func(x, y argument) argument { return x.(string) <= y.(string) },
	},
}

var doUnaryTest = map[zzzType]map[testType]func(x argument) argument{
	typNull: {
		testNot: func(_ argument) argument { return true },
	},
	typInt: {
		testNot: func(x argument) argument { return x.(zzzInt) == 0 },
	},
	typNumber: {
		testNot: func(x argument) argument { return x.(zzzNumber) == 0 },
	},
	typBoolean: {
		testNot: func(x argument) argument { return !x.(bool) },
	},
}

func calculusPair(tok Token, x, y argument) (argument, error) {
	argType := getTypeOfArgument(x)
	if tok.GetTokenType() == tokOperator {
		typ, ok := doPairOperation[argType]
		if !ok {
			return nil, fmt.Errorf("could not do any pair operation with %s", convType2Str[argType])
		}
		f, ok := typ[tok.(*opeToken).Operator]
		if !ok {
			return nil, fmt.Errorf("could not do pair operation %s with %s", tok.String(), convType2Str[argType])
		}
		return f(x, y), nil

	} else if tok.GetTokenType() == tokTest {
		typ, ok := doPairTest[argType]
		if !ok {
			return nil, fmt.Errorf("could not do any pair test with %s", convType2Str[argType])
		}
		f, ok := typ[tok.(*testToken).Test]
		if !ok {
			return nil, fmt.Errorf("could not do pair test %s with %s", tok.String(), convType2Str[argType])
		}
		return f(x, y), nil
	}
	return nil, fmt.Errorf("invalid token: %s", tok.String())
}

func calculusUnary(tok Token, x argument) (argument, error) {
	argType := getTypeOfArgument(x)
	if tok.GetTokenType() == tokOperator {
		typ, ok := doUnaryOperation[argType]
		if !ok {
			return nil, fmt.Errorf("could not do any unary operation with %s", convType2Str[argType])
		}
		f, ok := typ[tok.(*opeToken).Operator]
		if !ok {
			return nil, fmt.Errorf("could not do unary operation %s with %s", tok.String(), convType2Str[argType])
		}
		return f(x), nil
	} else if tok.GetTokenType() == tokTest {
		typ, ok := doUnaryTest[argType]
		if !ok {
			return nil, fmt.Errorf("could not do any unary test with %s", convType2Str[argType])
		}
		f, ok := typ[tok.(*testToken).Test]
		if !ok {
			return nil, fmt.Errorf("could not do unary test %s with %s", tok.String(), convType2Str[argType])
		}
		return f(x), nil
	}
	return nil, fmt.Errorf("invalid token: %s", tok.String())
}

// check if the argument is a return argument
func executeExpr(ast, f *Node, defmap *map[string]argument) (argument, error) {
	switch f.tok.GetTokenType() {
	case tokReturn:
		return executeExpr(ast, f.Children[0], defmap)
	case tokInt:
		return f.tok.(*intToken).Data, nil
	case tokNumber:
		return f.tok.(*numberToken).Data, nil
	case tokString:
		return f.tok.(*strToken).Data, nil
	case tokBoolean:
		return f.tok.(*boolToken).Data, nil
	case tokNull:
		return nil, nil

	case tokCall:
		fData, ok := f.tok.(*strToken)
		if !ok {
			return nil, fmt.Errorf("could not get the name of the function you call")
		}
		arglist := []argument{}
		for _, argNode := range f.Children {
			x, err := executeExpr(ast, argNode, defmap)
			if err != nil {
				return nil, err
			}
			arglist = append(arglist, x)
		}
		fMeta, ok := ast.meta.FunctionMeta[fData.Data]
		if !ok {
			return nil, fmt.Errorf("could not call '%s': function doesnt exist", fData.Data)
		}
		if fMeta.isBuiltin {
			return callBuiltin(fData.Data, fMeta, arglist)
		}
		toCall := findFunction(ast, fData.Data, arglist)
		if toCall == nil {
			return nil, fmt.Errorf("could not call '%s': invalid arguments, not guarded", fData.Data)
		}
		return callFunction(ast, toCall, arglist)

	case tokDef:
		defName, ok := f.tok.(*strToken)
		if !ok {
			return nil, fmt.Errorf("could not get the name of the variable you try to get")
		}
		def, ok := (*defmap)[defName.Data]
		if !ok {
			return nil, fmt.Errorf("undefined variable %s", defName.Data)
		}
		return def, nil

	case tokOperator, tokTest:
		a, err := executeExpr(ast, f.Children[0], defmap)
		if err != nil {
			return nil, err
		}
		if len(f.Children) == 1 {
			return calculusUnary(f.tok, a)
		}
		b, err := executeExpr(ast, f.Children[1], defmap)
		if err != nil {
			return nil, err
		}
		return calculusPair(f.tok, a, b)

	case tokFunction, tokImport, tokArgs, tokBlock, tokProcedure:
		return nil, fmt.Errorf("unexpected token '%s' in your function", f.tok.String())
	}
	return nil, fmt.Errorf("could not parse token '%s'", f.tok.String())
}

func callFunction(ast, f *Node, args []argument) (argument, error) {
	// check for count
	if len(f.Children) != 2 {
		return nil, fmt.Errorf("bad function format: expected 2 children")
	}

	// make a map from the args children
	defmap := map[string]argument{}
	for key, arg := range f.Children[0].Children {
		if arg.tok.GetTokenType() != tokDef {
			continue
		}
		x, ok := arg.tok.(*strToken)
		if !ok {
			return nil, fmt.Errorf("could not get the string of the def token")
		}
		defmap[x.Data] = args[key]
	}

	if f.Children[1].tok.GetTokenType() == tokBlock {
		for _, expr := range f.Children[1].Children {
			res, err := executeExpr(ast, expr, &defmap)
			if err != nil {
				return nil, err
			}
			if res != nil {
				return res, nil
			}
		}
	} else {
		res, err := executeExpr(ast, f.Children[1], &defmap)
		if err != nil {
			return nil, err
		}
		if res != nil {
			return res, nil
		}
	}
	return nil, nil // return nil
}

func Interpret(ast *Node) error {
	mNode := findFunction(ast, "main", []argument{})
	if mNode == nil {
		return nil // no main, no execution
	}
	// return nil

	// TODO: maybe check for the int it returns to exit ?
	_, err := callFunction(ast, mNode, []argument{})
	return err
}
