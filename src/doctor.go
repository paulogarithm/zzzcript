package zzz

import (
	"errors"
	"fmt"
)

var convTok2Typ = map[tokenType]zzzType{
	tokInt: typInt,
	tokNumber: typNumber,
	tokString: typString,
	tokBoolean: typBoolean,
	tokList: typList,
	tokNull: typNull,
}

func specialTypeResult(a, b zzzType) (zzzType, bool) {
	if a == typNumber && b == typInt {
		return a, true
	}
	if a == typString {
		return a, true
	}
	if (a == typNumber || a == typInt) && b == typBoolean {
		return a, true
	}
	return a, false
}

func checkTypeIntegrity(a, b zzzType) (zzzType, bool) {
	if a == b {
		return a, true
	}
	t, ok := specialTypeResult(a, b)
	if ok {
		return t, true
	}
	return specialTypeResult(b, a)
}

func canIAutoCastThisShit(from, desired zzzType) zzzType {
	if desired == typInt {
		if from == typNumber || from == typBoolean {
			return desired
		}
	} else if desired == typNumber {
		if from == typInt || from == typBoolean {
			return desired
		}
	} else if desired == typList && from == typString {
		return desired
	}
	return from
}

func checkThisReturnType(retType zzzType, ret *Node) error {
	return nil
}

func trackAndCheckReturn(retType zzzType, node *Node) error {
	if node.tok.GetToken() == tokReturn {
		println("CHECKING")
	}
	return nil
}

func checkIfYourFunctionIsFineOrShit(f *Node) error {
	// check some basic structure stuff
	if len(f.Children) != 2 {
		return errors.New("too few children in the call")
	}
	args := f.Children[0]
	if args.tok.GetToken() != tokArgs {
		return errors.New("expected call first child to be a token")
	}
	fname, ok := f.tok.(*strToken)
	if !ok {
		return errors.New("expected function to be a def (string)")
	}

	// parse the types of variables
	defTypes := map[string]zzzType{}
	if fname.Data != "main" {
		// check coherence arguments
		meta, ok := f.meta.FunctionMeta[fname.Data]
		if !ok {
			return errors.New("missing basically for your function " + fname.Data)
		}
		if len(args.Children) != len(meta.In) {
			return fmt.Errorf("%s expects %d arguments, but %d are given", fname.Data, len(meta.In), len(args.Children))
		}
		for index, arg := range args.Children {
			if arg.tok.GetToken() != tokDef {
				continue
			}
			defTypes[arg.tok.(*strToken).Data] = meta.In[index]
		}
	}

	t, ok := defTypes["n"]
	println(t == typInt, ok)
	return nil
}

func Doctor(ast *Node) error {
	for _, child := range ast.Children {
		if child.tok.GetToken() == tokFunction {
			err := checkIfYourFunctionIsFineOrShit(child)
			if err != nil {
				return err
			}
		}
	}
	return nil
}