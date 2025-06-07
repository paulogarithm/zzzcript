package zzz

import (
	"errors"
	"fmt"
)

// convert a token into a type (if applicable)
// int -> typInt
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

// get the final type of node
func getFinalNodeType(defmap map[string]zzzType, node *Node, meta *MetaData) (zzzType, error) {
	// TODO: implement the where thingy
	token := node.tok.GetToken()
	switch token {
		// check if it's the return node
	case tokReturn:
		if len(node.Children) == 0 {
			return typNull, nil
		}
		return getFinalNodeType(defmap, node.Children[0], meta)

		// check type when it's a lvalue
	case tokInt, tokNumber, tokString, tokBoolean, tokList, tokNull:
		t, ok := convTok2Typ[token]
		if !ok {
			return typInt, fmt.Errorf("could not convert type")
		}
		return t, nil
	
		// check when it's a variable/def
	case tokDef:
		def, ok := node.tok.(*strToken)
		if !ok {
			return typInt, fmt.Errorf("could not convert call to strToken (should never happend)")
		}
		if x, ok := defmap[def.Data]; !ok {
			return typInt, fmt.Errorf("variable not parsed")
		} else {
			return x, nil
		}

		// check type when it's call
	case tokCall:
		def, ok := node.tok.(*strToken)
		if !ok {
			return typInt, fmt.Errorf("could not convert call to strToken (should never happend)")
		}
		meta, ok := meta.FunctionMeta[def.Data]
		if !ok {
			return typInt, fmt.Errorf("function does not exist (missing basically maybe)")
		}
		if len(meta.Out) == 0 {
			return typNull, nil
		}
		return meta.Out[0], nil
	
		// check when it's an operator (+, -, >>, ...)
	case tokOperator:
		ope, ok := node.tok.(*opeToken)
		if !ok {
			return typInt, fmt.Errorf("could not convert call to opeToken (should never happend)")
		}
		if len(node.Children) < 2 {
			return typInt, fmt.Errorf("expected at least 2 children in operator")
		}
		if ope.Operator == opeOr || ope.Operator == opeAnd {
			return typBoolean, nil
		}
		a, err := getFinalNodeType(defmap, node.Children[0], meta)
		if err != nil {
			return a, err
		}
		b, err := getFinalNodeType(defmap, node.Children[1], meta)
		if err != nil {
			return b, err
		}
		res, isOK := checkTypeIntegrity(a, b)
		if !isOK {
			return res, fmt.Errorf("invalid operation between incompatible types: %s with %s", convType2Str[a], convType2Str[b])
		}
		return res, nil

		// check test (<, !, ==, ...)
	case tokTest:
		return typBoolean, nil
	}
	return typInt, fmt.Errorf("invalid node type here")
}

func trackAndCheckReturn(retType zzzType, defmap map[string]zzzType, node *Node, meta *MetaData) error {
	if node.tok.GetToken() == tokReturn {
		t, err := getFinalNodeType(defmap, node, meta)
		if err != nil {
			return err
		}
		t = canIAutoCastThisShit(t, retType)
		if t != retType {
			return fmt.Errorf("type mismatch: you return a %s, but your function expects a %s", convType2Str[t], convType2Str[retType])
		}
		return nil
	}
	for _, node := range node.Children {
		if err := trackAndCheckReturn(retType, defmap, node, meta); err != nil {
			return err
		}
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
	meta := functionMeta{In: []zzzType{}, Out: []zzzType{}}
	defTypes := map[string]zzzType{}
	if fname.Data != "main" {
		// check coherence arguments
		meta, ok = f.meta.FunctionMeta[fname.Data]
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

	// if there is an output, check for the return
	if len(meta.Out) != 0 {
		return trackAndCheckReturn(meta.Out[0], defTypes, f, f.meta)
	}
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