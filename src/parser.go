package zzz

import (
	"errors"
	"strconv"
	"strings"
)

// usefull

type void struct{}

type SymbolsPtr *[]Symbol

func forward(xs SymbolsPtr, off uint) bool {
	*xs = (*xs)[off:]
	return true
}

// operator

type operatorType uint

const (
	opePlus   operatorType = iota // +
	opeMinus                      // -
	opeMul                        // *
	opeDiv                        // /
	opeShl                        // <<
	opeShr                        // >>
	opeBitAnd                     // &
	opeBitOr                      // |
	opeBitXor                     // ^
	opeBitNot                     // ~
	opeAnd                        // &&
	opeOr                         // ||
)

var convSym2ope = map[symbolType]operatorType{
	symPlus:  opePlus,
	symMinus: opeMinus,
	symMul:   opeMul,
	symDiv:   opeDiv,
	symShl:   opeShl,
	symShr:   opeShr,
	symBand:  opeBitAnd,
	symBor:   opeBitOr,
	symBxor:  opeBitXor,
	symBnot:  opeBitNot,
	symAnd:   opeAnd,
	symOr:    opeOr,
}

var opePrio = map[operatorType]uint{
	opeBitNot: 11,
	opeDiv:    10,
	opeMul:    10,
	opePlus:   9,
	opeMinus:  9,
	opeShl:    8,
	opeShr:    8,
	opeBitAnd: 7,
	opeBitXor: 6,
	opeBitOr:  5,
	opeAnd:    1,
	opeOr:     0,
}

var setOfOpeUnaries = map[operatorType]void{
	opeMinus:  void{},
	opeBitNot: void{},
}

// test

type testType uint

const (
	testDiff testType = iota // !=
	testEq                   // ==
	testLt                   // <
	testGt                   // >
	testLte                  // <=
	testGte                  // >=
	testNot                  // !
)

var convSym2test = map[symbolType]testType{
	symIsDiff:  testDiff,
	symIsEqual: testEq,
	symLT:      testLt,
	symGT:      testGt,
	symLE:      testLte,
	symGE:      testGte,
	symNot:     testNot,
}

var testPrio = map[testType]uint{
	testLt:   4,
	testGt:   4,
	testGte:  4,
	testLte:  4,
	testEq:   3,
	testDiff: 3,
	testNot:  2,
}

var setOfTestUnaries = map[testType]void{
	testNot: void{},
}

type typType uint

const (
	typNull     typType = iota // null type
	typInt                     // an int (3)
	typNumber                  // a number (3.14)
	typString                  // a string ("hello")
	typBoolean                 // a boolean (true)
	typStruct                  // a struct (struct Element)
	typList                    // a list ([1, 2, 3])
	typUserdata                // an userdata
)

var convType2Str = map[typType]string{
	typNull:     "null",
	typInt:      "int",
	typNumber:   "number",
	typString:   "string",
	typBoolean:  "bool",
	typStruct:   "struct",
	typList:     "list",
	typUserdata: "userdata",
}

// tokens

// the enum token type
type tokenType uint

const (
	tokProcedure tokenType = iota // the full code container
	tokBlock                      // block is a set of multiple instructions
	tokFunction                   // when a function is created (first objects are args, second is code)
	tokArgs                       // a range of multiple elemnts for a function definition
	tokImport                     // when something is being imported
	tokReturn                     // a return statement
	tokSet                        // when a variable is set
	tokIf                         // an if block (first object is condition, second is statement, third is else)
	tokCall                       // a function call (print())
	tokOperator                   // when an operation is made (1 + 1)
	tokTest                       // when a test is made (3 < 3)
	tokDef                        // a definition (hello)
	tokType                       // a type (true),
	tokString                     // a string
	tokNumber                     // a number
	tokInt                        // an int
	tokBoolean                    // a boolean
	tokList                       // a list
)

// check for a no arg token
var convTok2Str = map[tokenType]string{
	tokIf:     "if",
	tokBlock:  "block",
	tokReturn: "return",
	tokArgs:   "args",
}

// check for a token that holds a string
var convStrtok2Str = map[tokenType]string{
	tokFunction:  "function",
	tokProcedure: "procedure",
	tokImport:    "import",
	tokSet:       "set",
	tokCall:      "call",
	tokDef:       "defined",
}

var convOperator2Str = map[operatorType]string{
	opeBitAnd: "band",
	opeBitOr:  "box",
	opeBitNot: "bnot",
	opeDiv:    "div",
	opeBitXor: "bxor",
	opeMinus:  "minus",
	opeMul:    "mul",
	opePlus:   "plus",
	opeShl:    "shl",
	opeShr:    "shr",
	opeAnd:    "and",
	opeOr:     "or",
}

var convTest2Str = map[testType]string{
	testDiff: "diff",
	testEq:   "eq",
	testGt:   "gt",
	testGte:  "gte",
	testLt:   "lt",
	testLte:  "lte",
	testNot:  "not",
}

type Token interface {
	GetToken() tokenType
	String() string
}

// different token implementations

type basicToken struct {
	tok tokenType
}

func (s *basicToken) GetToken() tokenType {
	return s.tok
}
func (s *basicToken) String() string {
	if n, ok := convTok2Str[s.tok]; ok {
		return n
	}
	return "undefined"
}

type opeToken struct {
	basicToken
	Operator operatorType
}

func (s *opeToken) String() string {
	return "operator <" + convOperator2Str[s.Operator] + ">"
}

type testToken struct {
	basicToken
	Test testType
}

func (s *testToken) String() string {
	return "test <" + convTest2Str[s.Test] + ">"
}

type intToken struct {
	basicToken
	Data int64
}

func (s *intToken) String() string {
	return "int <" + strconv.Itoa(int(s.Data)) + ">"
}

type strToken struct {
	basicToken
	Data string
}

func (s *strToken) String() string {
	what, ok := convStrtok2Str[s.tok]
	if !ok {
		what = "string"
	}
	return what + " <" + s.Data + ">"
}

type boolToken struct {
	basicToken
	Data bool
}

func (s *boolToken) String() string {
	return "boolean <" + strconv.FormatBool(s.Data) + ">"
}

type numberToken struct {
	basicToken
	Data float64
}

func (s *numberToken) String() string {
	return "number <" + strconv.FormatFloat(s.Data, 'f', -1, 64) + ">"
}

type typToken struct {
	basicToken
	Type typType
}

func (s *typToken) String() string {
	return "type <" + convType2Str[s.Type] + ">"
}

type typStructToken struct {
	typToken
	StructName string
}

func (s *typStructToken) String() string {
	return "type <struct " + s.StructName + ">"
}

// node type & functions for nodes

type functionMeta struct {
	In  []typType
	Out []typType
}

type MetaData struct {
	FunctionMeta map[string]functionMeta
	KStr         []string
	KNums        []float64
}

type Node struct {
	meta     *MetaData
	parent   *Node
	tok      Token
	Children []*Node
}

func newNode(tok Token) *Node {
	return &Node{
		tok:    tok,
		meta:   nil,
		parent: nil,
	}
}

func basictokenCallback(st tokenType) func(...any) *Node {
	return func(...any) *Node {
		return newNode(&basicToken{st})
	}
}

func strTokenCallback(st tokenType) func(...any) *Node {
	return func(args ...any) *Node {
		return newNode(&strToken{basicToken{st}, args[0].(string)})
	}
}

var nodeFactory = map[tokenType]func(...any) *Node{
	// the ones who need nothing
	tokBlock:  basictokenCallback(tokBlock),
	tokArgs:   basictokenCallback(tokArgs),
	tokReturn: basictokenCallback(tokReturn),
	tokIf:     basictokenCallback(tokIf),

	// the ones who need a string
	tokProcedure: strTokenCallback(tokProcedure),
	tokFunction:  strTokenCallback(tokFunction),
	tokImport:    strTokenCallback(tokImport),
	tokSet:       strTokenCallback(tokSet),
	tokCall:      strTokenCallback(tokCall),
	tokDef:       strTokenCallback(tokDef),
	tokString:    strTokenCallback(tokString),

	// the weirdos
	tokOperator: func(arg ...any) *Node {
		return newNode(&opeToken{basicToken{tokBoolean}, arg[0].(operatorType)})
	},
	tokTest: func(arg ...any) *Node {
		return newNode(&testToken{basicToken{tokBoolean}, arg[0].(testType)})
	},
	tokBoolean: func(arg ...any) *Node {
		return newNode(&boolToken{basicToken{tokBoolean}, arg[0].(bool)})
	},
	tokNumber: func(arg ...any) *Node {
		return newNode(&numberToken{basicToken{tokNumber}, arg[0].(float64)})
	},
	tokInt: func(arg ...any) *Node {
		return newNode(&intToken{basicToken{tokInt}, arg[0].(int64)})
	},
	tokType: func(arg ...any) *Node {
		sk := arg[0].(typType)
		if sk == typStruct && len(arg) == 2 {
			return newNode(&typStructToken{typToken{basicToken{tokType}, sk}, arg[1].(string)})
		}
		return newNode(&typToken{basicToken{tokType}, sk})
	},
}

func (p *Node) append(child *Node) {
	child.parent = p
	child.meta = p.meta
	p.Children = append(p.Children, child)
}

func (p *Node) makeMeta() {
	p.meta = &MetaData{
		FunctionMeta: map[string]functionMeta{},
	}
}

func (n *Node) leveledString(level int) string {
	buf := strings.Repeat(" ", level*4) + n.tok.String()
	if len(n.Children) == 0 {
		return buf + "\n"
	}
	buf += ":\n"
	for _, child := range n.Children {
		buf += child.leveledString(level + 1)
	}
	return buf
}

func (n *Node) showMetadata() string {
	var buf string
	for def, data := range n.meta.FunctionMeta {
		buf += def + "("
		for k, v := range data.In {
			if k != 0 {
				buf += ", "
			}
			buf += convType2Str[v]
		}
		buf += ")"
		if len(data.Out) != 0 {
			buf += " -> "
			for k, v := range data.Out {
				if k != 0 {
					buf += ", "
				}
				buf += convType2Str[v]
			}
		}
		buf += "\n"
	}
	return buf
}

// parser

// type ::= int | number | boolean | string
func parseType(xs SymbolsPtr) (typType, bool) {
	var typ typType
	switch (*xs)[0].GetType() {
	case symTBool:
		typ = typBoolean
	case symTInt:
		typ = typInt
	case symTNumber:
		typ = typNumber
	case symTString:
		typ = typString
	default:
		return typ, false // TODO: check for struct
	}
	return typ, forward(xs, 1)
}

// typedef ::= <type> <def>
func (p *Node) parseTypedef() bool {
	return false
}

// struct ::= struct <def> -> { <typedef> [, <typedelem>] }
func (p *Node) parseStruct(SymbolsPtr) bool {
	return false
}

// functype ::= <def> ( <type> [, <type>] ) (-> <type>)?
func (p *Node) parseFuncType(xs SymbolsPtr) bool {
	if (*xs)[0].GetType() != symDef || (*xs)[1].GetType() != symParOpen {
		return false
	}
	def := (*xs)[0].(strSymbol).Content
	forward(xs, 2)

	// check for no params function
	var (
		fmeta = functionMeta{}
		ok    bool
		t     typType
	)
	if (*xs)[0].GetType() == symParClose {
		goto checkRet
	}

	// check for the in types
	t, ok = parseType(xs)
	if !ok {
		return false
	}

	fmeta.In = append(fmeta.In, t)
	for (*xs)[0].GetType() == symComma {
		forward(xs, 1)
		t, ok = parseType(xs)
		if !ok {
			return false
		}
		fmeta.In = append(fmeta.In, t)
	}
	if (*xs)[0].GetType() != symParClose {
		return false
	}

checkRet:
	// check the return
	forward(xs, 1)
	if (*xs)[0].GetType() != symArrow {
		goto success
	}
	forward(xs, 1)
	t, ok = parseType(xs) // TODO: return tuples
	if !ok {
		return false
	}
	fmeta.Out = append(fmeta.Out, t)

success:
	p.meta.FunctionMeta[def] = fmeta
	return true
}

// basically ::= basically (<struct> | <functype>) ;
func (p *Node) parseBasically(xs SymbolsPtr) bool {
	if (*xs)[0].GetType() != symKWBasically {
		return false
	}
	forward(xs, 1)
	if !p.parseStruct(xs) && !p.parseFuncType(xs) {
		return false
	}
	if (*xs)[0].GetType() == symSemicolon {
		return forward(xs, 1)
	}
	return false
}

// import ::= import <def> [, <def>]? in (<def> | <string>) ;
func (p *Node) parseImport(sx SymbolsPtr) bool {
	// check for the requierments
	sxl := uint(len(*sx))
	if sxl < 5 {
		return false
	}
	var (
		inIndex uint = 2
		list         = []*Node{}
	)
	if (*sx)[0].GetType() != symKWImport ||
		(*sx)[1].GetType() != symDef {
		return false
	}
	x, ok := (*sx)[1].(strSymbol)
	if !ok {
		return false
	}
	list = append(list, nodeFactory[tokDef](x.Content))

	// get all the defines
	for inIndex+1 < sxl &&
		(*sx)[inIndex].GetType() != symKWIn &&
		((*sx)[inIndex].GetType() == symComma && (*sx)[inIndex+1].GetType() == symDef) {
		x, ok = (*sx)[inIndex+1].(strSymbol)
		if !ok {
			return false
		}
		list = append(list, nodeFactory[tokDef](x.Content))
		inIndex += 2
	}

	// check for where it is
	if (*sx)[inIndex].GetType() != symKWIn {
		return false
	}
	actualSym := (*sx)[inIndex+1].GetType()
	if actualSym != symString && actualSym != symDef {
		return false
	}
	x, ok = (*sx)[inIndex+1].(strSymbol)
	if !ok {
		return false
	}
	node := nodeFactory[tokImport](x.Content)
	for _, v := range list {
		node.append(v)
	}
	p.append(node)
	return forward(sx, inIndex+3)
}

// value ::= <def> | <number> | <int> | <string>
func (p *Node) parseValue(xs SymbolsPtr) bool {
	val := (*xs)[0]
	switch val.GetType() {
	case symDef:
		p.append(nodeFactory[tokDef](val.(strSymbol).Content))
	case symNumber:
		p.append(nodeFactory[tokNumber](val.(numSymbol).Content))
	case symInt:
		p.append(nodeFactory[tokInt](val.(intSymbol).Content))
	case symString:
		p.append(nodeFactory[tokString](val.(strSymbol).Content))
	case symKWTrue, symKWFalse:
		p.append(nodeFactory[tokBoolean](val.GetType() == symKWTrue))
	default:
		return false // not a valid value
	}
	forward(xs, 1)
	return true
}

// call ::= <def> ( [<leaf> ,] )
func (p *Node) parseCall(xs SymbolsPtr) bool {
	xsl := uint(len(*xs))
	if xsl < 3 {
		return false // invalid way to make call
	}
	if (*xs)[0].GetType() != symDef || (*xs)[1].GetType() != symParOpen {
		return false // expected def or '('.
	}

	
	callFunction, ok := (*xs)[0].(strSymbol)
	if !ok {
		return false // couldnt parse strSymbol from definition
	}
	forward(xs, 2)
	child := nodeFactory[tokCall](callFunction.Content)
	for (*xs)[0].GetType() != symParClose {
		if uint(len(*xs)) < 2 {
			return false
		}
		if !child.parseLeaf(xs) {
			return false // could not parse node
		}
		if (*xs)[0].GetType() == symParClose {
			break
		}
		if (*xs)[0].GetType() != symComma {
			return false
		}
		forward(xs, 1)
	}
	forward(xs, 1)
	p.append(child)
	return true
}

// parenexpr ::= ( <leaf> )
func (p *Node) parseParenExpr(xs SymbolsPtr) bool {
	return false
}

// term ::= <value> | <call> | <parenexpr>
func (p *Node) parseTerm(xs SymbolsPtr) bool {
	if !p.parseCall(xs) && !p.parseParenExpr(xs) && !p.parseValue(xs) {
		return false
	}
	return true
}

// unary ::= <test|operation> <leaf>
func (p *Node) parseUnary(xs SymbolsPtr) bool {
	if len(*xs) < 2 {
		return false // not enough symbols
	}

	// check for operation
	if operator, ok := convSym2ope[(*xs)[0].GetType()]; ok {
		if _, ok := setOfOpeUnaries[operator]; !ok {
			return false // not a unary operator
		}
		forward(xs, 1)
		child := nodeFactory[tokOperator](operator)
		child.parseLeaf(xs)
		p.append(child)
		return true

		// check for test
	} else if test, ok := convSym2test[(*xs)[0].GetType()]; ok {
		if _, ok := setOfTestUnaries[test]; !ok {
			return false // not a unary test
		}
		forward(xs, 1)
		child := nodeFactory[tokTest](test)
		child.parseLeaf(xs)
		p.append(child)
		return true
	}
	return false
}

// pair ::= <term> <test|operator> <leaf>
func (p *Node) parsePair(xs SymbolsPtr) bool {
	if len(*xs) < 3 {
		return false // not enough symbols
	}

	// create a dummy node to parse the first term (auto forward)
	sav := make([]Symbol, len(*xs))
	copy(sav, *xs)
	dummy := nodeFactory[tokProcedure]("")
	if !dummy.parseTerm(xs) {
		return false // could not parse term in the first operand
	}
	if len(*xs) < 2 {
		*xs = sav
		return false
	}

	// then create the real child and copy the first child of dummy in real
	var child *Node
	if o, ok := convSym2ope[(*xs)[0].GetType()]; ok {
		child = nodeFactory[tokOperator](o)
	} else if t, ok := convSym2test[(*xs)[0].GetType()]; ok {
		child = nodeFactory[tokTest](t)
	} else {
		*xs = sav
		return false // not an operator or test
	}
	forward(xs, 1)
	child.Children = append(child.Children, dummy.Children[0])

	// then parse the second child
	if !child.parseLeaf(xs) {
		*xs = sav
		return false // could not parse the second operand
	}
	p.append(child)
	return true
}

// leaf ::= <unary> | <pair> | <term>
func (p *Node) parseLeaf(xs SymbolsPtr) bool {
	if !p.parseUnary(xs) && !p.parsePair(xs) && !p.parseTerm(xs) {
		return false // cant parse the leaf
	}
	return true
}

// return ::= return <leaf>
func (p *Node) parseReturn(xs SymbolsPtr) bool {
	if (*xs)[0].GetType() != symKWReturn {
		return false
	}
	forward(xs, 1)
	node := nodeFactory[tokReturn]()
	if !node.parseLeaf(xs) {
		return false
	}
	p.append(node)
	return true
}

// expr ::= <leaf> ; | <set> ; | <return> ; | <if>
func (p *Node) parseExpr(xs SymbolsPtr) bool {
	if p.parseReturn(xs) {
		if len(*xs) == 0 || (*xs)[0].GetType() != symSemicolon {
			return false // missing semicolon
		}
		forward(xs, 1)
		return true
	}
	return false
}

// block ::= <expr> | { [<expr>] }
func (p *Node) parseBlock(xs SymbolsPtr) bool {
	if (*xs)[0].GetType() == symCurlyOpen {
		block := nodeFactory[tokBlock]()
		forward(xs, 1)
		if len(*xs) == 0 {
			return false // expected at least '}'
		}
		for (*xs)[0].GetType() != symCurlyClose {
			if !block.parseExpr(xs) {
				return false
			}
			if len(*xs) == 0 {
				return false // expected end of block '}'
			}
		}
		forward(xs, 1)
		p.append(block)
		return true
	}
	return p.parseExpr(xs)
}

// arg ::= < <value> | <testsym> <value !def> >
func (p *Node) parseArg(xs SymbolsPtr) bool {
	// check if its a test sym
	test, ok := convSym2test[(*xs)[0].GetType()]
	if ok {
		forward(xs, 1)
		testNode := nodeFactory[tokTest](test)
		if (*xs)[0].GetType() == symDef {
			return false // definition not accepted when pattern matching test
		}
		if !testNode.parseValue(xs) {
			return false // could not parse value
		}
		p.append(testNode)

		// or check if it's a value
	} else if !p.parseValue(xs) {
		return false
	}
	return true
}

// function ::= func <def> ( [<arg> ,] ) <block>
func (p *Node) parseFunction(xs SymbolsPtr) bool {
	if len(*xs) < 6 { // smallest is "func a ( ) { }" which is 6
		return false
	}
	if (*xs)[0].GetType() != symKWFunc || (*xs)[1].GetType() != symDef || (*xs)[2].GetType() != symParOpen {
		return false
	}

	// create the function node
	defName, ok := (*xs)[1].(strSymbol)
	if !ok {
		return false // def is not a string
	}
	funcNode := nodeFactory[tokFunction](defName.Content)
	forward(xs, 3)

	// parse the args
	args := nodeFactory[tokArgs]()
	for (*xs)[0].GetType() != symParClose {
		if !args.parseArg(xs) {
			return false
		}
		if (*xs)[0].GetType() == symParClose {
			break
		}
		if (*xs)[0].GetType() != symComma {
			return false // expected comma or end parenthesis
		}
		forward(xs, 1)
 	}
	forward(xs, 1)
	funcNode.append(args)

	// then parse block
	if !funcNode.parseBlock(xs) {
		return false
	}
	p.append(funcNode)
	return true
}

// action ::= <import> | <basically> | <function>
func (p *Node) parseAction(xs SymbolsPtr) bool {
	return p.parseImport(xs) || p.parseBasically(xs) || p.parseFunction(xs)
}

// General exported functions

func (n *Node) String() string {
	buf := "=== AST:\n"
	buf += n.leveledString(0) + "\n"
	buf += "=== METADATA:\n"
	buf += n.showMetadata()
	return buf
}

func Parse(symbols []Symbol) (*Node, error) {
	node := nodeFactory[tokProcedure]("filename")
	node.makeMeta()

	for len(symbols) > 0 {
		if !node.parseAction(&symbols) {
			return nil, errors.New("invalid")
		}
	}
	return node, nil
}
