package zzz

import (
	"errors"
	"log"
	"strconv"
	"strings"
)

// usefull

type void struct{}

type zzzInt int64
type zzzNumber float64

type SymbolsPtr *[]Symbol

func forward(xs SymbolsPtr, off uint) bool {
	*xs = (*xs)[off:]
	return true
}

var builtinDefs = map[string]map[string]functionMeta{
	"io": {
		"print": {doesntCareEntry: true, Out: []zzzType{}},
	},
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

var setOfOpeUnaries = map[operatorType]void{opeMinus: {}, opeBitNot: {}}

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

var setOfTestUnaries = map[testType]void{testNot: {}}

// the type of a zzz value
type zzzType uint

const (
	typNull     zzzType = iota // null type
	typInt                     // an int (3)
	typNumber                  // a number (3.14)
	typString                  // a string ("hello")
	typBoolean                 // a boolean (true)
	typStruct                  // a struct (struct Element)
	typList                    // a list ([1, 2, 3])
	typUserdata                // an userdata
)

var convType2Str = map[zzzType]string{
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
	tokNull                       // null
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
	opePlus:   "add",
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
	GetTokenType() tokenType
	String() string
}

// different token implementations

type basicToken struct {
	tok tokenType
}

func (s *basicToken) GetTokenType() tokenType {
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
	Data zzzInt
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
	Data zzzNumber
}

func (s *numberToken) String() string {
	return "number <" + strconv.FormatFloat(float64(s.Data), 'f', -1, 64) + ">"
}

type typToken struct {
	basicToken
	Type zzzType
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

// stack trace

type StackTrace struct {
	funcNames []string
}

func (s *StackTrace) Push(fname string) {
	s.funcNames = append(s.funcNames, fname)
}

func (s StackTrace) String() string {
	buf := ""
	for n, x := range s.funcNames {
		if n != 0 {
			buf += " -> "
		}
		buf += x + "()"
	}
	return buf
}

func (s *StackTrace) Extend(other StackTrace) {
	s.funcNames = append(s.funcNames, other.funcNames...)
}

// node type & functions for nodes

type functionMeta struct {
	In              []zzzType
	Out             []zzzType
	doesntCareEntry bool
	isBuiltin       bool
	holder          *Node
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
		tok:      tok,
		meta:     nil,
		parent:   nil,
		Children: []*Node{},
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
	tokNull:   basictokenCallback(tokNull),

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
		return newNode(&opeToken{basicToken{tokOperator}, arg[0].(operatorType)})
	},
	tokTest: func(arg ...any) *Node {
		return newNode(&testToken{basicToken{tokTest}, arg[0].(testType)})
	},
	tokBoolean: func(arg ...any) *Node {
		return newNode(&boolToken{basicToken{tokBoolean}, arg[0].(bool)})
	},
	tokNumber: func(arg ...any) *Node {
		return newNode(&numberToken{basicToken{tokNumber}, arg[0].(zzzNumber)})
	},
	tokInt: func(arg ...any) *Node {
		return newNode(&intToken{basicToken{tokInt}, arg[0].(zzzInt)})
	},
	tokType: func(arg ...any) *Node {
		sk := arg[0].(zzzType)
		if sk == typStruct && len(arg) == 2 {
			return newNode(&typStructToken{typToken{basicToken{tokType}, sk}, arg[1].(string)})
		}
		return newNode(&typToken{basicToken{tokType}, sk})
	},
}

func (p *Node) replaceChild(from *Node, to *Node) *Node {
	for k, child := range p.Children {
		if child == from {
			p.Children[k] = to
			return from
		}
	}
	return nil
}

func (f *Node) pop(what *Node) *Node {
	if what == nil {
		if len(f.Children) == 0 {
			return nil
		}
		first := f.Children[0]
		f.Children = f.Children[1:]
		return first
	}
	for i, child := range f.Children {
		if child == what {
			f.Children = append(f.Children[:i], f.Children[i+1:]...)
			return child
		}
	}
	return nil
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
		if data.doesntCareEntry && data.isBuiltin {
			buf += "..."
		} else {
			for k, v := range data.In {
				if k != 0 {
					buf += ", "
				}
				buf += convType2Str[v]
			}
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
		buf += ";\n"
	}
	return buf
}

// parser

// type ::= int | number | boolean | string
func parseType(xs SymbolsPtr) (zzzType, bool) {
	var typ zzzType
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
		t     zzzType
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
//
// TODO: change this to use forward() maybe.
//
// TODO: for non stdlibs (for strings) check the path
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

	// check first node
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

	// check for the lib
	if (*sx)[inIndex].GetType() != symKWIn {
		return false
	}
	actualSym := (*sx)[inIndex+1].GetType()
	if actualSym != symString && actualSym != symDef {
		return false
	}
	libName, ok := (*sx)[inIndex+1].(strSymbol)
	if !ok {
		return false
	}
	libNode := nodeFactory[tokImport](libName.Content)
	for _, v := range list {
		bName := v.tok.(*strToken).Data
		bDef, ok := builtinDefs[libName.Content][bName]
		if ok {
			bDef.isBuiltin = true
			bDef.holder = libNode
			p.meta.FunctionMeta[bName] = bDef
		}
		libNode.append(v)
	}
	p.append(libNode)
	return forward(sx, inIndex+3)
}

// value ::= <def> | <number> | <int> | <string> | <null>
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
	case symKWNull:
		p.append(nodeFactory[tokNull]())
	default:
		return false // not a valid value
	}
	forward(xs, 1)
	return true
}

// call ::= <def> ( [<expr> ,] )
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
		if !child.parseExpr(xs) {
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

// parenexpr ::= ( <expr> )
func (p *Node) parseParenExpr(xs SymbolsPtr) bool {
	if (*xs)[0].GetType() != symParOpen {
		return false
	}
	forward(xs, 1)
	if !p.parseExpr(xs) {
		return false
	}
	if (*xs)[0].GetType() != symParClose {
		return false
	}
	return forward(xs, 1)
}

// term ::= <value> | <call> | <parenexpr>
func (p *Node) parseTerm(xs SymbolsPtr) bool {
	return (p.parseCall(xs) || p.parseParenExpr(xs) || p.parseValue(xs))
}

// unary ::= <test|operation> <expr>
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
		child.parseExpr(xs)
		p.append(child)
		return true

		// check for test
	} else if test, ok := convSym2test[(*xs)[0].GetType()]; ok {
		if _, ok := setOfTestUnaries[test]; !ok {
			return false // not a unary test
		}
		forward(xs, 1)
		child := nodeFactory[tokTest](test)
		child.parseExpr(xs)
		p.append(child)
		return true
	}
	return false
}

var val int = 0

// pair ::= <term> <test|operator> <expr>
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
	var prio uint
	if o, ok := convSym2ope[(*xs)[0].GetType()]; ok {
		child = nodeFactory[tokOperator](o)
		prio = opePrio[o]
	} else if t, ok := convSym2test[(*xs)[0].GetType()]; ok {
		child = nodeFactory[tokTest](t)
		prio = testPrio[t]
	} else {
		*xs = sav
		return false // not an operator or test
	}
	forward(xs, 1)
	child.append(dummy.Children[0])

	// get the priority
	parentPrio := -1
	if o, ok := p.tok.(*opeToken); ok {
		parentPrio = int(opePrio[o.Operator])
	} else if t, ok := p.tok.(*testToken); ok {
		parentPrio = int(testPrio[t.Test])
	}

	// add the child: child.parent = p
	p.append(child)

	// before parsing second child: reorder
	if parentPrio >= 0 && uint(parentPrio) > prio && p.parent != nil {
		println("==> SWAPPP <==")
		parent := child.parent // same as p
		pparent := parent.parent // same as p.parent
		x := child.pop(nil) // get the number back
		if tmp := parent.pop(child); tmp != child { // remove the child from parent
			log.Fatal("Expected it to be the child: parent =", parent.String())
		}
		parent.append(x) // re-add the object
		child.append(parent) // re-add the parent in the child
		pparent.replaceChild(parent, child) // change the parent's parent child
	}

	// then parse the second child
	if !child.parseExpr(xs) {
		*xs = sav
		return false // could not parse the second operand
	}
	return true
}

// expr ::= <unary> | <pair> | <term>
func (p *Node) parseExpr(xs SymbolsPtr) bool {
	return (p.parseUnary(xs) || p.parsePair(xs) || p.parseTerm(xs))
}

// return ::= return <expr> | return
func (p *Node) parseReturn(xs SymbolsPtr) bool {
	if (*xs)[0].GetType() != symKWReturn {
		return false
	}
	forward(xs, 1)
	node := nodeFactory[tokReturn]()
	if !node.parseExpr(xs) && (*xs)[0].GetType() != symSemicolon {
		return true
	}
	p.append(node)
	return true
}

// action ::= <expr> ; | <set> ; | <return> ; | <if>
func (p *Node) parseAction(xs SymbolsPtr) bool {
	if p.parseReturn(xs) || p.parseExpr(xs) {
		if len(*xs) == 0 || (*xs)[0].GetType() != symSemicolon {
			return false // missing semicolon
		}
		forward(xs, 1)
		return true
	}
	return false
}

// block ::= <action> | { [<action>] }
func (p *Node) parseBlock(xs SymbolsPtr) bool {
	if (*xs)[0].GetType() == symCurlyOpen {
		block := nodeFactory[tokBlock]()
		forward(xs, 1)
		if len(*xs) == 0 {
			return false // expected at least '}'
		}
		for (*xs)[0].GetType() != symCurlyClose {
			if !block.parseAction(xs) {
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
	return p.parseAction(xs)
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
	if len(*xs) < 6 { // smallest is "func a ( ) return ;" or "func a ( ) { }" which is 6
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

// object ::= <import> | <basically> | <function>
func (p *Node) parseObject(xs SymbolsPtr) bool {
	return p.parseImport(xs) || p.parseBasically(xs) || p.parseFunction(xs)
}

// General exported functions

func (n *Node) String() string {
	buf := "=== AST:\n"
	buf += n.leveledString(0) + "\n"
	buf += "=== METADATA:\n"
	if n.meta != nil {
		buf += n.showMetadata()
	} else {
		buf += "no metadata"
	}
	return buf
}

func Parse(symbols []Symbol) (*Node, error) {
	node := nodeFactory[tokProcedure]("filename")
	node.makeMeta()

	for len(symbols) > 0 {
		if !node.parseObject(&symbols) {
			return nil, errors.New("could not parse the file")
		}
	}
	println(node.String())
	return node, nil
}
