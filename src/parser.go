package zzz

import (
	"fmt"
	"log"
	"strconv"
	"strings"
)

// usefull

type void struct{}

type zzzInt int64
type zzzNumber float64

type SymbolsPtr *[]Symbol

func forward(xs SymbolsPtr, off uint) error {
	*xs = (*xs)[off:]
	return nil
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
	typNull      zzzType = iota // null type
	typInt                      // an int (3)
	typNumber                   // a number (3.14)
	typCharacter                // a character ('h')
	typString                   // a string ("hello")
	typBoolean                  // a boolean (true)
	typStruct                   // a struct (struct Element)
	typList                     // a list ([1, 2, 3])
	typUserdata                 // an userdata
)

var convType2Str = map[zzzType]string{
	typNull:      "null",
	typInt:       "int",
	typCharacter: "char",
	typNumber:    "number",
	typString:    "string",
	typBoolean:   "bool",
	typStruct:    "struct",
	typList:      "list",
	typUserdata:  "userdata",
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
			to.parent = p
			p.Children[k] = to
			from.parent = nil
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

// type ::= int | number | boolean | string | null
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
	case symKWNull:
		typ = typNull
	default:
		return typ, false
	}
	forward(xs, 1)
	return typ, true
}

// struct ::= struct <def> -> { <typedef> [, <typedelem>] }
func (p *Node) parseStruct(SymbolsPtr) error {
	return fmt.Errorf("<wip>")
}

// functype ::= <def> ( <type> [, <type>] ) (-> <type>)?
func (p *Node) parseFuncType(xs SymbolsPtr) error {
	if (*xs)[0].GetType() != symDef || (*xs)[1].GetType() != symParOpen {
		return fmt.Errorf("expected <def> ( to start, got '%s %s'", (*xs)[0].String(), (*xs)[1].String())
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
		return fmt.Errorf("expected type (eg: int, string, ...) got '%s'", (*xs)[0].String())
	}

	fmeta.In = append(fmeta.In, t)
	for (*xs)[0].GetType() == symComma {
		forward(xs, 1)
		t, ok = parseType(xs)
		if !ok {
			return fmt.Errorf("expected type (eg: int, string, ...) got '%s'", (*xs)[0].String())
		}
		fmeta.In = append(fmeta.In, t)
	}
	if (*xs)[0].GetType() != symParClose {
		return fmt.Errorf("expected end of function type with ')', got '%s'", (*xs)[0].String())
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
		return fmt.Errorf("expected type for return (eg: int, string, ...) got '%s'", (*xs)[0].String())
	}
	fmeta.Out = append(fmeta.Out, t)

success:
	p.meta.FunctionMeta[def] = fmeta
	return nil
}

// basically ::= basically (<struct> | <functype>) ;
func (p *Node) parseBasically(xs SymbolsPtr) error {
	if (*xs)[0].GetType() != symKWBasically {
		return fmt.Errorf("basically parsing should start with a basically keyword")
	}
	forward(xs, 1)
	var errA, errB error
	errA = p.parseStruct(xs)
	if errA != nil {
		errB = p.parseFuncType(xs)
	}
	if errB != nil {
		return fmt.Errorf("in basically, could not compute -> not a struct (%s), nor a func (%s)", errA.Error(), errB.Error())
	}
	if (*xs)[0].GetType() == symSemicolon {
		return forward(xs, 1)
	}
	return nil
}

// import ::= import <def> [, <def>]? in (<def> | <string>) ;
//
// TODO: change this to use forward() maybe.
//
// TODO: for non stdlibs (for strings) check the path
func (p *Node) parseImport(sx SymbolsPtr) error {
	// check for the requierments
	sxl := uint(len(*sx))
	if sxl < 5 {
		return fmt.Errorf("expected at least 5 elements in import (import <def> in <def> ;)")
	}
	var (
		inIndex uint = 2
		list         = []*Node{}
	)
	if (*sx)[0].GetType() != symKWImport ||
		(*sx)[1].GetType() != symDef {
		return fmt.Errorf("expected to begin with 'import <def>', got: '%s %s'", (*sx)[0].String(), (*sx)[1].String())
	}

	// check first node
	x, ok := (*sx)[1].(strSymbol)
	if !ok {
		return fmt.Errorf("expected first operand to be string-like definition, got '%s'", (*sx)[1].String())
	}
	list = append(list, nodeFactory[tokDef](x.Content))

	// get all the defines
	for inIndex+1 < sxl &&
		(*sx)[inIndex].GetType() != symKWIn &&
		((*sx)[inIndex].GetType() == symComma && (*sx)[inIndex+1].GetType() == symDef) {
		x, ok = (*sx)[inIndex+1].(strSymbol)
		if !ok {
			return fmt.Errorf("expected operands to be string-like definition, got '%s'", (*sx)[inIndex+1].String())
		}
		list = append(list, nodeFactory[tokDef](x.Content))
		inIndex += 2
	}

	// check for the lib
	if (*sx)[inIndex].GetType() != symKWIn {
		return fmt.Errorf("expected to have in (librairies) got '%s'", (*sx)[inIndex].String())
	}
	actualSym := (*sx)[inIndex+1].GetType()
	if actualSym != symString && actualSym != symDef {
		return fmt.Errorf("expected library to be string or def, got '%s'", (*sx)[inIndex+1].String())
	}
	libName, ok := (*sx)[inIndex+1].(strSymbol)
	if !ok {
		return fmt.Errorf("could not cast symbol into string, got '%s'", (*sx)[inIndex+1].String())
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
func (p *Node) parseValue(xs SymbolsPtr) error {
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
		return fmt.Errorf("invalid value: %s", val.String())
	}
	forward(xs, 1)
	return nil
}

// call ::= <def> ( [<expr> ,] )
func (p *Node) parseCall(xs SymbolsPtr) error {
	xsl := uint(len(*xs))
	if xsl < 3 {
		return fmt.Errorf("call expects at least 3 arguments, got %d", xsl)
	}

	sav := make([]Symbol, len(*xs))
	copy(sav, *xs)

	if (*xs)[0].GetType() != symDef || (*xs)[1].GetType() != symParOpen {
		return fmt.Errorf("invalid syntax for call: expected '<def> (', got '%s %s'", (*xs)[0].String(), (*xs)[1].String())
	}

	callFunction, ok := (*xs)[0].(strSymbol)
	if !ok {
		return fmt.Errorf("couldnt parse strSymbol from definition: '%s'", (*xs)[0].String())
	}
	forward(xs, 2)
	child := nodeFactory[tokCall](callFunction.Content)
	pos := 1
	for (*xs)[0].GetType() != symParClose {
		if uint(len(*xs)) < 2 {
			*xs = sav
			return fmt.Errorf("expected at least 2 tokens: '<expr> )', got %d", uint(len(*xs)))
		}
		if err := child.parseExpr(xs); err != nil {
			*xs = sav
			return fmt.Errorf("in call of '%s', object %d -> %s", callFunction.Content, pos, err.Error())
		}
		if (*xs)[0].GetType() == symParClose {
			break
		}
		if (*xs)[0].GetType() != symComma {
			return fmt.Errorf("comma expcter after function argument %d, got '%s'", pos, (*xs)[0].String())
		}
		forward(xs, 1)
		pos++
	}
	forward(xs, 1)
	p.append(child)
	return nil
}

// parenexpr ::= ( <expr> )
func (p *Node) parseParenExpr(xs SymbolsPtr) error {
	if (*xs)[0].GetType() != symParOpen {
		return fmt.Errorf("expected '(', got '%s'", (*xs)[0].String())
	}
	forward(xs, 1)
	if err := p.parseExpr(xs); err != nil {
		return fmt.Errorf("in paren expr, could not parse expression -> (%s)", err.Error())
	}
	if (*xs)[0].GetType() != symParClose {
		return fmt.Errorf("expected ')', got '%s'", (*xs)[0].String())
	}
	return forward(xs, 1)
}

// term ::= <call> | <parenexpr> | <value>
func (p *Node) parseTerm(xs SymbolsPtr) error {
	err := [3]error{nil}
	err[0] = p.parseCall(xs)
	if err[0] != nil {
		err[1] = p.parseParenExpr(xs)
	}
	if err[1] != nil {
		err[2] = p.parseValue(xs)
	}
	if err[2] != nil {
		return fmt.Errorf("in term, could not parse -> not a call (%s), nor parenexpr (%s), nor value (%s)", err[0].Error(), err[1].Error(), err[2].Error())
	}
	return nil
}

// unary ::= <test|operation> <expr>
func (p *Node) parseUnary(xs SymbolsPtr) error {
	if len(*xs) < 2 {
		return fmt.Errorf("expected at least 2 symbols, got %d", len(*xs))
	}

	// check for operation
	if operator, ok := convSym2ope[(*xs)[0].GetType()]; ok {
		if _, ok := setOfOpeUnaries[operator]; !ok {
			return fmt.Errorf("not a unary operator: '%s'", convOperator2Str[operator])
		}
		forward(xs, 1)
		child := nodeFactory[tokOperator](operator)
		child.parseTerm(xs)
		p.append(child)
		return nil

		// check for test
	} else if test, ok := convSym2test[(*xs)[0].GetType()]; ok {
		if _, ok := setOfTestUnaries[test]; !ok {
			return fmt.Errorf("not a unary test: '%s'", convTest2Str[test])
		}
		forward(xs, 1)
		child := nodeFactory[tokTest](test)
		child.parseTerm(xs)
		p.append(child)
		return nil
	}
	return fmt.Errorf("not a unary operator/test: '%s'", (*xs)[0].String())
}

// pair ::= <term> <test|operator> <expr>
func (p *Node) parsePair(xs SymbolsPtr) error {
	if len(*xs) < 3 {
		return fmt.Errorf("expected at least 3 symbols, got %d", len(*xs))
	}

	// create a dummy node to parse the first term (auto forward)
	sav := make([]Symbol, len(*xs))
	copy(sav, *xs)

	dummy := nodeFactory[tokProcedure]("")
	if err := dummy.parseTerm(xs); err != nil {
		return fmt.Errorf("in pair, could not parse term in the first operand -> %s", err.Error())
	}
	if len(*xs) < 2 {
		err := fmt.Errorf("exptected at least 2 operators after, got %d", len(*xs))
		*xs = sav
		return err
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
		return fmt.Errorf("not an operator or test in symbol '%s'", (*xs)[0].String())
	}
	forward(xs, 1)
	child.append(dummy.Children[0])

	// add the child: child.parent = p
	p.append(child)

	// get the priority
	for {
		parent := child.parent // same as p
		parentPrio := -1
		if o, ok := parent.tok.(*opeToken); ok {
			parentPrio = int(opePrio[o.Operator])
		} else if t, ok := parent.tok.(*testToken); ok {
			parentPrio = int(testPrio[t.Test])
		}
		pparent := parent.parent // same as p.parent
		if parentPrio >= 0 && uint(parentPrio) > prio && pparent != nil {
			x := child.pop(nil)                         // get the number back
			if tmp := parent.pop(child); tmp != child { // remove the child from parent
				log.Fatal("Expected it to be the child: parent =", parent.String())
			}
			parent.append(x)                    // re-add the object
			child.append(parent)                // re-add the parent in the child
			pparent.replaceChild(parent, child) // change the parent's parent child
		} else {
			break
		}
	}

	// then parse the second child
	if err := child.parseExpr(xs); err != nil {
		*xs = sav
		return fmt.Errorf("in pair, could not parse the second operand -> %s", err.Error())
	}
	return nil
}

// expr ::= <unary> | <pair> | <term>
func (p *Node) parseExpr(xs SymbolsPtr) error {
	errs := [3]error{nil}
	errs[0] = p.parseUnary(xs)
	if errs[0] != nil {
		println("not unary")
		errs[1] = p.parsePair(xs)
	}
	if errs[1] != nil {
		println("not term")
		errs[2] = p.parseTerm(xs)
	}
	if errs[2] != nil {
		return fmt.Errorf("in expr -> not a unary (%s) nor a pais (%s), nor a term (%s)", errs[0], errs[1], errs[2])
	}
	return nil
}

// return ::= return <expr> | return
func (p *Node) parseReturn(xs SymbolsPtr) error {
	if (*xs)[0].GetType() != symKWReturn {
		return fmt.Errorf("expected 'return' keyword")
	}
	forward(xs, 1)
	node := nodeFactory[tokReturn]()
	if (*xs)[0].GetType() == symSemicolon {
		return nil // its just 'return ;'
	}
	if err := node.parseExpr(xs); err != nil {
		return fmt.Errorf("trying to parse content in return: %s", err.Error())
	}
	p.append(node)
	return nil
}

// action ::= <expr> ; | <set> ; | <return> ; | <if>
func (p *Node) parseAction(xs SymbolsPtr) error {
	var a, b error
	a = p.parseReturn(xs)
	if a != nil {
		b = p.parseExpr(xs)
		if b != nil {
			return fmt.Errorf("not an action -> not return (%s), not expr (%s)", a.Error(), b.Error())
		}
	}
	if len(*xs) == 0 || (*xs)[0].GetType() != symSemicolon {
		return fmt.Errorf("missing semicolon")
	}
	forward(xs, 1)
	return nil
}

// block ::= <action> | { [<action>] }
func (p *Node) parseBlock(xs SymbolsPtr) (err error) {
	if (*xs)[0].GetType() == symCurlyOpen {
		block := nodeFactory[tokBlock]()
		forward(xs, 1)
		if len(*xs) == 0 {
			return fmt.Errorf("expected at least an argument after '{'")
		}
		for (*xs)[0].GetType() != symCurlyClose {
			if err = block.parseAction(xs); err != nil {
				return fmt.Errorf("trying to parse action in block -> %s", err.Error())
			}
			if len(*xs) == 0 {
				return fmt.Errorf("expected end of block '}'")
			}
		}
		forward(xs, 1)
		p.append(block)
		return nil
	}
	if err = p.parseAction(xs); err != nil {
		return fmt.Errorf("trying to parse only action -> %s", err.Error())
	}
	return nil
}

// arg ::= < <value> | <testsym> <value !def> >
func (p *Node) parseArg(xs SymbolsPtr) (err error) {
	// check if its a test sym
	test, ok := convSym2test[(*xs)[0].GetType()]
	if ok {
		forward(xs, 1)
		testNode := nodeFactory[tokTest](test)
		if (*xs)[0].GetType() == symDef {
			return fmt.Errorf("definition not accepted when pattern matching test")
		}
		if err = testNode.parseValue(xs); err != nil {
			return fmt.Errorf("trying to parse value after test in argument -> %s", err.Error())
		}
		p.append(testNode)

		// or check if it's a value
	} else if err = p.parseValue(xs); err != nil {
		return fmt.Errorf("trying to parse value in argument -> %s", err.Error())
	}
	return nil
}

// function ::= func <def> ( [<arg> ,] ) <block>
func (p *Node) parseFunction(xs SymbolsPtr) (err error) {
	if len(*xs) < 6 { // smallest is "func a ( ) return ;" or "func a ( ) { }" which is 6
		return fmt.Errorf("a function should have at least 6 tokens: 'func a ( ) return ;'")
	}
	if (*xs)[0].GetType() != symKWFunc || (*xs)[1].GetType() != symDef || (*xs)[2].GetType() != symParOpen {
		return fmt.Errorf("a function should start with these 3 tokens: 'func <def> ('")
	}
	// create the function node
	defName, ok := (*xs)[1].(strSymbol)
	if !ok {
		return fmt.Errorf("function name is not a string'") // def is not a string
	}
	funcNode := nodeFactory[tokFunction](defName.Content)
	forward(xs, 3)

	// parse the args
	args := nodeFactory[tokArgs]()
	for (*xs)[0].GetType() != symParClose {
		if err = args.parseArg(xs); err != nil {
			return fmt.Errorf("trying to parse arguments -> %s", err.Error())
		}
		if (*xs)[0].GetType() == symParClose {
			break
		}
		if (*xs)[0].GetType() != symComma {
			return fmt.Errorf("expected comma or end parenthesis") // expected comma or end parenthesis
		}
		forward(xs, 1)
	}
	forward(xs, 1)
	funcNode.append(args)

	// then parse block
	if err = funcNode.parseBlock(xs); err != nil {
		return fmt.Errorf("trying to parse function content -> %s", err.Error())
	}
	p.append(funcNode)
	return nil
}

// object ::= <import> | <basically> | <function>
func (p *Node) parseObject(xs SymbolsPtr) error {
	var a, b, c error
	if a = p.parseImport(xs); a == nil {
		return nil
	} else if b = p.parseBasically(xs); b == nil {
		return nil
	} else if c = p.parseFunction(xs); c == nil {
		return nil
	}
	return fmt.Errorf("could not parse object -> not import (%s), nor basically (%s), nor function (%s): ", a.Error(), b.Error(), c.Error())
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
		if err := node.parseObject(&symbols); err != nil {
			DisplaySymbols(symbols)
			return nil, fmt.Errorf("could not parse the file -> %s\nNode profile:\n%s", err.Error(), node.String())
		}
		for len(symbols) > 0 && symbols[0].GetType() == symSemicolon {
			forward(&symbols, 1)
		}
	}
	// println(node.String())
	return node, nil
}
