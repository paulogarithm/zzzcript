package zzz

func Run(code string) error {
	xs, err := Lex(code)
	if err != nil {
		return err
	}
	ast, err := Parse(xs)
	if err != nil {
		return err
	}
	// println(ast.String())
	return Interpret(ast)
}