package zzz

import (
	"fmt"
	"strconv"
)

func builtinPrint(args []argument) argument {
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
}

func builtinEven(args []argument) argument {
	return []argument{args[0].(zzzInt) % 2 == 0}
}
