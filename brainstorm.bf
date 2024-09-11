(define (myAdd a b) (+ a b))
=== INSTRUCTIONS ===    // [1.0, 2.0]
ICALL "+"               // [3.0]
=== INSTRUCTIONS ===

(define (myInc a) (+ a 1))
=== INSTRUCTIONS ===    // [1.0]
IPUSH 1.0               // [1.0, 1.0]
ICALL "+"               // [2.0]
=== INSTRUCTIONS ===

(define (idk x) (* 3 (+ x 2)))
=== INSTRUCTIONS ===    // [1.0]
IPUSH 2.0               // [1.0, 2.0]
ICALL "+"               // [3.0]
IPUSH 3                 // [3.0, 3.0]
ISWAP                   // [3.0, 3.0]
ICALL                   // [9.0]
=== INSTRUCTIONS ===
