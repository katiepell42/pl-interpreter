;; Code completed by Garret Mook and Katie Pell

#lang plait
;; See https://docs.racket-lang.org/plait/
;; Adapted by S. Rivoire from https://cs.brown.edu/courses/cs173/2021/implementation-specs/interp/interpreter.rkt
;; =============================================================================
;; Interpreter: interpreter.rkt
;; =============================================================================

(require "support.rkt")

(define (eval [str : S-Exp]): Value
  (interp (desugar (parse str))))

;; DO NOT EDIT ABOVE THIS LINE =================================================

(define (desugarHelperAnd [left : Expr] [right : Expr]) : Expr
    (e-if left
        (e-if right (e-bool #t) (e-bool #f))
        (e-bool #f))
  )

(define (desugarHelperOr [left : Expr] [right : Expr]) : Expr
    (e-if left
        (e-bool #t)
        (e-if right (e-bool #t) (e-bool #f)))
  )

(define (desugarHelperLet [var : Symbol] [value : Expr] [body : Expr]): Expr ; some sort of expression that isn't let
  (e-app (e-lam var body) value)
  )

;DESUGAR
(define (desugar [expr : Expr]): Expr
  (type-case Expr expr
    [(sugar-and left right) (desugarHelperAnd (desugar left) (desugar right))]
    [(sugar-or left right) (desugarHelperOr (desugar left) (desugar right)) ]
    [(e-if cond consq altern) (e-if (desugar cond) consq altern)]
    [(e-op op left right) (e-op op (desugar left) (desugar right))]
    [(e-var name) (e-var name)] ; done, needs testing
    [(e-lam param body) (e-lam param (desugar body))] ; done, needs testing
    [(e-app func arg) (e-app (desugar func) (desugar arg))] ; done, need testing
    [(sugar-let var value body) (desugar (desugarHelperLet var value body))] ;WRONG BUCKO! needs to return only "e" functions
    [else expr])
  )

;INTERP helper function
(define (interp_recursive [env : Env] [expr : Expr]): Value
  (type-case Expr expr
    [(e-num expr) (v-num expr)]
    [(e-str expr) (v-str expr)]
    [(e-bool expr) (v-bool expr)]
    [(e-op op left right) (eopHelper op (interp_recursive env left) (interp_recursive env right))]
    [(e-if cond consq altern) (IfStatementChecker cond consq altern)]
    [(e-var name) (v-bool (var_helper (lookup env name)))] ; done, needs testing
    [(e-lam param body) (v-fun param body env)] ; done, needs testing
    [(e-app func arg) (v-num (func arg))] ;how to get from e-app to number/Value?
    [(sugar-let var val body) (v-fun var body (insert_pair env var (interp_recursive env val)))]
    [else (v-str "Perhaps invalid input")] )
  )

; VAR helper function
(define (var_helper [result : Value]): Boolean
  (type-case Value result
    [(v-bool result) (if result #t #f)]
  (else #f))
  )

;IF STATEMENT
(define (IfStatementChecker [cond : Expr] [consq : Expr] [altern : Expr])
(type-case Value (interp cond)
  [(v-bool bigQual) (if bigQual
                     (interp consq)
                     (interp altern))]
  [else (raise-error (err-if-got-non-boolean (interp cond)))]
  )
)

;HELPER FUNCTIONS FOR RETURNING THE CORRECT ERROR AND CHECKING TO SEE IF EXPRESSION TYPES MATCH
(define (checkIfAreOfSameTypeNum [op : Operator] [expr1 : Value] [expr2 : Value]) : Boolean
(type-case Value expr1
 [(v-num expr1)
  (type-case Value expr2
                 [(v-num expr2)#t]
                 [else (raise-error (err-bad-arg-to-op op expr2))])]
 [else (raise-error (err-bad-arg-to-op op expr1))]
  )                                      
 )


(define (checkIfAreOfSameTypeStr [op : Operator] [expr1 : Value] [expr2 : Value]) : Boolean
(type-case Value expr1
 [(v-str expr1)
  (type-case Value expr2
                 [(v-str expr2)#t]
                 [else (raise-error (err-bad-arg-to-op op expr2))])]
 [else (raise-error (err-bad-arg-to-op op expr1))]
  )                                      
 )

;HELPER FOR DOING OP OPERATIONS
(define (eopHelper [op : Operator] [left : Value] [right : Value]) : Value
(type-case Operator op

    [(op-plus)
     (if (checkIfAreOfSameTypeNum op left right)
     (v-num(+(v-num-value left)(v-num-value right)))
     (raise-error (err-bad-arg-to-op op (v-str "ERROR")))
     )
     ]
  
    [(op-append)
      (if (checkIfAreOfSameTypeStr op left right)
      (v-str(string-append(v-str-value left)(v-str-value right)))
     (raise-error (err-bad-arg-to-op (op-append) (v-str "ERROR")))
     )
    ]

  
    [(op-str-eq)
     (if (checkIfAreOfSameTypeStr op left right)
      (v-bool(string=?(v-str-value left)(v-str-value right)))
     (raise-error (err-bad-arg-to-op (op-str-eq) (v-str "ERROR")))
     )
     ]
  
    [(op-num-eq)
       (if (checkIfAreOfSameTypeNum op left right)
      (v-bool(=(v-num-value left)(v-num-value right)))
     (raise-error (err-bad-arg-to-op (op-num-eq) (v-str "ERROR")))
     )
     ]
  
     )
  )



;Create empty environment
(define (make_env): Env ; Creates an empty environment
  (hash (list))
  )

; Create an environment with one key value pair
(define (insert_pair [i-ht : Env] [key : Symbol] [val : Value]): Env
  (hash-set i-ht key val)
 )

;Look up a key value pair in the environment
(define (lookup [env : Env] [key : Symbol]): Value
  (type-case (Optionof Value) (hash-ref env key) ; What variant of OptionOf is x?
    [(none) (v-str "nothing")] ; If its variant is "none"
    [(some x) x] ; If its variant is "some", return its value
    )
)




;INTERP
(define (interp [expr : Expr]): Value
; Create a new environment
; Call helper function with expr, new environment as arguments
  (interp_recursive (make_env) expr)

  )


