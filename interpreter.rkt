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

; Creates empty environment
(define (make_env): Env ; Creates an empty environment
  (hash (list))
  )

; Creates an environment with one key value pair
(define (insert_pair [i-ht : Env] [key : Symbol] [val : Value]): Env
  (hash-set i-ht key val)
  )

;Looks up a key value pair in the environment
(define (lookup [env : Env] [key : Symbol]): Value
  (type-case (Optionof Value) (hash-ref env key) ; What variant of OptionOf is x?
    [(none) (raise-error (err-unbound-var key))] ; If its variant is "none" THIS SHOULD BE A RAISE ERROR INSTEAD
    [(some x) x] ; If its variant is "some", return its value
    )
)

; AND helper function: Translates "and" into equivalent core expressions
(define (desugarHelperAnd [left : Expr] [right : Expr]) : Expr
    (e-if left
        (e-if right (e-bool #t) (e-bool #f))
        (e-bool #f))
  )

; OR helper function: Translates "or" into equivalent core expressions
(define (desugarHelperOr [left : Expr] [right : Expr]) : Expr
    (e-if left
        (e-bool #t)
        (e-if right (e-bool #t) (e-bool #f)))
  )

; LET helper function: Translates "let" into equivalent core expressions
(define (desugarHelperLet [var : Symbol] [value : Expr] [body : Expr]): Expr ; some sort of expression that isn't let
  (e-app (e-lam var body) value)
  )

; VAR helper function: Translates a v-bool to a Boolean
;(define (var_helper [result : Value]): Boolean ; lookup needs to happen in here
;  (type-case Value result
;    [(v-bool result) (if result some )]
;  (else #f))
;  )


; APP helper function: applies a function to an argument, returns a value
(define (appHelper [func : Value] [arg : Value] [env : Env]) : Value ;insert key value pair into environment where key is param of v-fun that is passed in as func and value is arg
  (type-case Value func
    ;[(v-fun param body env) (v-fun param body (insert_pair env param arg))]
    ;[(v-fun param body env) (v-fun param body (insert_pair env (v-fun-param func) arg))]
    ;[(v-fun param body env) (interp_recursive body (insert_pair env (v-fun-param func) arg))]
    [(v-fun param body env1) (interp_recursive (v-fun-body func) (insert_pair env (v-fun-param func) arg))]
    [else (raise-error (err-not-a-function arg))] ;pass the arg into the error
  )
  )

; DESUGAR
(define (desugar [expr : Expr]): Expr
  (type-case Expr expr
    [(sugar-and left right) (desugarHelperAnd (desugar left) (desugar right))]
    [(sugar-or left right) (desugarHelperOr (desugar left) (desugar right)) ]
    [(e-if cond consq altern) (e-if (desugar cond) consq altern)]
    [(e-op op left right) (e-op op (desugar left) (desugar right))]
    [(e-lam param body) (e-lam param (desugar body))] ; done, needs testing
    [(e-app func arg) (e-app (desugar func) (desugar arg))] ; done, need testing
    ;[(sugar-let var value body) (desugarHelperLet var (desugar value) (desugar body))] ;needs to be made out desugar
    [(sugar-let var val body) (desugar (e-app (e-lam var (desugar body)) val))]
    [else expr])
  )


; Recursive INTERP function w/environment
(define (interp_recursive [expr : Expr] [env : Env]): Value
  (type-case Expr expr
    [(e-num expr) (v-num expr)]
    [(e-str expr) (v-str expr)]
    [(e-bool expr) (v-bool expr)]
    [(e-op op left right) (eopHelper op (interp_recursive left env) (interp_recursive right env))]
    [(e-if cond consq altern) (IfStatementChecker cond consq altern)]
    [(e-var name) (lookup env name)] ; done, needs testing
    [(e-lam param body) (v-fun param body env)] ; done, needs testing
    [(e-app func arg) (appHelper (interp_recursive func env) (interp_recursive arg env) env)]
   ;[(e-app func arg) (v-num (func arg))] ;how to get from e-app to number/Value?

    [else (v-str "Perhaps invalid input")] )
  )



; IF statement error handling function
(define (IfStatementChecker [cond : Expr] [consq : Expr] [altern : Expr])
  (type-case Value (interp cond)
    [(v-bool bigQual) (if bigQual
                          (interp consq)
                          (interp altern))]
    [else (raise-error (err-if-got-non-boolean (interp cond)))]
  )
)

; Error handling and type checking helper function
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

; Operator error handling helper function
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



; INTERP wrapper function
(define (interp [expr : Expr]): Value
  (interp_recursive expr (make_env))
  )



