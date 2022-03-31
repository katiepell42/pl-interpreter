#lang racket
;; Adapted by S. Rivoire from https://cs.brown.edu/courses/cs173/2021/implementation-specs/interp/interpreter-tests.rkt
;; my tests for SSU CS 460 Programming Assignment #1

;; =============================================================================
;; Interpreter: interpreter-tests.rkt
;; =============================================================================

(require "interpreter.rkt"
         "support.rkt"
         "test-support.rkt")

;; DO NOT EDIT ABOVE THIS LINE =================================================
(define/provide-test-suite student-tests ;; DO NOT EDIT THIS LINE ==========
3
;;-------------------------------------
;;           PROJECT 1 TEST
;;-------------------------------------
  
  ;; Tests 1-4: Primitive datatypes
  ;1. Check number evualutes to v-num
  (test-equal? "1: Num primitive" (eval `201) (v-num 201))
  ;2. Check a string evualutes to v-str
  (test-equal? "2: Str primitive" (eval `"cat") (v-str "cat"))
  ;3. Check if a true evualutes to a bool
  (test-equal? "3a: Bool primitive (true)" (eval `true) (v-bool #t))
  ;4. Check if a false evualutes to a bool
  (test-equal? "3b: Bool primitive (false)" (eval `false) (v-bool #f))
  

  ;; Test 5-10: Operators, with primitive operands
  ;5. Tests addition
  (test-equal? "4a: + with Num primitives" (eval `(+ 201 203)) (v-num 404))
  ;6. Tests string append
  (test-equal? "4b: ++ with Str primitives" (eval `(++ "cat" "dog")) (v-str "catdog"))
  ;7. Tests string equal
  (test-equal? "4c: str= with Str primitives, yes equal" (eval `(str= "dog" "dog")) (v-bool #t))
  ;8. Tests string not equal
  (test-equal? "4d: str= with Str primitives, not equal" (eval `(str= "cat" "dog")) (v-bool #f))
  ;9. Tests if two numbers are equal
  (test-equal? "4e: num= with Num primitives, yes equal" (eval `(num= 404 404)) (v-bool #t))
  ;10. Tests if two numbers are not equal
  (test-equal? "4f: num= with Num primitives, not equal" (eval `(num= 201 203)) (v-bool #f))

  ;; Test 11-16: If-expressions with primitive condition, primitive if, primitive else
  ;11. Tests if statement is running consq for a number
  (test-equal? "5a: if with primitive operands" (eval `(if true 201 202)) (v-num 201))
  ;12. Tests if statement is running altern for a number
  (test-equal? "5b: if with primitive operands" (eval `(if false 201 202)) (v-num 202))
  ;13. Tests if statement is running consq for a string
  (test-equal? "5c: if with primitive operands" (eval `(if true "cat" "dog")) (v-str "cat"))
  ;14. Tests if statement is running altern for a string
  (test-equal? "5d: if with primitive operands" (eval `(if false "cat" "dog")) (v-str "dog"))
  ;15. Tests if statement is running consq for a bool
  (test-equal? "5e: if with primitive operands" (eval `(if true false true)) (v-bool #f))
  ;16. Tests if statement is running altern for a bool
  (test-equal? "5f: if with primitive operands" (eval `(if false false true)) (v-bool #t))

  ;; Test 17-23: Operators, with recursively evaluated operands
  ;17. Tests + with left op gently recursive
  (test-equal? "6a: + with left op gently recursive" (eval `(+ (+ 69 94) 203)) (v-num 366))
  ;18. Tests + with left op deeply recursive
  (test-equal? "6b: + with left op deeply recursive" (eval `(+ (+ (+ 69 94) 22) 203)) (v-num 388))
  ;19. Tests + with right op gently recursive
  (test-equal? "6c: + with right op gently recursive" (eval `(+ 201 (+ 69 94))) (v-num 364))
  ;20. Tests + with right op deeply recursive
  (test-equal? "6d: + with right op deeply recursive" (eval `(+ 201 (+ (+ 69 94) 22))) (v-num 386))
  ;21. Tests 6e: + with both ops gently recursive
  (test-equal? "6e: + with both ops gently recursive" (eval `(+ (+ 69 94) (+ 69 94))) (v-num 326))
  ;22. Tests + with both ops deeply recursive
  (test-equal? "6f: + with both ops deeply recursive"
               (eval `(+ (+ 201 (+ (+ 69 94) 22)) (+ (+ (+ 69 94) 22) 203))) (v-num 774))

  ; Test 23-28: ++
  ;23. Tests ++ with left op gently recursive
  (test-equal? "6g ++ with left op gently recursive"
               (eval `(++ (++ "sonoma " "state ") "university")) (v-str "sonoma state university"))
  ;24. Tests ++ with left op deeply recursive
  (test-equal? "6h ++ with left op deeply recursive"
               (eval `(++ (++ (++ "sonoma " "state ") "university ") "california"))
               (v-str "sonoma state university california"))
  ;25. Tests ++ with right op gently recursive
  (test-equal? "6i ++ with right op gently recursive"
               (eval `(++ "university " (++ "sonoma " "state")))
               (v-str "university sonoma state"))
  ;26. Tests ++ with right op deeply recursive
  (test-equal? "6j ++ with right op deeply recursive"
               (eval `(++ "university " (++ (++ "sonoma " "state ") "california")))
               (v-str "university sonoma state california"))
  ;27. Tests ++ with both ops gently recursive
  (test-equal? "6k ++ with both ops gently recursive"
               (eval `(++ (++ "sonoma " "state") (++ "sonoma " "state")))
               (v-str "sonoma statesonoma state"))
  ;28. Tests ++ with both ops deeply recursive
  (test-equal? "6l ++ with both ops deeply recursive"
               (eval `(++ (++ "california" (++ (++ "sonoma " "state ") "university"))
                          (++ (++ (++ "sonoma " "state ") "university") "california")))
               (v-str "californiasonoma state universitysonoma state universitycalifornia"))

  ; Test 29-40: num=
  ;29. Tests num= with left op gently recursive (true 
  (test-equal? "6m: num= with left op gently recursive (true)"
               (eval `(num= (+ 69 94) 163)) (v-bool #t))
  ;30. Tests num= with left op gently recursive (false)
  (test-equal? "6n: num= with left op gently recursive (false)"
               (eval `(num= (+ 69 94) 203)) (v-bool #f))
  ;31. Tests num= with left op deeply recursive (true
  (test-equal? "6o: num= with left op deeply recursive (true)"
               (eval `(num= (+ (+ (+ 69 94) 22) 203) 388)) (v-bool #t))
  ;32. Tests num= with left op deeply recursive (false)
  (test-equal? "6p: num= with left op deeply recursive (false)"
               (eval `(num= (+ (+ (+ 69 94) 22) 203) 400)) (v-bool #f))
  ;33. Tests num= with right op gently recursive (true)
  (test-equal? "6q: num= with right op gently recursive (true)"
               (eval `(num= 163 (+ 69 94))) (v-bool #t))
  ;34. Tests num= with right op gently recursive (false)
  (test-equal? "6r: num= with right op gently recursive (false)"
               (eval `(num= 203 (+ 69 94))) (v-bool #f))
  ;35. Tests  num= with right op deeply recursive (true)
  (test-equal? "6s: num= with right op deeply recursive (true)"
               (eval `(num= 386 (+ 201 (+ (+ 69 94) 22)))) (v-bool #t))
  ;36. Tests num= with right op deeply recursive (false)
  (test-equal? "6t: num= with right op deeply recursive (false)"
               (eval `(num= 400 (+ 201 (+ (+ 69 94) 22)))) (v-bool #f))
  ;37. Tests num= with both ops gently recursive (true)
  (test-equal? "6u: num= with both ops gently recursive (true)"
               (eval `(num= (+ 69 94) (+ 69 94))) (v-bool #t))
  ;38. Tests num= with both ops gently recursive (false)
  (test-equal? "6v: num= with both ops gently recursive (false)"
               (eval `(num= (+ 69 94) (+ 69 95))) (v-bool #f))
  ;39. Tests num= with both ops deeply recursive (true)
  (test-equal? "6w: num= with both ops deeply recursive (true)"
               (eval `(num= (+ 201 (+ (+ 69 94) 22)) (+ (+ (+ 69 94) 22) 201))) (v-bool #t))
  ;40. Tests num= with both ops deeply recursive (false)
  (test-equal? "6x: num= with both ops deeply recursive (false)"
               (eval `(num= (+ 201 (+ (+ 69 94) 22)) (+ (+ (+ 69 94) 22) 203))) (v-bool #f))

  ; Test 41 -52: str=
  ;41. Tests str= with left op gently recursive (true
  (test-equal? "6y: str= with left op gently recursive (true)"
               (eval `(str= (++ "sonoma " "state") "sonoma state")) (v-bool #t))
  ;42. Tests str= with left op gently recursive (false)
  (test-equal? "6z: str= with left op gently recursive (false)"
                (eval `(str= (++ "sonoma " "state") "sonomastate"))  (v-bool #f))
  ;43. Tests str= with left op deeply recursive (true
  (test-equal? "6aa: str= with left op deeply recursive (true)"
               (eval `(str= (++ (++ (++ "sonoma " "state ") "university ") "california") "sonoma state university california"))
               (v-bool #t))
  ;44. Tests str= with left op deeply recursive (false)
  (test-equal? "6ab: str= with left op deeply recursive (false)"
               (eval `(str= (++ (++ (++ "sonoma " "state ") "university ") "california") "sonomastate university california"))
               (v-bool #f))
  ;45. Tests str= with right op gently recursive (true)
  (test-equal? "6ac: str= with right op gently recursive (true)"
               (eval `(str= "sonoma state" (++ "sonoma " "state"))) (v-bool #t))
  ;46. Tests str= with right op gently recursive (false)
  (test-equal? "6ad: str= with right op gently recursive (false)"
               (eval `(str= "sonomastate" (++ "sonoma " "state"))) (v-bool #f))
  ;47. Tests str= with right op deeply recursive (true)
  (test-equal? "6ae: str= with right op deeply recursive (true)"
               (eval `(str= (++ "university " (++ (++ "sonoma " "state ") "california"))
                            "university sonoma state california"))
               (v-bool #t))
  ;48. Tests str= with right op deeply recursive (false)
  (test-equal? "6af: str= with right op deeply recursive (false)"
               (eval `(str= (++ "university " (++ (++ "sonoma " "state ") "california"))
                            "university sonomastate california"))
               (v-bool #f))
  ;49. Tests str= with both ops gently recursive (true)
  (test-equal? "6ag: str= with both ops gently recursive (true)"
               (eval `(str= (++ "sonoma " "state") (++ "sonoma " "state"))) (v-bool #t))
  ;50. Tests tr= with both ops gently recursive (false)
  (test-equal? "6ah: str= with both ops gently recursive (false)"
               (eval `(str= (++ "sonoma " "state") (++ "sonoma" "state"))) (v-bool #f))
  ;51. Tests str= with both ops deeply recursive (true
  (test-equal? "6ai: str= with both ops deeply recursive (true)"
               (eval `(str= (++ "california" (++ (++ "sonoma " "state ") "university"))
                          (++ "california" (++ (++ "sonoma " "state ") "university"))))
               (v-bool #t))
  ;52. Tests str= with both ops deeply recursive (false)
  (test-equal? "6aj: str= with both ops deeply recursive (false)"
               (eval `(str= (++ "california " (++ (++ "sonoma " "state ") "university"))
                          (++ "california" (++ (++ "sonoma " "state ") "university"))))
               (v-bool #f))
  
  ; Tests 53-72: if
  ;53. Tests if with true condition and gently recursive consq clause
  (test-equal? "6ak: if with true condition and gently recursive consq clause"
               (eval `(if true (+ 69 94) 202)) (v-num 163))
  ;54. Tests if with false condition and gently recursive consq clause
  (test-equal? "6al: if with false condition and gently recursive consq clause"
               (eval `(if false (+ 69 94) 202)) (v-num 202))
  ;55. Tests if with true condition and deeply recursive consq clause
  (test-equal? "6am: if with true condition and deeply recursive consq clause"
               (eval `(if true (+ (+ (+ 69 94) 22) 203) 202)) (v-num 388))
  ;56. Tests if with false condition and deeply recursive consq clause
  (test-equal? "6an: if with false condition and deeply recursive consq clause"
               (eval `(if false (+ (+ (+ 69 94) 22) 203) 202)) (v-num 202))
  ;57. Tests if with true condition and gently recursive alt clause
  (test-equal? "6ao: if with true condition and gently recursive alt clause"
               (eval `(if true "SSU" (++ "sonoma " "state"))) (v-str "SSU"))
  ;58. Tests if with false condition and gently recursive alt clause
  (test-equal? "6ap if with false condition and gently recursive alt clause"
               (eval `(if false "SSU" (++ "sonoma " "state"))) (v-str "sonoma state"))
  ;59. Tests if with true condition and deeply recursive alt clause
  (test-equal? "6aq: if with true condition and deeply recursive alt clause"
               (eval `(if true "SSU" (++ (++ "california" (++ (++ "sonoma " "state ") "university"))
                          (++ (++ (++ "sonoma " "state ") "university") "california")))) (v-str "SSU"))
  ;60. Tests if with false condition and deeply recursive alt clause
  (test-equal? "6ar: if with false condition and deeply recursive alt clause"
               (eval `(if false "SSU" (++ (++ "california" (++ (++ "sonoma " "state ") "university"))
                          (++ (++ (++ "sonoma " "state ") "university") "california"))))
               (v-str "californiasonoma state universitysonoma state universitycalifornia"))
  ;61. Tests if with true condition and both clauses gently recursive
  (test-equal? "6as: if with true condition and both clauses gently recursive"
               (eval `(if true (+ 69 94) (+ 201 203))) (v-num 163))
  ;62. Tests if with false condition and both clauses gently recursive
  (test-equal? "6at: if with false condition and both clauses gently recursive"
               (eval `(if false (+ 69 94) (+ 201 203))) (v-num 404))
  ;63. Tests if with true condition and both clauses deeply recursive
  (test-equal? "6au: if with true condition and both clauses deeply recursive"
               (eval `(if true (+ 201 (+ (+ 69 94) 22)) (+ (+ (+ 69 94) 22) 203))) (v-num 386))
  ;64. Tests if with false condition and both clauses deeply recursive
  (test-equal? "6av: if with false condition and both clauses deeply recursive"
               (eval `(if false (+ 201 (+ (+ 69 94) 22)) (+ (+ (+ 69 94) 22) 203))) (v-num 388))
  ;65. Tests if with gently recursive true condition
  (test-equal? "6aw: if with gently recursive true condition"
               (eval `(if (str= "dog" "dog") 201 202)) (v-num 201))
  ;66. Tests if with gently recursive false condition
  (test-equal? "6ax: if with gently recursive false condition"
               (eval `(if (str= "dog" "cat") 201 202)) (v-num 202))
  ;67. Tests  if with deeply recursive true condition
  (test-equal? "6ay: if with deeply recursive true condition"
               (eval `(if (num= (+ 201 (+ (+ 69 94) 22)) (+ (+ (+ 69 94) 22) 201)) 201 202))
               (v-num 201))
  ;68. Tests if with deeply recursive false condition
  (test-equal? "6az: if with deeply recursive false condition"
               (eval `(if (num= (+ 201 (+ (+ 69 94) 22)) (+ (+ (+ 69 94) 22) 203)) 201 202))
               (v-num 202))
  ;69. Tests if with gently recursive true condition and gently recursive clauses
  (test-equal? "6ba: if with gently recursive true condition and gently recursive clauses"
               (eval `(if (str= "dog" "dog") (++ "sonoma " "state") (++ "state " "sonoma")))
               (v-str "sonoma state"))
  ;70. Tests if with gently recursive false condition and gently recursive clauses
  (test-equal? "6bb: if with gently recursive false condition and gently recursive clauses"
               (eval `(if (str= "dog" "cat") (++ "sonoma " "state") (++ "state " "sonoma")))
               (v-str "state sonoma"))
  ;71. Tests if with deeply recursive true condition and deeply recursive clauses
  (test-equal? "6bc: if with deeply recursive true condition and deeply recursive clauses"
               (eval `(if (num= (+ 201 (+ (+ 69 94) 22))
                                (+ 201 (+ (+ 69 94) 22)))
                                (+ (+ (+ 69 94) 22) 201) (+ (+ (+ 69 94) 22) 203)))
                     (v-num 386))
  ;72. Tests 
  (test-equal? "6bd: if with deeply recursive false condition and deeply recursive clauses"
               (eval `(if (num= (+ 201 (+ (+ 69 94) 22))
                                (+ 203 (+ (+ 69 94) 22)))
                                (+ (+ (+ 69 94) 22) 201) (+ (+ (+ 69 94) 22) 203)))
                     (v-num 388))
  
  ;; Test 73-74: The coolest tests from Piazza
  ;73. Credit: Jonathan Calderon Chavez
  (test-equal? "7a: (++ <expr> <expr>) - nested append"
               (eval `{++ (++(++"g" "o")(++ "o" (++ "f" "y ")))(++(++ "g" (++ "o" "o"))(++ "b" (++ "e" "r")))})
               (v-str "goofy goober"))
  ;74. Credit: David Holstedt
  (test-equal? "7b: Arbitrarily nested statements"
               (eval `{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+{+ 1 0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0}0})
               (v-num 1))
  
  ;; Test 75-76: Error handling for if-got-non-boolean
  ;75. Tests string constant in if-condition"
  (test-raises-interp-error? "8a: String constant in if-condition"
                             (eval `(if "rat" "cat" "dog"))
                             (err-if-got-non-boolean (v-str "rat")))
  ;76. Tests more complicated non-bool expr in if-condition
  (test-raises-interp-error? "8b: More complicated non-bool expr in if-condition"
                             (eval `(if (+ 100 200) "cat" "dog"))
                             (err-if-got-non-boolean (v-num 300)))

  ;; Test 77-: Error handling for bad-arg-to-op
  ;77. Tests string const as left operand of +
  (test-raises-interp-error? "9a: String const as left operand of +"
                             (eval `(+ "cat" 1))
                             (err-bad-arg-to-op (op-plus) (v-str "cat")))
  ;78. Tests string const as right operand of +
  (test-raises-interp-error? "9b: String const as right operand of +"
                             (eval `(+ 1 "cat"))
                             (err-bad-arg-to-op (op-plus) (v-str "cat")))
  ;79. Tests two bad args to +, pick correct one
  (test-raises-interp-error? "9c: Two bad args to +, pick correct one"
                             (eval `(+ "dog" "cat"))
                             (err-bad-arg-to-op (op-plus) (v-str "dog")))
  ;80. Tests numeric const as left operand of ++
  (test-raises-interp-error? "9d: Numeric const as left operand of ++"
                             (eval `(++ 1 "cat"))
                             (err-bad-arg-to-op (op-append) (v-num 1)))
  ;81. Tests numeric const as right operand of +
  (test-raises-interp-error? "9e: Numeric const as right operand of +"
                             (eval `(++ "cat" 11))
                             (err-bad-arg-to-op (op-append) (v-num 11)))
  ;82. Tests two bad args to ++, pick correct one
  (test-raises-interp-error? "9f: Two bad args to ++, pick correct one"
                             (eval `(++ 12 false))
                             (err-bad-arg-to-op (op-append) (v-num 12)))
  ;83. Tests numeric const as left operand of str=
  (test-raises-interp-error? "9g: Numeric const as left operand of str="
                             (eval `(str= 1 "cat"))
                             (err-bad-arg-to-op (op-str-eq) (v-num 1)))
  ;84. Tests numeric const as right operand of str=
  (test-raises-interp-error? "9h: Numeric const as right operand of str="
                             (eval `(str= "cat" 11))
                             (err-bad-arg-to-op (op-str-eq) (v-num 11)))
  ;85. Tests two bad args to str=, pick correct one
  (test-raises-interp-error? "9i: Two bad args to str=, pick correct one"
                             (eval `(str= 12 false))
                             (err-bad-arg-to-op (op-str-eq) (v-num 12)))
  ;86. Tests string const as left operand of num=
  (test-raises-interp-error? "9j: String const as left operand of num="
                             (eval `(num= "cat" 1))
                             (err-bad-arg-to-op (op-num-eq) (v-str "cat")))
  ;87. Tests string const as right operand of num
  (test-raises-interp-error? "9k: String const as right operand of num="
                             (eval `(num= 1 "cat"))
                             (err-bad-arg-to-op (op-num-eq) (v-str "cat")))
  ;88. Tests two bad args to num=, pick correct one
  (test-raises-interp-error? "9l: Two bad args to num=, pick correct one"
                             (eval `(num= "dog" "cat"))
                             (err-bad-arg-to-op (op-num-eq) (v-str "dog")))

;;-------------------------------------
;;           PROJECT 2 TEST
;;-------------------------------------
  
  ;Test 89-: desguar and and or
  ;;89. Tests basic desugar AND
  (test-equal? "Test basic desugar AND"
               (eval `{and true true}) (v-bool #t)
  )
  ;;90. Tests basic desugar AND returns false
    (test-equal? "Test basic desugar AND returns false "
               (eval `{and true false}) (v-bool #f)
  )
  ;;91. Tests basic desugar OR
  (test-equal? "Test basic desugar OR"
               (eval `{or false true}) (v-bool #t)) 

  ;;92. Tests IF desugar AND returns true
   (test-equal? "Test IF desugar AND returns true"
               (eval `{if (and true true) true false}) (v-bool #t)
  )
  ;;93. Tests IF desugar OR returns true
  (test-equal? "Test IF desugar OR returns true"
               (eval `{if (or false true) true false}) (v-bool #t)
  ) 
  ;;94. Tests IF desugar AND returns fals
  (test-equal? "Test IF desugar AND returns false"
               (eval `{if (and true false) true false}) (v-bool #f)
  )
  ;;95. Tests IF desugar OR returns false
  (test-equal? "Test IF desugar OR returns false"
               (eval `{if (or false false) true false}) (v-bool #f)
  ) 
  ;;96. Tests IF desugar AND returns true complex
  (test-equal? "Test IF desugar AND returns true complex"
               (eval `{if (and (str= (++ "Appending" "This") "AppendingThis") true) true false}) (v-bool #t)
  ) 
  ;;97. Tests IF desugar OR returns true complex
  (test-equal? "Test IF desugar OR returns true complex"
               (eval `{if (or false (num= (+ 1 1) 2)) true false}) (v-bool #t)
  ) 
  ;;98. Tests IF desugar AND returns false complex
  (test-equal? "Test IF desugar AND returns false complex"
               (eval `{if (and true (str= (++ "Appending" "This") "AppendingThisJustKidding" )) true false}) (v-bool #f)
  )
  ;;99. Tests IF desugar OR returns false complex
  (test-equal? "Test IF desugar OR returns false complex"
               (eval `{if (or false (num= (+ 1 1) 3)) true false}) (v-bool #f)
  ) 
  ;;100. Tests IF desugar OR returns true complex SHORT CIRCUIT
  (test-equal? "Test IF desugar OR returns true complex SHORT CIRCUIT"
               (eval `{if (or true "abc") true false}) (v-bool #t)
  )
  ;;101. Tests Test if case for numbers
  (test-equal? "Test if case for numbers"
               (eval `{ +   1   (if (and (num= 1 1) (num= 2 2)) 1 2)      }) (v-num 2)
  )

;;EXTRA Sugar tests

  ;Tests 102-105: nested AND and OR
  ;102. Tests nested desugar AND(should return true)
  (test-equal? "NESTED TEST: desugar AND - should return true"
               (eval `{if (and (and (and (and true true) (and true true)) (and (and true true) (and true true))) (and (and true true) (and true true))) true false}) (v-bool #t)
  )
  ;103. Tests nested desugar OR(should return true)
  (test-equal? "NESTED TEST: desugar OR - should return true"
               (eval `{if (or (or (or (or false true) (or false true)) (or (or false true) (or false true))) (or (or false true) (or false true))) true false}) (v-bool #t)
  ) 
  ;104. Tests nested desugar AND(should return false)
  (test-equal? "NESTED TEST: desugar AND - should return false"
               (eval `{if (and (and (and (and (and true false) (and true false)) (and (and true false) (and true false))) (and (and (and true false) (and true false)) (and (and true false) (and true false)))) (and (and (and true false) (and true false)) (and (and true false) (and true false)))) true false}) (v-bool #f)
  )
  ;105. Tests nested desugar OR(should return false)
  (test-equal? "NESTED TEST: desugar OR - should return false"
               (eval `{if (or (or (or (or false false) (or false false)) (or (or false false) (or false false))) (or (or (or false false) (or false false)) (or (or false false) (or false false)))) true false}) (v-bool #f)
  ) 


;;END EXTRA

  
  ;Test 106-112: Enviornment tests
  ;106. Tests for creating an empty environment
  (test-equal? "Test for creating an empty environment"
               (v-bool (hash-empty? (make_env))) (v-bool #t)
  )
  ;107. Tests for inserting a v-str into an empty environment (will also fail if creating an empty environment fails)
    (test-equal? "Test for inserting a v-str into an empty environment (will also fail if creating an empty environment fails)"
               (v-bool (hash-empty? (insert_pair (make_env) 'A (v-str "test")))) (v-bool #f)
  )
  ;108. Tests for inserting a v-num into an empty environment (will also fail if creating an empty environment fails)
  (test-equal? "Test for inserting a v-num into an empty environment (will also fail if creating an empty environment fails)"
               (v-bool (hash-empty? (insert_pair (make_env) 'A (v-num 4)))) (v-bool #f)
  )
  ;109. Tests for inserting a v-bool into an empty environment (will also fail if creating an empty environment fails)
  (test-equal? "Test for inserting a v-bool into an empty environment (will also fail if creating an empty environment fails)"
               (v-bool (hash-empty? (insert_pair (make_env) 'A (v-bool #t)))) (v-bool #f)
  )
  ;110. Tests for inserting the correct value into an empty environment
  (test-equal? "Test for inserting the correct value into an empty environment"
               (hash-ref (insert_pair (make_env) 'A (v-str "test")) 'A) (v-str "test")
  )
  ;111. Tests for looking up a key in the Env
  (test-equal? "Test for looking up a key in the Env"
               (lookup (hash-set (make_env) 'A (v-str "test")) 'A) (v-str "test")
  )
;  ;112. Tests for looking up a key that is not in the Env
;  (test-equal? "Test for looking up a key that is not in the Env"
;               (lookup (make_env) 'B) (key)
;  )

  ;113.
  (test-raises-interp-error? "II-E-1: Raises error for simple expression with unknown identifier"
               (eval `a) (err-unbound-var 'a)
   )

  ;114.
  (test-raises-interp-error? "II-E-1: Raises error for simple expression with unknown identifier"
               (lookup (make_env) 'B) (err-unbound-var 'B)
   )


  ;;----Let tests----

 (test-equal? "Testing Lam"
               (eval `((lam x (+ x 3)) 2)) (v-num 5))
  
  (test-equal? "Testing the let identity (just returning what the value is)"
               (eval `{let (x 5) x}) (v-num 5))

  (test-equal? "The let example from Brown's 3.3.1.2, simple addition"
               (eval `{let (x 1) (+ x 2)}) (v-num 3))

  (test-equal? "The same let addition but on the other branch"
               (eval `{let (x 1) (+ 2 x)}) (v-num 3))

  (test-equal? "Nested lets"
               (eval `{let (x 1) (let (y 2) (+ y x))}) (v-num 3))

  (test-equal? "Deeply nested let"
               (eval `{let (x0 0) (let (x1 1) (let (x2 2) (let (x3 3) (let (x4 4) (let (x5 5) (let (x6 6) (let (x7 7) (let (x8 8) (let (x9 9) (let (x10 10) (let (x11 11) (let (x12 12) (let (x13 13) (let (x14 14) (let (x15 15) (let (x16 16) (let (x17 17) (let (x18 18) (let (x19 19) (let (x20 20) (let (x21 21) (let (x22 22) (let (x23 23) (let (x24 24) (let (x25 25) (let (x26 26) (let (x27 27) (let (x28 28) (let (x29 29) (let (x30 30) (let (x31 31) (let (x32 32) (let (x33 33) (let (x34 34) (let (x35 35) (let (x36 36) (let (x37 37) (let (x38 38) (let (x39 39) (let (x40 40) (let (x41 41) (let (x42 42) (let (x43 43) (let (x44 44) (let (x45 45) (let (x46 46) (let (x47 47) (let (x48 48) (let (x49 49) (+ x0 (+ x1 (+ x2 (+ x3 (+ x4 (+ x5 (+ x6 (+ x7 (+ x8 (+ x9 (+ x10 (+ x11 (+ x12 (+ x13 (+ x14 (+ x15 (+ x16 (+ x17 (+ x18 (+ x19 (+ x20 (+ x21 (+ x22 (+ x23 (+ x24 (+ x25 (+ x26 (+ x27 (+ x28 (+ x29 (+ x30 (+ x31 (+ x32 (+ x33 (+ x34 (+ x35 (+ x36 (+ x37 (+ x38 (+ x39 (+ x40 (+ x41 (+ x42 (+ x43 (+ x44 (+ x45 (+ x46 (+ x47 (+ x48 (+ x49 0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))}) (v-num 1225))
  ; sum of n consecutive numbers is n/2 (first num + last num). 2/49 (1 + 49) = 1225

  (test-equal? "check on evaluating the value in let"
               (eval `{let (x (+ 0 (+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ 8 (+ 9 (+ 10 (+ 11 (+ 12 (+ 13 (+ 14 (+ 15 (+ 16 (+ 17 (+ 18 (+ 19 (+ 20 (+ 21 (+ 22 (+ 23 (+ 24 (+ 25 (+ 26 (+ 27 (+ 28 (+ 29 (+ 30 (+ 31 (+ 32 (+ 33 (+ 34 (+ 35 (+ 36 (+ 37 (+ 38 (+ 39 (+ 40 (+ 41 (+ 42 (+ 43 (+ 44 (+ 45 (+ 46 (+ 47 (+ 48 (+ 49 0))))))))))))))))))))))))))))))))))))))))))))))))))) x}) (v-num 1225))

  (test-equal? "Correct scoping"
               (eval `{let (x0 0) (let (x1 1) (let (x2 2) (let (x3 3) (let (x4 4) (let (x5 5) (let (x6 6) (let (x7 7) (let (x8 8) (let (x9 9) (let (x10 10) (let (x11 11) (let (x12 12) (let (x13 13) (let (x14 14) (let (x15 15) (let (x16 16) (let (x17 17) (let (x18 18) (let (x19 19) (let (x20 20) (let (x21 21) (let (x22 22) (let (x23 23) (let (x24 24) (let (x25 25) (let (x26 26) (let (x27 27) (let (x28 28) (let (x29 29) (let (x30 30) (let (x31 31) (let (x32 32) (let (x33 33) (let (x34 34) (let (x35 35) (let (x36 36) (let (x37 37) (let (x38 38) (let (x39 39) (let (x40 40) (let (x41 41) (let (x42 42) (let (x43 43) (let (x44 44) (let (x45 45) (let (x46 46) (let (x47 47) (let (x48 48) (let (x49 x0) (+ x0 (+ x1 (+ x2 (+ x3 (+ x4 (+ x5 (+ x6 (+ x7 (+ x8 (+ x9 (+ x10 (+ x11 (+ x12 (+ x13 (+ x14 (+ x15 (+ x16 (+ x17 (+ x18 (+ x19 (+ x20 (+ x21 (+ x22 (+ x23 (+ x24 (+ x25 (+ x26 (+ x27 (+ x28 (+ x29 (+ x30 (+ x31 (+ x32 (+ x33 (+ x34 (+ x35 (+ x36 (+ x37 (+ x38 (+ x39 (+ x40 (+ x41 (+ x42 (+ x43 (+ x44 (+ x45 (+ x46 (+ x47 (+ x48 (+ x49 0 )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))}) (v-num 1176))
  ; in this case x49 is set to the value of x0
  ; 1225 - 49 = 1176
)

;; DO NOT EDIT BELOW THIS LINE =================================================

(module+ main (run-tests student-tests))
