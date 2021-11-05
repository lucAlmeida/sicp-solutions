;; If the interpreter is using applicative-order evaluation, then the execution of the expression (test 0 (p)) would always yield an infinite loop.
;; The reason for this is that, in applicative-order evaluation, the arguments need to be evaluated and reduced to primitive values.
;; Since the procedure p is a recursive call to itself without a condition for finishing the procedure execution, the program would execute forever.

;; In contrast to the behavior above for applicative-order evaluation interpreter, the normal-order evaluation interpreter would proceed to the substitution
;; process of the formal parameters in the body of the procedure, with the arguments as is. After substitution taking place, the condition of the if clause
;; would be evaluated, which in turn would return the value 0, since the value of the condition is true.

;; Conclusion:
;; If the interpreter is using applicative-order evaluation, the execution of the program would never stop.
;; Otherwise, if the interpreter is using normal-order evaluation, the procedure call would return the value 0.
