
(define ax (new_ClVariable 0))
(define ay (new_ClVariable 0))
(define bx (new_ClVariable 0))
(define by (new_ClVariable 0))

(ClVariable_value ax)

(define expr-ax (new_ClLinearExpression ax 1 0))
(define expr-ay (new_ClLinearExpression ay 1 0))
(define expr-bx (new_ClLinearExpression bx 1 0))
(define expr-by (new_ClLinearExpression by 1 0))

(define solver (new_ClSimplexSolver))

(define edit-ax (new_ClEditConstraint ax (clsStrong) 1.0))
(define edit-ay (new_ClEditConstraint ay (clsStrong) 1.0))

(define ax-equals-bx (new_ClLinearEquation2 expr-ax bx (clsRequired) 1.0))
(define ay-equals-by (new_ClLinearEquation2 expr-ay by (clsRequired) 1.0))


(ClSimplexSolver_addConstraint solver ax-equals-bx)
(ClSimplexSolver_addConstraint solver ay-equals-by)

(ClSimplexSolver_addConstraint solver edit-ax)
(ClSimplexSolver_addConstraint solver edit-ay)

(ClVariable_value ax)
(ClVariable_value ay)
(ClVariable_value bx)
(ClVariable_value by)

(ClSimplexSolver_resolveXY solver 1 2)

(ClVariable_value ax)
(ClVariable_value ay)
(ClVariable_value bx)
(ClVariable_value by)
