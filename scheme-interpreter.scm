(#%require "scheme-interpreter-operations.scm")
(#%provide (all-defined))
;; this is the function to start the interpreter

(define (interpret code)
  (evaluate (append '(values) code)
            (lambda (object) (get-from-environment object))))

(define evaluate
  (lambda (expression environment)
    (cond
      ; number or bool -> return it
      ((or (number? expression)
           (boolean? expression))
       expression)
      ; (special-form-name . args) -> evaluate special form depending on its name
      ((and
        (pair? expression)
        (special-form? (car expression)))
       (evaluate-special-form expression environment))
      ; ((not-lambda . ?) '(...))
      ((and (pair? expression)
            (pair? (car expression))
            (not (eq? (caar expression) 'lambda))
            (not(null? (cdr expression))))
       (if (member (caar expression) '(define set!))
           (begin
             (evaluate (car expression) environment)
             (evaluate (cdr expression) environment))
           (
            (evaluate (car expression) environment)
            (evaluate (cadr expression) environment))))
      ; (( . )) -> evaluate ( . )
      ((and (pair? expression)
            (pair? (car expression))
            (null? (cdr expression)))
       (evaluate (car expression) environment))
      ; (f . args) -> evaluate args and then evaluate (f args)
      ((and (pair? expression)
            (symbol? (car expression))
            (not (built-in-procedure? (car expression)))
            (procedure? (environment (car expression))))
       (apply (environment (car expression))
              (map (lambda (expression)
                     (evaluate expression environment))
                   (cdr expression))))
      ; '(...)
      ((list? expression)
       (case (car expression)
         ; '(if p? t f) -> evaluate p? -> evaluate t or f based on the righteouness of p?
         ((if)
          (if (evaluate (cadr expression) environment)
              (evaluate (caddr expression) environment)
              (evaluate (cadddr expression) environment)))
         ; '(map f l) -> evaluate f and apply a map to the evaluation of l 
         ((map)
          (map (evaluate (cadr expression) environment)
               (evaluate (caddr expression) environment)))
         ; '(lambda params body) -> lambda-env:= (zip params args); evaluate body in the environment 
         ((lambda)
          (lambda args
            (let ((lambda-env (zip (cadr expression) args)))
              (evaluate (caddr expression) (lambda (third)
                                             (if (assoc third lambda-env)
                                                 (cdr (assoc third lambda-env))
                                                 (environment third)))))))
         ; '(operator operand) -> evaluate each item
         (else
          ((evaluate (car expression) environment)
           (evaluate (cadr expression) environment)))))
      (else (environment expression)))))