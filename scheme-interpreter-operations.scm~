;; this file contains all operations
(load "scheme-interpreter-types.scm")
(load "scheme-interpreter-")

;; this is the function to start the interpreter
(define (interpret code)
  (define (eval-exp exp env)
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          ((quotation? exp) (text-of-quotation exp))
          ((assignment? exp) (eval-assignment exp env))
          ((definition? exp) (eval-definition exp env))
          ((if? exp) (eval-if exp env))
          ((lambda? exp)
           (make-procedure (lambda-parameters exp)
                           (lambda-body exp)
                           env))
          ((begin? exp) (eval-sequence (begin-actions exp) env))
          ((cond? exp) (eval (cond-to-if exp) env))
          ((application? exp)
           (apply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
          (else (error "Unknown expression type -- EVAL" exp))))
  (define global-env (setup-environment))
  (eval-exp code global-env))

;; these are the helper functions

; retrieves the value of a variable in a specific environment
(define (lookup-variable-value var env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((null? vars) (env-loop (enclosing-environment env)))
              ((eq? var (car vars)) (car vals))
              (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
          (error "Unbound variable" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame) (frame-values frame)))))
    (env-loop env))

; sets the specific variable a specific value in a given environment
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

; creates a new environment, extending the given one
; each environment has its own list of variables and values
(define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (make-frame vars vals base-env)
        (if (< (length vars) (length vals))
            (error "Too many arguments supplied" vars vals)
            (error "Too few arguments supplied" vars vals))))

; evaluates a list of expressions in a given environment
; each expression is mapped to its value in the environment
(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (cons (eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))

; applies a function to a list of arguments
(define (apply proc args)
  (cond ((primitive-procedure? proc) (apply-primitive-procedure proc args))
        ((compound-procedure? proc) (eval-sequence (procedure-body proc)
                                                   (extend-environment (procedure-parameters proc)
                                                                       args
                                                                       (procedure-environment proc))))
        (else (error "Unknown procedure type -- APPLY" proc))))

; primitive procedures
(define (apply-addition args)
    (if (null? args)
        0
        (+ (car args) (apply-addition (cdr args)))))
  
(define (apply-subtraction args)
  (if (null? (cdr args))
      (car args)
      (- (car args) (apply-subtraction (cdr args)))))
  
(define (apply-multiplication args)
  (if (null? args)
      1
      (* (car args) (apply-multiplication (cdr args)))))
  
(define (apply-division args)
  (if (null? (cdr args))
      (car args)
      (/ (car args) (apply-division (cdr args)))))

; compound procedure handlers
(define (make-procedure parameters body env)
  (cons 'procedure (cons parameters (cons body (cons env '())))))

; adds primitive procedures to the environment
(define (extend-primitive-procedures env)
    (define (extend-procedure proc)
      (define (dispatch-lambda-primitive-procedure x)
        (cond ((eq? x 'null?) (lambda (x) (null? x)))
              ((eq? x 'boolean?) (lambda (x) (boolean? x)))
              ((eq? x 'symbol?) (lambda (x) (symbol? x)))
              ((eq? x 'integer?) (lambda (x) (integer? x)))
              ((eq? x 'char?) (lambda (x) (char? x)))
              ((eq? x 'string?) (lambda (x) (string? x)))
              ((eq? x 'pair?) (lambda (x) (pair? x)))
              ((eq? x 'procedure?) (lambda (x) (procedure? x)))
              ((eq? x 'car) (lambda (x) (car x)))
              ((eq? x 'cdr) (lambda (x) (cdr x)))
              ((eq? x 'set-car!) (lambda (x y) (set-car! x y)))
              ((eq? x 'set-cdr!) (lambda (x y) (set-cdr! x y)))
              ((eq? x 'cons) cons)
              ((eq? x 'list) list)
              ((eq? x 'eq?) eq?)
              ((eq? x 'equal?) equal?)
              (else (error "Unknown primitive procedure -- DISPATCH-LAMBDA-PRIMITIVE-PROCEDURE"
                           proc))))
      (set-car! proc (dispatch-lambda-primitive-procedure proc)))
    (map extend-procedure '(null? boolean? symbol? integer? char?
                                  string? pair? procedure? car cdr
                                  set-car! set-cdr! cons list eq? equal?))
    env)

; add primitive constructors to the environment
(define (extend-primitive-constructors env)
      (define (extend-constructor proc)
        (define (dispatch-lambda-primitive-constructor x)
          (cond ((eq? x 'make-symbol) (lambda (x) (string->symbol x)))
                ((eq? x 'make-string) (lambda (x) (make-string x)))
                ((eq? x 'make-vector) (lambda (x) (make-vector x)))
                ((eq? x 'symbol->string) (lambda (x) (symbol->string x)))
                ((eq? x 'string->symbol) (lambda (x) (string->symbol x)))
                (else (error "Unknown primitive constructor -- DISPATCH-LAMBDA-PRIMITIVE-CONSTRUCTOR"
                             proc))))
        (set-car! proc (dispatch-lambda-primitive-constructor proc)))
      (map extend-constructor '(make-symbol make-string make-vector
                                symbol->string string->symbol))
      env)

; adds primitive I/O operations to the environment
(define (extend-primitive-I/O-procedures env)
        (define (extend-procedure proc)
          (define (dispatch-lambda-primitive-I/O-procedure x)
            (cond ((eq? x 'display) (lambda (x) (display x)))
              ((eq? x 'newline) (lambda () (newline)))
              ((eq? x 'read) (lambda () (read)))
              ((eq? x 'read-char) (lambda () (read-char)))
              ((eq? x 'peek-char) (lambda () (peek-char)))
              ((eq? x 'char-ready?) (lambda () (char-ready?)))
              (else (error "Unknown primitive I/O procedure -- DISPATCH-LAMBDA-PRIMITIVE-I/O-PROCEDURE"
                           proc))))
          (set-car! proc (dispatch-lambda-primitive-I/O-procedure proc)))
        (map extend-procedure '(display newline read read-char peek-char char-ready?))
    env)

; environment setup
(define (setup-environment)
  (let ((initial-env (extend-environment '() '() the-empty-environment)))
    (extend-primitive-procedures
     (extend-primitive-constructors
      (extend-primitive-I/O-procedures initial-env)))))