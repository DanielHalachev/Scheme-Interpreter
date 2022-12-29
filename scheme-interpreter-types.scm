;; this file represents all possible types of expressions

; self-evaluating expression
(define (self-evaluating? expression)
    (cond ((number? expression) #t)
          ((string? expression) #t)
          ((boolean? expression) #t)
          (else #f)))

; variable
(define (variable? exp)
  (symbol? exp))

; quotation
(define (quotation? exp) (eq? (car exp) 'quote))
  (define (text-of-quotation exp) (cadr exp))

; assignment
(define (assignment? exp)
  (eq? (car exp) 'set!))

(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

;; special forms
; define special form
(define (definition? exp)
  (eq? (car exp) 'define))

(define (definition-variable exp) (if (symbol? (cadr exp)) (cadr exp) (caadr exp)))
(define (definition-value exp) (if (symbol? (cadr exp)) (caddr exp) (make-lambda (cdadr exp) (cddr exp))))
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

; if special form
(define (if? exp)
  (eq? (car exp) 'if))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp) (if (not (null? (cdddr exp))) (cadddr exp) 'false))
(define (eval-if exp env)
  (if (eval (if-predicate exp) env)
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; lambda
(define (lambda? exp)
  (eq? (car exp) 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))
(define (make-lambda parameters body env)
  (lambda (env)
    (lambda args
      (eval (make-procedure parameters body env)
            (extend-environment parameters args env)))))

; begin
(define (begin? exp)
  (eq? (car exp) 'begin))
(define (begin-actions exp)
  (cdr exp))
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
                (eval-sequence (rest-exps exps) env))))
; cond
(define (cond? exp) (eq? (car exp) 'cond))
  (define (cond-clauses exp) (cdr exp))
  (define (cond-to-if exp)
    (if (null? exp) 'false
        (let ((clause (car exp)))
          (if (eq? (car clause) 'else)
              (if (null? (cdr clause)) 'true (cons 'begin (cdr clause)))
              (cons 'if (cons (car clause) (cdr clause)))))))

; application of an operation
(define (application? exp) (pair? exp))

; expression parts
(define (last-exp? exps) (null? (cdr exps)))
(define (first-exp exps) (car exps))
(define (rest-exps exps) (cdr exps))

; operators
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; environments - lists
(define (the-empty-environment) '())
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define (make-frame variables values base-env)
  (cons (cons variables values) base-env))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

; procedures - primitive and compound
(define (primitive-procedure? proc) (primitive-procedure-names proc))
(define (primitive-procedure-names proc)
  (member proc '(+ - * /)))
(define (apply-primitive-procedure proc args)
  (cond ((eq? proc '+) (apply-addition args))
        ((eq? proc '-) (apply-subtraction args))
        ((eq? proc '*) (apply-multiplication args))
        ((eq? proc '/) (apply-division args))
        (else (error "Unknown operator -- APPLY-PRIMITIVE-PROCEDURE" proc))))

(define (compound-procedure? proc)
    (tagged-list? proc 'procedure))
(define (procedure-parameters proc) (cadr proc))
(define (procedure-body proc) (caddr proc))
(define (procedure-environment proc) (cadddr proc))