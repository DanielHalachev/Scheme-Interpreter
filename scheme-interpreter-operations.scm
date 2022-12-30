#lang r5rs
(#%require "scheme-interpreter-operations.scm")
(#%provide (all-defined))

;; this part contains all auxilliary functions

;; basic types and checks
; default environment definition
(define default-environment '())

; special form check
(define (special-form? expression)
  (member expression '(cond list and or values define set! quote)))

; built-in procedure check
(define (built-in-procedure? expression)
  (member expression '(if cond car list? and or values cdr map quote foldr lambda define set!)))

;; environment setup and operations
; get from environment
(define (get-from-environment name)
  (cadr (assoc name default-environment)))

; set value in environment
(define (set-in-environment name value)
  (set-car! (cdr (assoc name default-environment))
            value))

; bind a procedure with its value in the default environment
(define (setup-procedure name value)
  (let ((old (assoc name default-environment)))
    (if old
        (set-car! (cdr old) value)
        (set! default-environment
              (cons (list name value) default-environment)))))

(setup-procedure 'not not)
(setup-procedure 'even? even?)
(setup-procedure 'odd? odd?)
(setup-procedure 'number? number?)
(setup-procedure 'boolean? boolean?)
(setup-procedure 'equal? equal?)
(setup-procedure 'eq? eq?)

(setup-procedure 'null? null?)
(setup-procedure 'list? list?)
(setup-procedure 'car car)
(setup-procedure 'cdr cdr)
(setup-procedure 'append append)
(setup-procedure 'cons cons)

(setup-procedure 'quotient quotient)
(setup-procedure 'remainder remainder)

(setup-procedure '+ +)
(setup-procedure '- -)
(setup-procedure '* *)
(setup-procedure '/ /)
(setup-procedure '< <)
(setup-procedure '> >)
(setup-procedure '= =)
(setup-procedure '<= <=)
(setup-procedure '>= >=)

; define/set!/and/or/list/quote/cond evaluation redirection
(define (evaluate-special-form form environment)
  (let ((name (car form)))
    (case name
      ((define) (evaluate-define form environment))
      ((set!) (evaluate-set! form environment))
      ((and) (evaluate-and form environment))
      ((or) (evaluate-or form environment))
      ((list) (evaluate-list form environment))
      ((quote) (evaluate-list (append '(list) (cadr form)) environment))
      ((values) (evaluate-values form environment))
      ((cond) (evaluate-cond form environment)))))

; define evaluation
(define (evaluate-define expression environment)
  (if (pair? (cadr expression))
      (setup-procedure (caadr expression)
                       (evaluate (cons 'lambda (cons (cdadr expression) (cons (cddr expression) '() ))) environment))
      (setup-procedure (cadr expression)
                       (evaluate (caddr expression) environment))))
; set! evaluation
(define (evaluate-set! expression environment)
  (set-in-environment (cadr expression)
                      (evaluate (caddr expression) environment)))

; and evaluation
(define (evaluate-and expression environment)
  (if (null? (cdr expression))
      #t
      (and (evaluate (cadr expression) environment)
           (evaluate-and (cdr expression) environment))))

; or evaluation
(define (evaluate-or expression environment)
  (if (null? (cdr expression))
      #f
      (or (evaluate (cadr expression) environment)
          (evaluate-or (cdr expression) environment))))

; list-evaluation
(define (evaluate-list expression environment)
  (if (null? (cdr expression))
      '()
      (cons (evaluate (cadr expression) environment)
            (evaluate-list (cdr expression) environment))))

; values evaluation
(define (evaluate-values expression environment)
  (define result
    (evaluate-list expression environment))
  (apply values result))

; cond evaluation
(define  (evaluate-cond expression environment)
  (let (
        (case (caadr expression))
        (action (cadadr expression)))
    (cond ((eq? case 'else)
           (evaluate action environment))
          ((evaluate case environment)
           (evaluate action environment))
          (else (evaluate (cons (car expression) (cddr expression))
                          environment)))))

; helper function
(define (zip list1 list2)
  (cond ((null? list1) '())
        ((null? list2) '())
        (else (cons (cons (car list1) (car list2)) ( zip (cdr list1) (cdr list2))))))
