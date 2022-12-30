#lang r5rs

(define (zip list1 list2)
  (cond ((null? list1) '())
        ((null? list2) '())
        (else (cons (cons (car list1) (car list2)) ( zip (cdr list1) (cdr list2))))))

;(define (error cause . arguments)
;  (display "Error: ")
;  (display cause)
;  (apply display " " (map write arguments))
;  (newline))

(define default-environment '())

(define (setup-procedure name value)
  (let ((old (assoc name default-environment)))
    (if old
        (set-car! (cdr old) value)
        (set! default-environment
              (cons (list name value) default-environment)))))

(setup-procedure '+ +)
(setup-procedure '- -)
(setup-procedure '* *)
(setup-procedure '/ /)
(setup-procedure '< <)
(setup-procedure '> >)
(setup-procedure '= =)
(setup-procedure '<= <=)
(setup-procedure '>= >=)
(setup-procedure 'not not)
(setup-procedure 'null? null?)
(setup-procedure 'even? even?)
(setup-procedure 'number? number?)
(setup-procedure 'boolean? boolean?)
(setup-procedure 'equal? equal?)
(setup-procedure 'list? list?)
(setup-procedure 'floor floor)
(setup-procedure 'ceiling ceiling)
(setup-procedure 'quotient quotient)
(setup-procedure 'remainder remainder)
(setup-procedure 'modulo modulo)
(setup-procedure 'cons cons)
(setup-procedure 'car car)
(setup-procedure 'cdr cdr)
(setup-procedure 'append append)


(define (get-from-environment name)
  (cadr (assoc name default-environment)))

(define (set-in-environment name value)
  (set-car! (cdr (assoc name default-environment))
            value))

(define (evaluate-define expression environment)
  (if (pair? (cadr expression))
      (setup-procedure (caadr expression)
                       (evaluate (cons 'lambda (cons (cdadr expression) (cons (cddr expression) '() ))) environment))
      (setup-procedure (cadr expression)
                       (evaluate (caddr expression) environment))))
  
(define (evaluate-set! expression environment)
  (set-in-environment (cadr expression)
                      (evaluate (caddr expression) environment)))

(define (special-form? expression)
  (member expression '(cond list and or values define set! quote)))

(define (built-in-procedure? expression)
  (member expression '(if cond car list? and or values cdr map quote foldr lambda define set!)))

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


(define (evaluate-and expression environment)
  (if (null? (cdr expression))
      #t
      (and (evaluate (cadr expression) environment)
           (evaluate-and (cdr expression) environment))))

(define (evaluate-or expression environment)
  (if (null? (cdr expression))
      #f
      (or (evaluate (cadr expression) environment)
          (evaluate-or (cdr expression) environment))))

(define (evaluate-list expression environment)
  (if (null? (cdr expression))
      '()
      (cons (evaluate (cadr expression) environment)
            (evaluate-list (cdr expression) environment))))

(define (evaluate-values expression environment)
  (define result
    (evaluate-list expression environment))
  (apply values result))

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