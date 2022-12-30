#lang racket
;; this is the function to start the interpreter

(load "scheme-interpreter-operations.scm")
(load "scheme-interpreter-types.scm")

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

(define code1 '(+ 1 2))

