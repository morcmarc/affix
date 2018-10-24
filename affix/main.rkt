#lang racket

(require racket/contract
         "state.rkt"
         "base.rkt")

(provide (all-from-out "state.rkt")
         (all-defined-out))

(module+ test
  (require rackunit))

(define (start-all args)
  (start (registry-states reg) args))

(define (stop-all)
  (stop (registry-states reg)))

(define (start states args)
  (for-each
   (λ (s)
     (start-state s args))
   states))

(define (stop states)
  (for-each
   (λ (s)
     (stop-state s))
   states))
