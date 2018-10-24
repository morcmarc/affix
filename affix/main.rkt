#lang racket

(require racket/contract
         "state.rkt"
         "base.rkt")

(provide (all-from-out "state.rkt")
         start-all
         stop-all)

(define (start-all args)
  (for-each
   (λ (component)
     (start component args))
   (registry-components reg)))

(define (stop-all)
  (for-each
   (λ (component)
     (stop component))
   (registry-components reg)))
