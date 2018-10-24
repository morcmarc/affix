#lang racket

(require racket/contract
         "state.rkt"
         "base.rkt")

(provide (all-from-out "state.rkt")
         (contract-out
          [start-all (-> (or/c list? false?) any)]
          [stop-all (-> any)]))

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
