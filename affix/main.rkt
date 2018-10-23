#lang racket

(require "state.rkt"
         "base.rkt")

(provide (all-from-out "state.rkt")
         (all-defined-out))

(define (start-all args)
  (for-each
   (λ (component)
     (start pcomponent args))
     (registry-components reg)))
