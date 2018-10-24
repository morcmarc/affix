#lang racket/base

(provide registry-states
         set-registry-states!
         reg)

(struct registry ([states #:mutable #:auto])
  #:auto-value (list))

(define reg (registry))
