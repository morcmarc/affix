#lang racket/base

(provide registry-components
         set-registry-components!
         reg)

(struct registry ([components #:mutable #:auto])
  #:auto-value '())

(define reg (registry))
