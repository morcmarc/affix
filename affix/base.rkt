#lang racket/base

(provide affix
         affix-components
         set-affix-components!
         main-state)

(struct affix ([components #:mutable #:auto])
  #:auto-value '())

(define main-state (affix))
