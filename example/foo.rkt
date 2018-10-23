#lang racket

(require "../affix/main.rkt")

(provide foo)

(defstate foo
  #:start (λ (state args)
            (displayln "starting foo")
            "I am foo"))
