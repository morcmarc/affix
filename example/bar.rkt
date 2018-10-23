#lang racket

(require "../affix/main.rkt"
         "foo.rkt")

(provide bar)

(defstate bar
  #:start (λ (state args)
            (displayln "starting bar")
            (displayln (foo))
            "I am bar"))
