#lang racket

(require "../affix/main.rkt"
         "foo.rkt"
         "bar.rkt")

(module* main #f
  (begin (start-all #f)
         (displayln (bar))))
