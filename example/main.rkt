#lang racket

(require affix
         "config.rkt"
         "server.rkt")

(module* main #f
  (begin (start-all #hash(("config-file" . "config.yml")))
         (server)))
