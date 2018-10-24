#lang racket

(require affix
         yaml)

(provide config)

(defstate config
  #:start (λ (state args)
            (let ([config-file (hash-ref args "config-file")])
              (displayln (string-append "Reading config file: " config-file))
              (file->yaml config-file))))
