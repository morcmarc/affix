#lang racket

(require affix
         web-server/servlet
         web-server/servlet-env
         "config.rkt")

(provide server)

(define (handler req)
  (response/xexpr
   `(html (head (title "Hello World"))
          (body (h1 "Hi there!")))))

(defstate server
  #:start (λ (state args)
            (let ([port (hash-ref (config) "port")])
              (displayln (string-append "Starting server on port " (number->string port)))
              (serve/servlet handler #:port port))))
