#lang racket

(require racket/generic)

(provide gen:state state/c state?)

(define-generics state
  [start state arg]
  [stop state])

;; (define-syntax (defstate stx)
;;   (let* ([xs (syntax->list stx)]
;;          [name (cadr xs)])
;;     (datum->syntax stx `(struct (unquote name) ()
;;                           #:methods gen:state
;;                           [(define (start state args)
;;                              "starting")
;;                            (define (stop state)
;;                              "stopping")]))))

(define-syntax-rule (defstate name start-fn stop-fn)
  (struct name ()
    #:methods gen:state
    [(define (start state args)
       (start-fn state args))
     (define (stop state)
       (stop-fn state))]))

(module+ test
  (require rackunit)

  (defstate my-test
    (lambda (state args) "starting")
    (lambda (state) "stopping"))
  (define m (my-test))

  (check-eq? "starting" (start m #f))
  (check-eq? "stopping" (stop m)))
