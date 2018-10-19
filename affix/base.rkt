#lang racket

(require racket/generic
         (for-syntax racket/base syntax/parse))


(define-generics state
  [start state arg]
  [stop state])

(define-syntax (defstate stx)
  (syntax-parse stx
    #:datum-literals (==>)
    [(_ name:id
        (~or  (~once (~optional (~seq #:start start-fn*:expr)))
              (~once (~optional (~seq #:stop stop-fn*:expr))))
        ...)
     #'(struct name ()
         #:methods gen:state
         [(define (start state args) (start-fn* state args))
          (define (stop state) (stop-fn* state))])]))

(module+ test
  (require rackunit)

  (defstate my-test
    #:start (lambda (state args) "custom starting")
    #:stop (lambda (state) "custom stopping"))
  (defstate my-test-2
    #:stop (lambda (state) "stopping")
    #:start (lambda (state args) "starting"))
  ;; (defstate my-test-2)
  
  (define m-1 (my-test))
  (define m-2 (my-test-2))

  (check-eq? "custom stopping" (stop m-1))
  (check-eq? "custom starting" (start m-1 #f))
  (check-eq? "stopping" (stop m-2))
  (check-eq? "starting" (start m-2 #f)))
