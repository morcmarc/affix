#lang racket

(require racket/generic
         (for-syntax racket/base syntax/parse))

(define-generics state
  [start state arg]
  [stop state])

(define-syntax (defstate stx)
  (syntax-parse stx
    [(defstate name:id
       (~alt (~optional (~seq #:start start-fn*)
                        #:defaults ([start-fn* #'#f]))
             (~optional (~seq #:stop stop-fn*)
                        #:defaults ([stop-fn* #'#f])))
       ...)
     #:with fn-start
     #'(define (start state args)
         (if start-fn*
             (start-fn* state args)
             "starting"))
     #:with fn-stop
     #'(define (stop state)
         (if stop-fn*
             (stop-fn* state)
             "stopping"))
     #'(struct name ()
         #:methods gen:state
         [fn-start
          fn-stop])]))

(module+ test
  (require rackunit)

  (defstate my-test
    #:start (lambda (state args) "custom starting")
    #:stop (lambda (state) "custom stopping"))
  
  (defstate my-test-2)
  
  (define m-1 (my-test))
  
  (define m-2 (my-test-2))

  (check-eq? "custom stopping" (stop m-1))
  (check-eq? "custom starting" (start m-1 #f))

  (check-eq? "stopping" (stop m-2))
  (check-eq? "starting" (start m-2 #f)))
