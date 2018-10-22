#lang racket

(require racket/generic
         (for-syntax syntax/parse
                     racket/syntax))

(provide gen:state state? state/c defstate)

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
     #:with struct-name
     (format-id #'name "state-~a" #'name)
     #:with set-val-fn
     (format-id #'name "set-state-~a-val!" #'name)
     #:with fn-start
     #'(define (start state args)
         (if start-fn*
             (let ([val (start-fn* state args)])
               (set-val-fn state val)
               val)
             "starting"))
     #:with fn-stop
     #'(define (stop state)
         (if stop-fn*
             (stop-fn* state)
             "stopping"))
     #'(begin
         (struct struct-name ([val #:auto #:mutable])
           #:methods gen:state
           [fn-start
            fn-stop])
         (define name (struct-name)))]))

(module+ test
  (require rackunit)

  (test-case "start-stop methods"
    (defstate my-test
      #:start (lambda (state args) "custom starting")
      #:stop (lambda (state) "custom stopping"))
    
    (defstate my-test-2)
    
    (check-eq? "custom stopping" (stop my-test))
    (check-eq? "custom starting" (start my-test #f))

    (check-eq? "stopping" (stop my-test-2))
    (check-eq? "starting" (start my-test-2 #f)))

  
  (test-case "calling start shoud set the encapsulate value"
    (defstate my-test
      #:start (lambda (state args) 'foo))
    
    (start my-test #f)
    (check-eq? (state-my-test-val my-test) 'foo)))
