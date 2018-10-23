#lang racket

(require racket/generic
         (for-syntax syntax/parse
                     racket/syntax)
         "base.rkt")

(provide gen:state
         state?
         state/c
         defstate
         start
         stop)

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

     #:with set-lifecycle-fn
     (format-id #'name "set-state-~a-lifecycle!" #'name)

     #:with get-lifecycle-fn
     (format-id #'name "state-~a-lifecycle" #'name)

     #:with fn-start
     #'(define (start state args)
         (if (not (eq? (get-lifecycle-fn state) 'started))
             (if start-fn*
                 (let ([val (start-fn* state args)])
                   (set-val-fn state val)
                   (set-lifecycle-fn state 'started)
                   val)
                 (begin
                   (set-lifecycle-fn state 'started)
                   #t))
             (raise-user-error (format "already started: ~a" 'name))))

     #:with fn-stop
     #'(define (stop state)
         (if (not (eq? (get-lifecycle-fn state) 'stopped))
             (if stop-fn*
                 (let ([val (stop-fn* state)])
                   (set-val-fn state #f)
                   (set-lifecycle-fn state 'stopped)
                   val)
                 (begin (set-lifecycle-fn state 'stopped)
                        #t))
             (raise-user-error (format "already stopped: ~a" 'name))))

     #:with register
     #'(when main-state
         (set-affix-components! main-state (append (affix-components main-state) '(name))))

     #'(begin
         (struct struct-name ([val #:auto #:mutable]
                              [lifecycle #:auto #:mutable])
           #:methods gen:state
           [fn-start
            fn-stop])
         (define name (struct-name))
         register)]))

(module+ test
  (require rackunit)

  (test-case "start-stop methods"
    (defstate my-test
      #:start (lambda (state args) "custom starting")
      #:stop (lambda (state) "custom stopping"))
    
    (defstate my-test-2)
    
    (check-eq? "custom stopping" (stop my-test))
    (check-eq? "custom starting" (start my-test #f))

    (check-eq? #t (stop my-test-2))
    (check-eq? #t (start my-test-2 #f)))

  (test-case "cannot be started twice"
    (defstate my-test-3)
    (start my-test-3 #f)
    (check-exn exn:fail:user? (start my-test-3 #f)))

  (test-case "cannot be stopped twice"
    (defstate my-test-4)
    (start my-test-4 #f)
    (stop my-test-4)
    (check-exn exn:fail:user? (stop my-test-4)))

  ;; @TODO: too brittle, should reset the global state before running the test
  (test-case "registers component"
    (check-eq? 4 (length (affix-components main-state))))
  
  (test-case "calling start shoud set the encapsulate value"
    (defstate my-test-5
      #:start (lambda (state args) 'foo))
    
    (start my-test-5 #f)
    (check-eq? (state-my-test-5-val my-test-5) 'foo)))