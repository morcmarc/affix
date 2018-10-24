#lang racket

(require racket/generic
         (for-syntax syntax/parse
                     racket/syntax)
         "base.rkt")

(provide gen:state
         state?
         state/c
         defstate
         start-state
         stop-state)

(define-generics state
  [start-state state arg]
  [stop-state state])

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

     #:with get-val-fn
     (format-id #'name "state-~a-val" #'name)
     
     #:with set-lifecycle-fn
     (format-id #'name "set-state-~a-lifecycle!" #'name)

     #:with get-lifecycle-fn
     (format-id #'name "state-~a-lifecycle" #'name)

     #:with fn-start
     #'(define (start-state state args)
         (if (not (eq? (get-lifecycle-fn state) 'started))
             (if start-fn*
                 (let ([val (delay/sync (start-fn* state args))])
                   (set-val-fn state val)
                   (set-lifecycle-fn state 'started)
                   val)
                 (begin
                   (set-lifecycle-fn state 'started)
                   #t))
             (raise-user-error (format "already started: ~a" 'name))))

     #:with fn-stop
     #'(define (stop-state state)
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
     #'(when reg
         (set-registry-states! reg (append (registry-states reg) (list name))))

     #'(begin
         (struct struct-name ([val #:auto #:mutable]
                              [lifecycle #:auto #:mutable])

           #:methods gen:state
           [fn-start
            fn-stop]

           #:property prop:procedure
           (lambda (self)
             (let ([val (get-val-fn self)])
               (if (promise? val)
                   (force val)
                   val))))

         (define name (struct-name))
         register)]))

(module+ test
  (require rackunit)

  (test-case "cannot be started twice"
    (defstate my-test-3)
    (start-state my-test-3 #f)
    (check-exn exn:fail:user? (start-state my-test-3 #f)))

  (test-case "cannot be stopped twice"
    (defstate my-test-4)
    (start-state my-test-4 #f)
    (stop-state my-test-4)
    (check-exn exn:fail:user? (stop-state my-test-4)))

  ;; @TODO: too brittle, should reset the global state before running the test
  (test-case "registers components"
    (check-eq? 2 (length (registry-states reg))))
  
  (test-case "calling start shoud set the encapsulated value"
    (defstate my-test-5
      #:start (lambda (state args) 'foo))
    
    (start-state my-test-5 #f)
    (let ([val (state-my-test-5-val my-test-5)])
      (check-true (promise? val))
      (check-eq? (force val) 'foo)))

  (test-case "the resulting state can be called as a function and it returns the encapsulated value"
    (defstate my-test-6
      #:start (lambda (state args) 'bar))
    (start-state my-test-6 #f)
    (check-eq? (my-test-6) 'bar)))
