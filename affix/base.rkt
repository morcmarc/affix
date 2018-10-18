#lang racket

(require racket/generic
         (for-syntax racket/base
                     syntax/parse))

(provide gen:state state/c state?)

(define-generics state
  [start state arg])

;; (define-syntax (defstate stx)
;;   (let* ([xs (syntax->list stx)]
;;          [name (cadr xs)])
;;     (datum->syntax stx `(struct (unquote name) ()
;;                           #:methods gen:state
;;                           [(define (start state args)
;;                              "starting")
;;                            (define (stop state)
;;                              "stopping")]))))

(define-syntax (defstate stx)
  (syntax-parse stx
    [(defstate id (~optional (~seq #:start start-fn:expr)))
     (with-syntax ([start-fn? (if (attribute start-fn) #'#t #'#f)]
                   [start-fn (or (attribute start-fn) #'#f)])
       #'(defstate* id start-fn? start-fn))]))

(define-syntax defstate*
  (syntax-rules ()
    [(defstate id #t start-fn)
     (struct id ()
       #:methods gen:state
       [(define (start state args)
          (start-fn state args))])]
    [(defstate id #f _)
     (struct id ()
       #:methods gen:state
       [(define (start state args)
          "starting")])]
    [(defstate id _)
     (struct id ())]))

(module+ test
  (require rackunit)

  (defstate my-test
    #:start (lambda (state args) "custom starting"))
  (defstate my-test-2)
  
  (define m-1 (my-test))
  (define m-2 (my-test-2))

  (check-eq? "custom starting" (start m-1 #f))
  (check-eq? "starting" (start m-2 #f)))
