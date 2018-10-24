# Affix

Affix is a tool for managing "application state" in Racket applications, akin to [mount](https://github.com/tolitius/mount) in Clojure.

*Work-in-progress.*

## Guide

### Creating States

The following will create a new locally bound *state*:

```racket
;; db-connection.rkt

#lang racket

(require affix)

(defstate db-connection
  #:start (lambda (state args)
            ;; connect to db and return connection)
  #:stop (lambda (state)
           ;; terminate db connection) 
```

To expose a *state* to other modules you can use `provide` like you normally would:

```racket
;; db-connection.rkt

(provide db-connection)
```

### Using States

Defining a *state* does not start it. It will be added to an internal registry that needs to be started first. 

```racket
;; main.rkt

#lang racket

(require "db-connection.rkt"
         affix)

(module+ main #f
  (start-all (current-command-line-arguments)))
```

`start-all` calls `start` on each *state* and wraps the result in a `delay/sync` (i.e promise).

### Accessing States

Now that we have started our *states*, we can access their encapsulated value by calling it as a function:

```racket
;; foo.rkt

#lang racket

(require "db-connection.rkt")

(define (hello-world)
  (run-query (db-connection) "SELECT * FROM foo"))
```

This will `force` the wrapped state to evaluate and return.
