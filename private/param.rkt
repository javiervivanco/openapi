#lang racket/base

(require "types.rkt")

(provide (all-defined-out))
;; falta scheme del tipo de param
(define (param name in (descr '()) (schema-type '()) #:required? (r #t))
  (define p
    (make-hasheq
     `((name . ,(symbol->string name))
       (in . ,(symbol->string in))
       (required . ,r))))
  (unless (null? descr) (hash-set! p 'description descr))
  (unless (null? schema-type) (hash-set! p 'schema (type-> schema-type)))
  p)

(define (&query name (descr '()) (schema '()) #:required? (r #t))
  (param name 'query descr schema #:required? r))

(define (&path name  (descr '()) (schema '()) #:required? (r #t))
  (param name 'path descr schema #:required? r))

(define (&header name (descr '()) (schema '()) #:required? (r #t))
  (param name 'header descr schema #:required? r))

(define (&cookie name (descr '()) (schema '()) #:required? (r #t))
  (param name 'cookie descr schema #:required? r))


