#lang racket/base
(require "types.rkt" (for-syntax racket/syntax racket/base) )
(provide (all-defined-out))

(define-syntax (define-response stx)
  (syntax-case stx ()
    [(_ NUM) (with-syntax
                 ([NUM: (format-id #'NUM "~a:" (syntax->datum #'NUM))])
               #'(define (NUM: desc (body '()) #:headers (headers '())) (response NUM  desc body #:headers headers) ))]))

(define-syntax-rule (define-responses CODE ...)
  (begin (define-response CODE) ...))

(define-responses 100 101 102 103)
(define-responses 200 202 203 204 205 206 207 208 226)
(define-responses 300 301 302 303 304 305 306 307 308)
(define-responses 400 401 402 403 404 405 406 407 408 409 410 411 412 413 414 415 416 417 421 422 423 424 425 426 427 428 429 430 451 )
(define-responses 500 501 502 503 504 505 506 507 508 509 510)


(define (response code desc #:headers (headers '()) (body '()))
  (let-values ([(root content) (make-key-desc (string->symbol (number->string code)) desc)])
    (unless (null? body) (set-body! content body))
    (unless (null? headers)
      (let ([~headers (make-hash)])
        (hash-set! content 'headers ~headers)
        (for ([kv headers])
          (hash-set! ~headers (car kv)  (make-hash `((schema . ,(type-> (cadr kv)))))))))
    root))


(define (201: desc)
  (response 201 desc #:headers `((Content-Location ,:string))  '()))
