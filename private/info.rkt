#lang racket/base

(provide (all-defined-out))

(define (info title (desc "") #:version (version "1.0.0"))
  (let ([Info (make-hash `((title . ,title) (version . ,version) ))])
    (unless (equal? desc "") (hash-set! Info 'description desc))
    
    Info))