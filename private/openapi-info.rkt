#lang racket/base

(provide openapi-info)

(define (openapi-info title (desc "") #:version (version "1.0.0"))
  (let ([Info (make-hash `((title . ,title) (version . ,version) ))])
    (unless (equal? desc "") (hash-set! Info 'description desc))
    
    Info))