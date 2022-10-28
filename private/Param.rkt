#lang typed/racket/base

(provide     (struct-out Param))

(require typed/json
         racket/list
         typed/net/url
         "json-schema.rkt"
         "utils.rkt")

 

(define-type Param-In (U 'query 'path 'header 'cookie))

(struct Param
  ([name : Symbol]
   [schema : Schema]
   [in : Param-In]
   [required? : Boolean]
   [description : String]
   )
  #:transparent)

(define (Param->hash)
 
(define (string->Param-In [in : String]) : Param-In
  (case in
    [("path") 'path]
    [("header") 'header]
    [("query") 'query]
    [("cookie") 'cookie]
    [else (error (format "Expected one of 'query', 'path', 'header', 'cookie. Given ~a"
                         in))]))

   

