#lang racket/base

(require "openapi.rkt" "json-schema.rkt" racket/list json)
(provide API: API->json
         route
         response
         :Object
         ::Object
         :String
         :Integer
         :Number
         :Boolean
         :ArrayOf
         :@
         %query
         %path
         %header
         %cookie
         Body
         define-entity
         define-Object
         GET POST PUT DELETE
         values-Objects
         (all-from-out "openapi.rkt"))

(define :String  (Schema-String))
(define :Integer (Schema-Integer))
(define :Number  (Schema-Number))
(define :Boolean (Schema-Boolean))

(define-syntax-rule (:@ (NAME TYPE) ...)
  (list (Property 'NAME TYPE) ... ))

(define (Body . args )
  (map (λ (e) (Property (car e) (cdr e) )) args))

(define (:Object name properties )
  (Schema-Object name properties ))

(define (::Object name . properties )
  (Schema-Object name properties ))

(define-syntax-rule (define-Object SCHEMA PLURAL (NAME TYPE) ...)
  (begin
    (define SCHEMA (Schema-Object 'SCHEMA (list (Property 'NAME TYPE) ... )))
    (define PLURAL (:Object 'PLURAL (:@ (PLURAL (:ArrayOf SCHEMA)))
                            ))))
(define (values-Objects SCHEMA PLURAL props)
  (let ([schema (Schema-Object SCHEMA props)])
    (values
     schema
     (:Object PLURAL (list (Property PLURAL (:ArrayOf schema)))))))

(define-syntax-rule (define-entity (SCHEMA PLURAL) PROPS)
  (define-values (SCHEMA PLURAL) (values-Objects 'SCHEMA 'PLURAL PROPS)))

(define (:ArrayOf schema) (Schema-Array schema))

(define PATH (make-parameter '(no-path)))
(define ~ARGS (make-parameter '()))

(define (route:  #:security-requirement [security-requirement #f]
                 #:method method . args)
  (let-values ([(body parameters responses desc op-ip) (body-parameters-responses (flatten args))]
               [(path0) (let ([~path (filter (λ (e) (or (string? e) (symbol? e))) args)])
                          (cond [(null? ~path) PATH]
                                [else    (λ () ~path)]))])
    (λ () (let ([path (path0)])
            (Route
             (if (symbol? path) (symbol->string path) path)
             method
             security-requirement
             body
             parameters
             responses)))))

(define (GET  #:security-requirement [security-requirement #f] .  args)
  (apply route: (append (~ARGS) args) #:method 'get #:security-requirement security-requirement))


(define (process-args args )
  (values (filter Property?  args)
          (filter Param?     args)
          (filter Response?  args)
          (let ([desc (filter string?  args)]) (if (null? desc) "" (car desc)))
          (let ([op-id (filter symbol?  args)]) (if (null? op-id) (gensym) (car op-id)))
          ))

(define (route
         #:security-requirement [security-requirement #f]
         #:desc (desc '())
         path
         . args)
  (parameterize ([PATH path]
                 [~ARGS args])
    (let ([routes (filter procedure? args)])
      (cond [(null? routes) (list ((GET #:security-requirement security-requirement)))]
            [else (map (λ (p) (p)) routes )]))))

(define (%query name schema)
  (Param name schema 'query))

(define (%path name schema)
  (Param name schema 'path))

(define (%header name schema)
  (Param name schema 'header))

(define (%cookie name schema)
  (Param name schema 'cookie))

(define (response #:code [code 'OK] schema) 
  (Response schema code))

(define (Route->hash route)
  (let ([route-hash (make-hasheq)]
        [path       (Route-path route)]
        [method     (Route-method route)]
        [body       (Route-body route)]
        [params     (Route-parameters route)]
        [reponses   (Route-responses route)])
    (hash-set! route-hash 'operationId (symbol->string (gensym method)))
    (hash-set! route-hash
    
               )))

(define (API->paths:hash api)
  (let ([routes (API-routes api)]
        [paths  (make-hasheq)])
    (for ([route routes])
      (hash-set! paths (Route-path route)  (Route->hash route))
      )
    paths
    ))

(define (API: #:name name
              #:hostname [hostname "localhost"]
              #:ssl? [ssl? #f]
              #:path-prefix [path-prefix "/"] . routes )
  (API name (flatten routes) hostname ssl? path-prefix))

(define (API->json api #:version (version "1.0.0")
                   #:servers (servers '()))
  (let ([openapi (make-hasheq `((openapi . "3.0.0")))])

    (hash-set! openapi 'info (make-hasheq `((version . ,version)
                                            (title . ,(API-name api)))))
    (unless (null? servers) )
    (hash-set! openapi 'paths (API->paths:hash api))
    (jsexpr->string openapi)
    ))
