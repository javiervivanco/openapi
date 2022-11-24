#lang racket/base
(require "response.rkt"
         "types.rkt"
         "param.rkt"
         "paths.rkt"
         racket/syntax
         racket/format
         (for-syntax
          "paths.rkt"
          
          racket/syntax
          racket/base))

(provide (all-defined-out))

(define-syntax (define-entity stx)
  (syntax-case stx ()
    [(_ (ID ARRAYOF) DESC (PROP ARGS ...) ...)
     (let ([entity-name (string-downcase (symbol->string (syntax->datum  #'ID)))]
           [entities-name (string-downcase (symbol->string (syntax->datum  #'ARRAYOF)))]) 
       (with-syntax*
           ([ENTITY-NAME     entity-name ]
            [ENTITY/404:     (format-id  #'ID "~a/404:" #'ID)]
            [?ENTITY-PATH-ID (format-id  #'ID "?~a-id" entity-name)]
            [ENTITY-PATH-ID  (format-id  #'ID "~a-id" entity-name)]
            [ENTITY-UID      (format-id  #'ID "~a-UID" #'ID)]
            [ENTITY-URI      (format-id  #'ID "/~a" entities-name)]
            [MAKE-PATH-ENTITIES  (format-id  #'ID "make-path-~a" entities-name)]
            [ENTITY-URI1     (format-id  #'ID "/~a/<~a>" entities-name #'ENTITY-PATH-ID)]
            )
         #'(begin
             (define-schema (ENTITY-UID (format "Identificador Unico de ~a" ENTITY-NAME) UID))
             (define ENTITY/404: (404: (format "~a: Entidad no encontrada" ENTITY-NAME)))
             (define ?ENTITY-PATH-ID (&path 'ENTITY-PATH-ID (format "Identificador Unico de ~a" ENTITY-NAME) ENTITY-UID))

             (define-schema-object ((ID ARRAYOF) DESC  (PROP ARGS ...) ...))
             (define (MAKE-PATH-ENTITIES
                      #:delete (delete '())
                      #:put    (put    '()) 
                      #:post   (post   '())  
                      #:get    (get    '())                       
                      #:url    (url    '())
                      #:desc   (desc   '())
                      . params)
               (let ([kw   '(#:all)]
                     [val  (list ARRAYOF)])
                 (define (add-kw! key var)
                   (unless (null? var)
                     (set! kw (append kw (list key)))
                     (set! val (append val (list var)))))
                 (add-kw! '#:delete delete)
                 (add-kw! '#:desc desc)                 
                 (add-kw! '#:entity ID)                 
                 (add-kw! '#:get get)
                 (add-kw! '#:not-found ENTITY/404:)                 
                 (add-kw! '#:post post)
                 (add-kw! '#:put put)                 
                 (add-kw! '#:url url)
                 (keyword-apply make-path-entities kw val params)))
             (define (ENTITY-URI #:all (all '()) )
               (/<entities>  #:all ARRAYOF #:entity ID #:not-found ENTITY/404:))
             (define (ENTITY-URI1)
               (/<entities>/<path-id> #:id ?ENTITY-PATH-ID #:all ARRAYOF #:entity ID #:not-found ENTITY/404:))

             (provide ENTITY-UID ENTITY-URI1 ENTITY/404: ?ENTITY-PATH-ID ID ARRAYOF ENTITY-URI )
             )))
     ]
    ))
