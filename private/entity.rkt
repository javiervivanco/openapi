#lang racket/base
(require "response.rkt"
         "types.rkt"
         "param.rkt"
         "paths.rkt"
         racket/syntax
         (for-syntax
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
            [ENTITY-URI1     (format-id  #'ID "/~a/<~a>" entities-name #'ENTITY-PATH-ID)]
            )
         #'(begin
             (define-schema (ENTITY-UID (format "Identificador Unico de ~a" ENTITY-NAME) UID))
             (define ENTITY/404: (404: (format "~a: Entidad no encontrada" ENTITY-NAME)))
             (define ?ENTITY-PATH-ID (&path 'ENTITY-PATH-ID (format "Identificador Unico de ~a" ENTITY-NAME) ENTITY-UID))

             (define-schema-object ((ID ARRAYOF) DESC  (PROP ARGS ...) ...))
             (define-values (ENTITY-URI ENTITY-URI1)
               (apply values (/<entities>/...
                              #:id ?ENTITY-PATH-ID
                              #:all ARRAYOF
                              #:entity ID
                              #:not-found ENTITY/404:)))
             (provide ENTITY-UID ENTITY-URI1 ENTITY/404: ?ENTITY-PATH-ID ID ARRAYOF ENTITY-URI )
             )))
     ]
    ))
