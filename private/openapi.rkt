#lang racket/base

(require "openapi-info.rkt" "paths.rkt" "types.rkt" racket/hash racket/port racket/list json  )

(provide openapi openapi-display )
(define (openapi-display API)
  (display (call-with-output-string (λ (out) (write-json API out)))))

(define (openapi  #:title title  #:desc desc  #:api-version version
                  #:tags (tags '())
                  #:types (schemas (all-defined-types))
                  . paths )
  (let ([ast (make-hash
              `((openapi . "3.0.0")
                (info . ,(openapi-info title desc #:version version))
                (paths .  ,(make-hasheq))
                (tags . ,tags) ))])
    (unless (null? schemas)
      (hash-set! ast 'components
                 (make-hash (list (cons 'schemas
                                        (make-hash (apply append (map hash->list schemas))))))))
    (map
     (λ (a-path)
       (for ([a-url (hash-keys a-path) ])
         (let ([h (hash-ref! (hash-ref ast 'paths) (string->symbol a-url)  (make-hash))])
           (define (href key failure)
             (hash-ref (hash-ref a-path a-url ) key failure))
           (hash-set! h 'description (href 'description ""))
           (when (href 'parameters #f) (hash-set! h 'parameters (href 'parameters    (make-hash))))
           (when (href 'get #f)        (hash-set! h 'get    (href 'get    (make-hash))))
           (when (href 'post #f)       (hash-set! h 'post   (href 'post   (make-hash))))
           (when (href 'delete #f)     (hash-set! h 'delete (href 'delete (make-hash))))
           (when (href 'put #f)        (hash-set! h 'put     (href 'put    (make-hash))))           
           )))
     
     (map (λ (p)
            (cond [(procedure? p) (p)]
                  [else p]))
          (flatten paths)))
    ;(apply hash-union (map (λ (p) (hash-copy-clear p #:kind 'immutable)) paths))
    '(components
      security 

      externalDocs)    
    ast))



