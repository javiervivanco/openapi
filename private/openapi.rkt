#lang racket/base

(require "info.rkt" "paths.rkt" "types.rkt" racket/hash racket/list  )

(provide openapi)
(define (openapi info
                 #:tags (tags '())
                 #:types (schemas '())
                 . paths )
  (let ([ast (make-hash
              `((openapi . "3.1.0")
                (info . ,info)
                (paths .  ,(make-hasheq))
                (tags . ,tags) ))])
    (unless (null? schemas)
      (hash-set! ast 'components
                 (make-hash (list (cons 'schemas  (make-hash (apply append (map hash->list schemas))))))))
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
     
     (flatten paths))
    ;(apply hash-union (map (λ (p) (hash-copy-clear p #:kind 'immutable)) paths))
    '(components
      security 

      externalDocs)    
    ast))



          