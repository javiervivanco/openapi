#lang racket/base
(provide path -> body response  (all-from-out "param.rkt")  (all-from-out "response.rkt"))
(require racket/format "types.rkt" "param.rkt" "response.rkt")

(define (hash-set-not-null! h key val )
  (unless (null? val) (hash-set! h key val)))

(define (body #:required? (r #t) desc abody)
  (let-values ([(root content) (make-key-desc 'requestBody desc)])
    (hash-set! content 'required r)
    (set-body! content abody)
    content))

(define (->
         #:id (op-id '())
         #:desc (desc '())
         #:tags (tags '())
         #:body (request-body '())
         #:params (params '())  sum  . responses)
  (unless (string? sum) (error "summary must be string"))
  (define p (make-hash ))
  (hash-set-not-null! p 'description desc)
  (hash-set-not-null! p 'requestBody request-body)
  (when (symbol? op-id) (set! op-id (symbol->string op-id)))
  (hash-set-not-null! p 'operationId op-id)
  (hash-set-not-null! p 'parameters params)
  (hash-set-not-null! p 'tags tags)
  (hash-set-not-null! p 'summary sum)
  (when (null? responses) (error "Empty Response"))
  (define ~responses (make-hash))

  (for ([a-response responses ])
    (let ([code (car (hash-keys a-response))])
      (hash-set! ~responses code (hash-ref a-response code))))
  (hash-set-not-null! p 'responses ~responses)
  p)


(define (path uri description 
              #:get [get '() ]
              #:post [post '() ]
              #:delete [delete '() ]
              #:put [put '() ]
              . parameters
              )
  (unless (string? description) (error "Description must be string"))
  (let* ([e (make-hasheq `((,uri . ,(make-hasheq `((description . ,description)))) ))]
         [set!! (λ (k v) (hash-set! (hash-ref e uri) k v)) ])
    (unless (null? parameters) (set!! 'parameters  parameters))
    (unless (null? get)    (set!! 'get get))
    (unless (null? post)   (set!! 'post post))
    (unless (null? delete) (set!! 'delete delete))
    (unless (null? put)    (set!! 'put put))
    e
    ))

(provide ->/all
         ->/new
         ->/get
         ->/update
         ->/remove
         /<entities>/...
         ?limit
         ?offset
         )
(define ?limit  (&query 'limit "Limite de paginación" :integer))
(define ?offset (&query 'offset "Desde donde comienza paginación" :integer))


(define (genOpId OpIp Entity)
  (string->symbol (~a OpIp (string-titlecase Entity))))


(provide description)

(define description (make-parameter #f))

(define (desc-ref key default)
  (if (description)
      (hash-ref (make-hash (description)) key default)
      default))

(define (entity-title Entity text)
  (~a (schema->name Entity) ": " text))

(define (array-of->url ArrayOfEntity )
  (~a "/"(string-downcase (schema->string ArrayOfEntity))))

(define (->/all Entity ArrayOfEntity NotFound   )
  (-> (desc-ref 'all (entity-title Entity "Lista todos items")) 
      #:id (genOpId 'getAll (schema->string ArrayOfEntity) )
      #:params (: ?limit ?offset)
      (200: "OK" ArrayOfEntity)
      NotFound))

(define (->/new Entity )
  (-> (desc-ref 'new (entity-title Entity "Crear nuevo item"))
      #:id (genOpId 'new (schema->name Entity))
      #:body (body (schema->name Entity)  Entity)
      (201: (entity-title Entity " Item creado"))))

(define (->/get Entity NotFound)
  (-> (desc-ref 'get (entity-title Entity "Traer")) 
      #:id (genOpId 'get (schema->name Entity) )
      (200: (entity-title Entity "OK")  Entity)
      NotFound))

(define (->/update  Entity NotFound)
  (-> (desc-ref 'update (entity-title Entity "Actualizar datos"))
      #:id (genOpId 'update (schema->name Entity) )
      #:body (body  (entity-title Entity " Datos nuevos") Entity)
      (200: (entity-title Entity " Item actualizado correctamente"))
      NotFound))

(define (->/remove Entity NotFound)
  (-> (desc-ref 'remove (entity-title Entity "Borrar item"))
      #:id (genOpId 'remove (schema->name Entity) )
      (204: (entity-title Entity " Item borrado correctamente"))
      NotFound))


(define (/<entities> 
         #:all ArrayOfEntity
         #:entity Entity
         #:not-found NotFound)
  (path  (array-of->url ArrayOfEntity ) (~a "Gestión de " (schema->string ArrayOfEntity))
         #:get (->/all Entity ArrayOfEntity NotFound)
         #:post (->/new Entity)))

(define (/<entities>/<path-id> #:id param-id
                               #:all ArrayOfEntity
                               #:entity Entity
                               #:not-found NotFound)
  (path  (~a  (array-of->url ArrayOfEntity ) "/{" (hash-ref param-id 'name) "}")
         (~a "Gestión de " (schema->string ArrayOfEntity)) 
         param-id
         #:get    (->/get    Entity NotFound)
         #:put    (->/update Entity NotFound)
         #:delete (->/remove Entity NotFound)))


(define (/<entities>/... #:id param-id
                         #:all ArrayOfEntity
                         #:entity Entity
                         #:not-found NotFound)
  (: (/<entities>  #:all ArrayOfEntity #:entity Entity #:not-found NotFound)
     (/<entities>/<path-id> #:id param-id #:all ArrayOfEntity #:entity Entity #:not-found NotFound)))