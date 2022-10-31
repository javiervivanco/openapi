#lang racket/base
(require (for-syntax racket/syntax racket/base))

(provide (all-defined-out))

(define all-defined-types (make-parameter '()))

(define : list)

(define-syntax-rule (define-primitive :NAME :TYPE :DEFAULT_FORMAT )
  (define (:NAME #:format (format :DEFAULT_FORMAT)  desc (example '()) )
    (let ([type (make-hash `((type . :TYPE) (description . ,desc) ))])
      (unless (null? :DEFAULT_FORMAT) (hash-set! type 'format format))
      (unless (null? example) (hash-set! type 'example example))
      type)))

(define (:enum desc enums)
  (let ([type (make-hash `((type . "string")
                           (description . ,desc)
                           (enum . ,(map (λ (e) (if (symbol? e) (symbol->string e) e))  enums))))])
    ;;(unless (null? example) (hash-set! type 'example example))
    type))

(define-primitive :string "string" '())
(define-primitive :byte "string" "byte")
(define-primitive :password "string" "password")
(define-primitive :binary "string" "binary")
(define-primitive :number "number" "float")
(define-primitive :date "string" "date")
(define-primitive :date-time "string" "date-time")
(define-primitive :float  "number" "float")
(define-primitive :double "number" "double")
(define-primitive :boolean "boolean" '())
(define-primitive :integer "integer" '())
(define-primitive :int32 "integer" "int32")
(define-primitive :int64 "integer" "int64")


(define ($$ref a-schema)
  (make-hash `(($ref . ,(format "#/components/schemas/~a" (:schema-name a-schema))))))
  

(define ($ref a-schema)
  (make-hash `((schema . ,($$ref a-schema)))))
(define (:schema-type a-schema)
  (hash-ref a-schema 'type #f))

(define (type-> atype)
  (cond
    [(hash? atype)  (if (:schema-type atype) atype  ($$ref atype))]
    [(equal? atype :string)    (make-hash '((type . "string")))]
    [(equal? atype :byte)      (make-hash '((type . "string") (format . "byte")))]
    [(equal? atype :password)  (make-hash '((type . "string") (format . "password")))]
    [(equal? atype :binary)    (make-hash '((type . "string") (format . "binary")))]
    [(equal? atype :number)    (make-hash '((type . "number")))]
    [(equal? atype :date)      (make-hash '((type . "string") (format . "date")))]
    [(equal? atype :date-time) (make-hash '((type . "string") (format . "date-time")))]
    [(equal? atype :float)     (make-hash '((type . "number") (format . "float")))]
    [(equal? atype :double)    (make-hash '((type . "number") (format . "double")))]
    [(equal? atype :boolean)   (make-hash '((type . "boolean")))]
    [(equal? atype :integer)   (make-hash '((type . "integer")))]
    [(equal? atype :int32)     (make-hash '((type . "integer") (format . "int32")))]
    [(equal? atype :int64)     (make-hash '((type . "integer") (format . "int64")))]))

(define (:array typeof desc . examples)
  (let ([type (make-hash `((type . "array") (items . ,(type-> typeof)) (description . ,desc) ))])
     
    (unless (null? examples) (hash-set! type 'example examples))
    type))
 
(define (:schema #:example (ex '()) name desc :type)
  (define ~content (type-> :type))
  (unless (null? ex) (hash-set! ~content 'example ex))
  (hash-set! ~content 'description desc)
  (define ~schema (make-hash (list (cons name ~content))))
  ~schema)

(define (:schema-name a-schema)
  (car (hash-keys a-schema)))

(define (schema->string aschema)
  (symbol->string (:schema-name aschema)))

(define (example schema ex)
  (hash-set! (car (hash-values schema)) 'example ex)) 

(define-syntax-rule (define-schema (ID desc type))
  (begin
    (define ID (:schema 'ID desc type))
    (all-defined-types (append (all-defined-types) (list ID)))
    ))

(define-syntax-rule (define-enum (ID desc (ENUM ...)))
  (begin
    (define ID (:schema 'ID desc (:enum desc (list 'ENUM ...))))
    (all-defined-types (append (all-defined-types) (list ID )))
    ))



(define-syntax (define-schema-object stx)
  (syntax-case stx ()
    [(_  ((ID ARRAYOF) DESC (PROP ARGS ...) ...))
     (with-syntax ([ARRAY-DESC (symbol->string (syntax->datum #'ARRAYOF))])
       #'(begin
           (define ID (:schema-object 'ID DESC (prop 'PROP ARGS ... ) ... ))
           (define ARRAYOF (:schema 'ARRAYOF (format "Lista de ~a "(symbol->string 'ARRAYOF)) (:array ID ARRAY-DESC)))
           ;;(define ARRAYOF (:array ID ARRAY-DESC))
           (all-defined-types (append (all-defined-types) (list ID ARRAYOF)))
           ))]
    [(_  (ID DESC (PROP ARGS ...) ...))
     #'(define ID (:schema-object 'ID DESC (prop 'PROP ARGS ... ) ... ))]    ))

(define (prop id type desc . args)
  (let ([~content (if (hash? type) ($$ref type) (apply type (append (list desc) args)))])
    (hash-set! ~content 'description desc)
    (cons id ~content)))

(define (:schema-object name desc . props )
  (:schema name desc (apply :object (append (list desc) props) )))

(define (:object desc . props )
  (let* ([~props (make-hash)]
         [obj    (make-hash `((description . ,desc)
                              (type . "object")
                              (properties . ,~props)))])
    (map (λ (p) (hash-set! ~props (car p) (cdr p)) )  props)
    obj))


(define (schema->name aschema)
  (symbol->string (car (hash-keys aschema))))

(define (array-of-desc array-of)
  (hash-ref array-of 'description))

(define (set-body! root body)
  (let ([content (make-hash `((application/json . ,(make-hash `( (schema . ,(type-> body)) )))))])
    (hash-set!  root 'content content)))

(define (make-key-desc key desc)
  (let*([content (make-hash `((description . ,desc)))]
        [root    (make-hash `((,key . ,content)))])
    (values root content)))

(define-schema (UID "Identificador Unico de algo" :string ))
