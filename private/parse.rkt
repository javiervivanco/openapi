#lang racket

(define (make-param [param : JSExpr] [top : JSExpr]) : Param
  (cond
    [(json-has-key? param '$ref)
     (make-param (trace-ref (assert (json-ref param '$ref) string?) top) top)]
    [else
     (define name (string->symbol (assert (json-ref param 'name) string?)))
     (define schema-json (json-ref param 'schema))
     (define schema (make-schema-with-name name schema-json top))
     (define param-in (string->Param-In (assert (json-ref param 'in) string?)))
     (Param name schema param-in)]))

(define (make-request-body [request : JSExpr]
                           [top : JSExpr]) : (Listof Property)
  (cond
    [(json-has-keys? request schema-keys)
     (make-properties (json-ref (json-refs request schema-keys) 'properties) top)]
    [else '()]))

(define (make-response [code : Symbol]
                       [method : Method]
                       [path : Symbol]
                       [response : JSExpr]
                       [top : JSExpr]) : Response
  (define schema-name (string->symbol (format "~a-~a-~a" method path code)))
  (define schema
    (if (json-has-keys? response schema-keys)
        (make-schema-with-name schema-name (json-refs response schema-keys) top)
        (Schema-Object schema-name '())))
  (Response (assert schema Schema-Object?) code))

(define (make-routes-for-path [path : Symbol]
                              [contents : JSExpr]
                              [top : JSExpr]) : (Listof Route)
  (hash-map
   (assert contents hash?)
   (λ ([method : Symbol] [body : JSExpr])
     (with-asserts ([method method?])
       (define parameters
         (map
          (λ ([p : JSExpr]) (make-param p top))
          (assert (json-ref body 'parameters '()) list?)))

       (define responses : (Listof Response)
         (for/list ([(code response-body)
                     (in-hash (assert (json-ref body 'responses) hash?))])
           (make-response code method path response-body top)))

       (define request-body
         (make-request-body (json-ref body 'requestBody (hash)) top))

       (Route
        (symbol->string path)
        method
        #f  ; Security not implemented
        request-body
        parameters
        responses)))))

(define (make-routes [paths : JSExpr] [top : JSExpr])
  (define routes : (Listof (Listof Route))
    (for/list ([(path contents) (in-hash (assert paths hash?))])
      (make-routes-for-path path contents top)))
  (append* routes))

(define (parse-openapi [contents : JSExpr])
  (define name (assert (json-refs contents '(info title)) string?))
  (define routes (make-routes (json-ref contents 'paths) contents))
  (define url
    (string->url
     (assert
      (json-ref
       (car (assert (json-ref contents 'servers) list?))
       'url)
      string?)))
  (define path-prefix
    (foldl
     (λ ([e : path/param] [acc : String])
       (string-append acc "/" (assert (path/param-path e) string?)))
     ""
     (url-path url)))
  (define ssl? (string=? (assert (url-scheme url) string?) "https"))
  (API
   name
   routes
   (assert (url-host url) string?)
   ssl?
   path-prefix))

(define (file->openapi [filename : String])
  (with-input-from-file filename
    (λ ()
      (define contents (cast (read-json) JSExpr))
      (parse-openapi contents))))