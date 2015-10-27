#lang racket/base

(require net/http-client
         net/url
         net/uri-codec
         json

         "credentials.rkt"
         "urls.rkt")

(provide api-call)
(define (api-call #:module module
                  #:method method
                  #:request-parameters [request-parameters #hash()]
                  #:token [token credentials/token])
  (define request-data
    (jsexpr->string
      `#hash((token . ,token)
             (module . ,module)
             (method . ,method)
             (request . ,request-parameters))))

  (define-values (response headers input-port)
    (http-sendrecv host
                   base-url
                   #:ssl? #t
                   #:method "POST"
                   #:data
                   (alist->form-urlencoded
                     (list (cons 'data request-data)))
                   #:headers
                   (list "Content-Type: application/x-www-form-urlencoded")))

  (hash-ref (read-json input-port)
            'data))
