#lang racket/base

(require net/http-client
         net/url
         net/uri-codec
         json

         "credentials.rkt"
         "urls.rkt")

(define (get-token [username credentials/username]
                   [password credentials/password]
                   [api-key credentials/api-key])
  (define request-data
    (jsexpr->string
      `#hash((key . ,api-key)
             (request .
                      #hash((username . ,username)
                            (password . ,password)
                            (module . "staff.login")
                            (method . "GET"))))))

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

  (read-json input-port))

(define (get-config [api-key credentials/api-key])
  (define request-data
    (jsexpr->string
      `#hash((key . ,api-key)
             (request .
                      #hash((module . "api.config"))))))

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

  (read-json input-port))

(module+ main
  (require racket/pretty)
  (pretty-print
    (get-token)))
