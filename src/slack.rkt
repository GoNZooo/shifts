#lang racket/base

(require racket/port
         net/http-client
         net/url
         net/uri-codec
         json

         "slack-information.rkt")

(provide slack-message)
(define (slack-message msg
                       #:host [host hook-host]
                       #:url [url hook-url])
  (define request-data
    (jsexpr->string
      `#hash((text . ,msg))))

  (define-values (response headers input-port)
    (http-sendrecv host
                   url
                   #:ssl? #t
                   #:method "POST"
                   #:data
                   (alist->form-urlencoded
                     (list (cons 'payload request-data)))
                   #:headers
                   (list "Content-Type: application/x-www-form-urlencoded")))
  (port->string input-port))

(module+ main
  (slack-message "This is a message.\n"))
