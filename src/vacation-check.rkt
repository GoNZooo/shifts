#lang racket/base

(require "shiftplanning-api/vacations.rkt"
         "slack.rkt")

(define seen-vacations (make-parameter '()))

(define (add-seen-vacation v)
  (seen-vacations (cons v
                        (seen-vacations))))

(define (write-seen-vacations vs
                              [path "data/seen-vacations"])
  (call-with-output-file
    path
    (lambda (out)
      (write vs out))
    #:exists 'replace))

(define (read-seen-vacations [path "data/seen-vacations"])
  (call-with-input-file
    path
    read))

(define (seen-vacation? v)
  (findf (lambda (seen-vacation)
           (equal? (hash-ref seen-vacation
                             'id)
                   (hash-ref v
                             'id)))
         (seen-vacations)))

(define (new-vacation? v)
  (not (seen-vacation? v)))

(define (new-vacations [team-name "cs_row"])
  (filter new-vacation?
          (get/vacations/team team-name)))

(define (fetch-loop)
  (sleep 240)
  (seen-vacations (read-seen-vacations))
  (define new (new-vacations))
  (for-each add-seen-vacation new)
  (for-each (lambda (v)
              (slack-message (format "~a has requested a vacation!"
                                     (hash-ref v
                                               'employee_name))))
            new)
  (write-seen-vacations (seen-vacations)))

(module+ main
  (fetch-loop))

