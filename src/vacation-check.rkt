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

(define (vacation-notify v)
  (define start-date (hash-ref (hash-ref v
                                         'start_day)
                               'date))
  (define end-date (hash-ref (hash-ref v
                                       'end_day)
                             'date))

  (define employee-name (hash-ref v
                                  'employee_name))
  
  (slack-message
    (format "Mагичка, ~a has requested a vacation! (~a to ~a)"
            employee-name start-date end-date)
    #:attachments
    `(#hash((fallback . ,(format "~a has requested vacation (~a to ~a)"
                                 employee-name start-date end-date))
            (title . ,(format "Vacation request (~a)"
                              employee-name))
            (fields .
                    (#hash((title . "Employee")
                           (value . ,employee-name)
                           (short . ,#t))
                     #hash((title . "Timeframe")
                           (value . ,(format "From ~a to ~a"
                                             start-date end-date))
                           (short . ,#t))))))))

(define (fetch-loop [sleep-time 300])
  (seen-vacations (read-seen-vacations))
  (define new (new-vacations))
  (for-each add-seen-vacation new)
  (for-each (lambda (v)
              (vacation-notify v))
            new)
  (write-seen-vacations (seen-vacations))
  (sleep sleep-time)
  (fetch-loop))

(module+ main
  (fetch-loop))

