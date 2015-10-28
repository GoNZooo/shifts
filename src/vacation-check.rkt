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
                               'formatted))
  (define end-date (hash-ref (hash-ref v
                                       'end_day)
                             'formatted))

  (define employee-name (hash-ref v
                                  'employee_name))
  
  (slack-message
    (format "@mag4e: ~a has requested a vacation!" employee-name)
    #:attachments
    `(#hash((fallback . ,(format "~a has requested vacation (~a to ~a)"
                                 employee-name start-date end-date))
            (title . ,(format "Vacation request [~a]"
                              employee-name))
            (fields .
                    (#hash((title . "Employee")
                           (value . ,employee-name)
                           (short . ,#t))
                     #hash((title . "Timeframe")
                           (value . ,(format "*~a* to *~a*"
                                             start-date end-date))
                           (short . ,#t))))
            (mrkdwn_in . ("fields"))))))

(define (fetch-loop [sleep-time 300])
  (seen-vacations (read-seen-vacations))
  (with-handlers ([exn:fail:read?
                    (lambda (e)
                      (printf "Read error: ~a~n"
                              e))])
                 (define new (new-vacations))
                 (for-each add-seen-vacation new)
                 (for-each (lambda (v)
                             (vacation-notify v))
                           new)
                 (write-seen-vacations (seen-vacations)))
  (sleep sleep-time)
  (fetch-loop))

(module+ main
  (fetch-loop))

