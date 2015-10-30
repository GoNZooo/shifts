#lang racket/base

(require "shiftplanning-api/shifts.rkt"
         "shiftplanning-api/employees.rkt"
         "slack.rkt")

(define (notify/deleted deleted snapshot)
  (define deleted-shift (hash-ref snapshot
                                  deleted))
  (define user (employee-id->name (hash-ref deleted-shift
                                            'user)
                                  "cs_row"))

  (define start-date (hash-ref (hash-ref deleted-shift
                                         'start_date)
                               'date))

  (define end-date (hash-ref (hash-ref deleted-shift
                                       'end_date)
                             'date))


  (slack-message
    (format "@mag4e: A shift was deleted by ~a"
            user)
    #:attachments
    `(#hash((fallback . ,(format "~a has deleted a shift"
                                 user))
            (title . "Shift deleted")
            (fields .
                    (#hash((title . "User")
                           (value . ,user)
                           (short . ,#f))
                     #hash((title . "Timeframe")
                           (value . ,(format "*~a* to *~a*"
                                             start-date end-date))
                           (short . ,#f))))
            (mrkdwn_in . ("fields"))))))

(define (notify/new new)
  (define user (employee-id->name (hash-ref new
                                            'user)
                                  "cs_row"))

  (define start-date (hash-ref (hash-ref new
                                         'start_date)
                               'date))

  (define end-date (hash-ref (hash-ref new
                                       'end_date)
                             'date))


  (slack-message
    (format "@mag4e: A new shift was created by ~a"
            user)
    #:attachments
    `(#hash((fallback . ,(format "~a has created a shift"
                                 user))
            (title . "Shift created")
            (fields .
                    (#hash((title . "User")
                           (value . ,user)
                           (short . ,#f))
                     #hash((title . "Timeframe")
                           (value . ,(format "*~a* to *~a*"
                                             start-date end-date))
                           (short . ,#f))))
            (mrkdwn_in . ("fields"))))))

(define (notify/edited edited snapshot)
  (define user (employee-id->name (hash-ref (cdr edited)
                                            'user)
                                  "cs_row"))

  (define old-start-date (hash-ref (hash-ref (hash-ref snapshot
                                                       (hash-ref (cdr edited)
                                                                 'id))
                                             'start_date)
                                   'date))
  
  (define old-end-date (hash-ref (hash-ref (hash-ref snapshot
                                                     (hash-ref (cdr edited)
                                                               'id))
                                           'end_date)
                                 'date))

  (define new-start-date (hash-ref (hash-ref (cdr edited)
                                             'start_date)
                                   'date))

  (define new-end-date (hash-ref (hash-ref (cdr edited)
                                           'end_date)
                                 'date))


  (slack-message
    (format "@mag4e: A shift was modified by ~a"
            user)
    #:attachments
    `(#hash((fallback . ,(format "~a has modified a shift"
                                 user))
            (title . "Shift modified")
            (fields .
                    (#hash((title . "User")
                           (value . ,user)
                           (short . ,#f))
                     #hash((title . "Old timeframe")
                           (value . ,(format "*~a* to *~a*"
                                             old-start-date old-end-date))
                           (short . ,#f))
                     #hash((title . "New timeframe")
                           (value . ,(format "*~a* to *~a*"
                                             new-start-date new-end-date))
                           (short . ,#f))))
            (mrkdwn_in . ("fields"))))))

(define (fetch-loop [sleep-time 1200])
  (with-handlers ([exn:fail:read? (lambda (e)
                                    (format "Read error: ~a"
                                            e))])
  (define old-snapshot (read-snapshot "cs_row"))
  (define current-shifts (get/schedule/shifts/team "cs_row"))
  (define diff (shifts/snapshot/diff old-snapshot
                                     current-shifts))
  
  (for-each (lambda (deleted-shift)
              (notify/deleted deleted-shift
                              old-snapshot))
            (hash-ref diff
                      'deleted))
  
  (for-each (lambda (new-shift)
              (notify/new new-shift))
            (hash-ref diff
                      'new))
  
  (for-each (lambda (edited-shift)
              (notify/edited edited-shift
                              old-snapshot))
            (hash-ref diff
                      'edited))

  (write-snapshot (modify-snapshot old-snapshot diff)))
  
  (sleep sleep-time)
  (fetch-loop))

(module+ main
  (fetch-loop))
