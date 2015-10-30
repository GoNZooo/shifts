#lang racket/base

(require racket/date
         racket/string

         "api-call.rkt"
         "employees.rkt"
         "shiftplanning-dates.rkt")

(provide get/schedule/shifts)
(define (get/schedule/shifts #:start
                             [start-date
                               (date->shiftplanning-date (month-start))]
                             #:end
                             [end-date
                               (date->shiftplanning-date (month-end))])
  (api-call #:module "schedule.shifts"
            #:method "GET"
            #:request-parameters
            `#hash((start_date . ,start-date)
                   (end_date . ,end-date)
                   (mode . "overview")
                   (detailed . 0))))

(provide get/schedule/shifts/team)
(define (get/schedule/shifts/team [team-name "cs_row"]
                                  #:start
                                  [start-date
                                    (date->shiftplanning-date (month-start))]
                                  #:end
                                  [end-date
                                    (date->shiftplanning-date (month-end))])
  (filter (lambda (shift)
            (ormap (lambda (employee)
                     (employee-in-team? (hash-ref employee 'id)
                                        team-name))
                   (shift-employees shift)))
          (get/schedule/shifts #:start start-date
                               #:end end-date)))

(define (shifts/snapshot/team [team-name "cs_row"]
                              #:start
                              [start-date
                                (date->shiftplanning-date (month-start))]
                              #:end
                              [end-date (date->shiftplanning-date (month-end))])
  (for/hash ([shift (get/schedule/shifts/team team-name
                                              #:start start-date
                                              #:end end-date)])
    (values (hash-ref shift 'id)
            shift)))

(provide write-snapshot)
(define (write-snapshot snapshot [team-name "cs_row"])
  (call-with-output-file (format "cache/shifts/~a.cache"
                                 team-name)
                         (lambda (out)
                           (write snapshot out))
                         #:exists 'replace))

(provide read-snapshot)
(define (read-snapshot [team-name "cs_row"])
  (call-with-input-file (format "cache/shifts/~a.cache"
                                team-name)
                        read))

(provide modify-snapshot)
(define (modify-snapshot snapshot diff)
  (define new-snapshot (make-hash))

  (for-each (lambda (edited)
              (hash-set! new-snapshot
                         (car edited)
                         (cdr edited)))
            (hash-ref diff
                      'edited))

  (for-each (lambda (deleted)
              (hash-remove! new-snapshot
                            deleted))
            (hash-ref diff
                      'deleted))

  (for-each (lambda (new)
              (hash-set! new-snapshot
                         (hash-ref new
                                   'id)
                         new))
            (hash-ref diff
                      'new))
  
  (for-each (lambda (unchanged)
              (hash-set! new-snapshot
                         unchanged
                         (hash-ref snapshot
                                   unchanged)))
            (hash-ref diff
                      'unchanged))

  new-snapshot)

(define (shift-id shift)
  (hash-ref shift 'id))

(define (shift-in-snapshot shift snapshot)
  (hash-ref snapshot (shift-id shift) #f))


(provide shifts/snapshot/diff)
(define (shifts/snapshot/diff snapshot shifts
                              [new-shifts '()]
                              [edited-shifts '()]
                              [unchanged-shifts '()])
  (define (shifts/snapshot/new?)
    (not (hash-has-key? snapshot (shift-id (car shifts)))))

  (define (shifts/snapshot/edited?)
    (not (equal? (hash-ref (shift-in-snapshot (car shifts)
                                              snapshot)
                           'edited)
                 (hash-ref (car shifts)
                           'edited))))

  (define (add-edited-shift shift snapshot-id shift-list)
    (cons (cons snapshot-id
                shift)
          shift-list))

  (cond
    [(null? shifts)
     `#hash((new . ,new-shifts)
            (deleted .
                     ,(filter (lambda (snapshot-id)
                                (not (findf (lambda (shift)
                                              (equal? shift
                                                      snapshot-id))
                                            (append (map shift-id
                                                         (map cdr
                                                              edited-shifts))
                                                    unchanged-shifts))))
                              (hash-keys snapshot)))
            (edited . ,edited-shifts)
            (unchanged . ,unchanged-shifts))]
    [(shifts/snapshot/new?)
     (shifts/snapshot/diff snapshot
                           (cdr shifts)
                           (cons (car shifts)
                                 new-shifts)
                           edited-shifts
                           unchanged-shifts)]
    [(shifts/snapshot/edited?)
     (shifts/snapshot/diff snapshot
                           (cdr shifts)
                           new-shifts
                           (add-edited-shift (car shifts)
                                             (shift-id (car shifts))
                                             edited-shifts)
                           unchanged-shifts)]
    [else
      (shifts/snapshot/diff snapshot
                            (cdr shifts)
                            new-shifts
                            edited-shifts
                            (cons (shift-id (car shifts))
                                  unchanged-shifts))]))

(define (shift-employees shift)
  (if (hash-has-key? shift 'employees)
    (hash-ref shift 'employees)
    '()))

(define (edited-this-month? shift)
  (equal? (date-month (current-date))
          (date-month (seconds->date (string->number (hash-ref shift
                                                               'edited))))))

(define (get/shifts/edited)
  (define shifts (get/schedule/shifts #:start
                                      (date->shiftplanning-date (month-start))
                                      #:end
                                      (date->shiftplanning-date (month-end))))
  (filter edited-this-month?
          shifts))

(define (get/shifts/edited/team [team-name "cs_row"])
  (filter (lambda (shift)
            (ormap (lambda (employees)
                     (employee-in-team? (hash-ref employees 'id)
                                        team-name))
                   (shift-employees shift)))
          (get/shifts/edited)))

(provide get/report/shifts)
(define (get/report/shifts #:start start-date
                           #:end end-date
                           #:employees [employees '()])
  (if (null? employees)
    (api-call #:module "reports.schedule"
              #:method "GET"
              #:request-parameters
              `#hash((start_date . ,start-date)
                     (end_date . ,end-date)
                     (type . "crib_sheet")
                     (with_statuses . 1)))
    (api-call #:module "reports.schedule"
              #:method "GET"
              #:request-parameters
              `#hash((start_date . ,start-date)
                     (end_date . ,end-date)
                     (employees . ,employees)
                     (type . "crib_sheet")))))

(define (month-start)
  (date 0 0 0
        1 (date-month (current-date)) (date-year (current-date))
        0 0 #f 0))

(define (month-end)
  (date 0 0 0
        31 (date-month (current-date)) (date-year (current-date))
        0 0 #f 0))

(define (get/on-now)
  (api-call #:module "dashboard.onnow"
            #:method "GET"))

(define (get/on-now/team [team-name "cs_row"])
  (filter (lambda (employee)
            (employee-in-team? employee
                               team-name))
          (get/on-now)))

(module+ main
  (require racket/pretty)
  ;(define snapshot (read-snapshot "cs_row2"))
   (define snapshot (shifts/snapshot/team "cs_row"))
   (write-snapshot snapshot "cs_row"))
