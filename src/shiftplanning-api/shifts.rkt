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

(define (write-snapshot snapshot [team-name "cs_row"])
  (call-with-output-file (format "cache/shifts/~a.cache"
                                 team-name)
                         (lambda (out)
                           (write snapshot out))
                         #:exists 'replace))

(define (read-snapshot [team-name "cs_row"])
  (call-with-input-file (format "cache/shifts/~a.cache"
                                team-name)
                        read))

(define (shift-id shift)
  (hash-ref shift 'id))

(define (shift-in-snapshot shift snapshot)
  (hash-ref snapshot (shift-id shift) #f))

(define (shift-deleted? shift)
  (equal? (hash-ref shift
                    'deleted)
          "1"))

(provide shifts/snapshot/changed)
(define (shifts/snapshot/changed snapshot shifts
                                 [new-shifts '()]
                                 [deleted-shifts '()]
                                 [edited-shifts '()])
  (define (shifts/snapshot/new?)
    (not (hash-has-key? snapshot (shift-id (car shifts)))))

  (define (shifts/snapshot/deleted?)
    (and (not (shift-deleted? (shift-in-snapshot (car shifts)
                                                 snapshot)))
         (shift-deleted? (car shifts))))

  (define (shifts/snapshot/edited?)
    (not (equal? (hash-ref (shift-in-snapshot (car shifts)
                                              snapshot)
                           'edited)
                 (hash-ref (car shifts)
                           'edited))))

  (define (add-changed-shift shift shift-list)
    (cons (cons (shift-in-snapshot shift
                                   snapshot)
                shift)
          shift-list))

  (cond
    [(null? shifts)
     `#hash((new . ,new-shifts)
            (deleted . ,deleted-shifts)
            (edited . ,edited-shifts))]
    [(shifts/snapshot/new?)
     (shifts/snapshot/changed snapshot
                              (cdr shifts)
                              (add-changed-shift (car shifts)
                                                 new-shifts)
                              deleted-shifts  
                              edited-shifts)]
    [(shifts/snapshot/deleted?)
     (shifts/snapshot/changed snapshot
                              (cdr shifts)
                              new-shifts
                              (add-changed-shift (car shifts)
                                                 deleted-shifts)
                              edited-shifts)]
    [(shifts/snapshot/edited?)
     (shifts/snapshot/changed snapshot
                              (cdr shifts)
                              new-shifts
                              deleted-shifts
                              (add-changed-shift (car shifts)
                                                 edited-shifts))]
    [else
      (shifts/snapshot/changed snapshot
                               (cdr shifts)
                               new-shifts
                               deleted-shifts
                               edited-shifts)]))

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
  (define snapshot (read-snapshot "cs_row"))
  (pretty-print
    (shifts/snapshot/changed snapshot
                             (get/schedule/shifts/team "cs_row"))))
