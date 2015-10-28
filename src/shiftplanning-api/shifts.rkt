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
                                  (date->shiftplanning-date (month-end)))
  (filter (lambda (shift)
            (ormap (lambda (employee)
                     (employee-in-team? (hash-ref employee 'id)
                                        team-name))
                   (shift-employees shift)))
          (get/schedule/shifts)))

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
  (pretty-print
    (get/schedule/shifts/team "cs_row")))
