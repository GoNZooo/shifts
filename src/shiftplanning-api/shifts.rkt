#lang racket/base

(require racket/date
         
         "api-call.rkt"
         "employees.rkt"
         "shiftplanning-dates.rkt")

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
                     (type . "schedule_summary")))
    (api-call #:module "reports.schedule"
              #:method "GET"
              #:request-parameters
              `#hash((start_date . ,start-date)
                     (end_date . ,end-date)
                     (employees . ,employees)
                     (type . "schedule_summary")))))

(define (start-of-month)
  (date 0 0 0
        1 (date-month (current-date)) (date-year (current-date))
        0 0 #f 0))

(define (end-of-month)
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
  (define cd (current-date))
  (define start (start-of-month))
  (define end (end-of-month))
  (pretty-print
    (get/report/shifts #:start (date->shiftplanning-date (start-of-month))
                       #:end (date->shiftplanning-date (end-of-month)))))
