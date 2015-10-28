#lang racket/base

(require "api-call.rkt"
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
                     (end_date . ,end-date)))
    (api-call #:module "reports.schedule"
              #:method "GET"
              #:request-parameters
              `#hash((start_date . ,start-date)
                     (end_date . ,end-date)
                     (employees . ,employees)))))

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
  (define start-of-month (date 0 0 0 1 (date-month cd) (date-year cd) 0 0 0 #f))
  (define end-of-month (date 0 0 0 31 (date-month cd) (date-year cd) 0 0 0 #f))
  (pretty-print
    (get/report/shifts #:start (date->shiftplanning-date start-of-month)
                       #:end (date->shiftplanning-date end-of-month))))
