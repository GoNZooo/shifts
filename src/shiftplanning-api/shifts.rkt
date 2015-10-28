#lang racket/base

(require "api-call.rkt"
         "teams.rkt"
         "team-members.rkt"
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

(define (get/on-now/team [team-name team/row])
  (filter (lambda (employee)
            (employee-in-team?/id employee
                                  team-name))
          (get/on-now)))

(module+ main
  (require racket/pretty)
  (pretty-print (get/on-now/team team/italian)))
