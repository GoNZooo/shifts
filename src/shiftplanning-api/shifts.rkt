#lang racket/base

(require "api-call.rkt"
         "teams.rkt"
         "team-members.rkt")

(provide get/shifts)
(define (get/shifts)
  (api-call #:module "schedule.shifts"
            #:method "GET"))

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
