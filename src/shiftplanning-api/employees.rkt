#lang racket/base

(require "api-call.rkt"
         "teams.rkt"
         "team-members.rkt")

(provide get/employees)
(define (get/employees)
  (api-call #:module "staff.employees"
            #:method "GET"))

(define (get/employees/team [team-name team/row])
  (filter (lambda (employee)
            (employee-in-team?/schedule employee team-name))
          (api-call #:module "staff.employees"
                    #:method "GET")))

(module+ main
  (require racket/pretty)
  (pretty-print (get/employees/team team/row)))
