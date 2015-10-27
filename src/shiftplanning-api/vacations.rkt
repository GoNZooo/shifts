#lang racket/base

(require "api-call.rkt"
         "employees.rkt")

(provide get/vacations)
(define (get/vacations)
  (api-call #:module "schedule.vacations"
            #:method "GET"
            #:request-parameters
            #hash((mode . "manage"))))

(provide get/vacations/team)
(define (get/vacations/team [team-name "cs_row"])
  (filter (lambda (v)
            (employee-in-team? (hash-ref v
                                         'user)
                               team-name))
          (get/vacations)))

(module+ main
  (require racket/pretty)
  (pretty-print (get/vacations)))
