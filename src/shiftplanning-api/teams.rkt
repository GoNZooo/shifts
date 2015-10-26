#lang racket/base

(require "team-members.rkt")

(provide employee-in-team?)
(define (employee-in-team? employee team)
  (ormap (lambda (team-member)
           (equal? (hash-ref employee
                             'employee_id)
                   (car team-member)))
         team))
