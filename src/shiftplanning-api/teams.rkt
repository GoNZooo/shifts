#lang racket/base

(require "team-members.rkt")

(provide employee-in-team?/id)
(define (employee-in-team?/id employee team)
  (ormap (lambda (team-member)
           (equal? (hash-ref employee
                             'employee_id)
                   (car team-member)))
         team))

(provide employee-in-team?/schedule)
(define (employee-in-team?/id employee team)
  (define (schedule-in-team? schedule)
    (findf (lambda (team-schedule)
             (equal? schedule team-schedule))
           team))

  (ormap schedule-in-team?
         (hash-ref employee 'schedules)))
