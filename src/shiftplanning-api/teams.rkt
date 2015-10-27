#lang racket/base

(provide employee-in-team?/id)
(define (employee-in-team?/id employee team)
  (ormap (lambda (team-member)
           (equal? (hash-ref employee
                             'employee_id)
                   (car team-member)))
         team))

(provide employee-in-team?/schedule)
(define (employee-in-team?/schedule employee team)
  (for/or ([t team])
    (hash-has-key? (hash-ref employee 'schedules)
                   (car t))))
