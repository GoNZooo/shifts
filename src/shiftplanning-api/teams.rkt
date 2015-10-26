#lang racket/base

(provide team/row)
(define team/row
  '((|909929| .  "French CS Agent")
    (|909928| .  "Polish CS Agent")
    (|909932| .  "Czech CS Agent")
    (|909927| .  "ROW TM")
    (|909930| .  "Swedish CS Agent")))

(provide team/english)
(define team/english
  '((|907443| . "English BCP CS Agent")))

(provide employee-in-team?)
(define (employee-in-team? employee team)
  (ormap (lambda (team-schedule)
           (hash-has-key? (hash-ref employee
                                    'schedules)
                          (car team-schedule)))
         team))

(provide employee-in-team?/on-now)
(define (employee-in-team?/on-now employee team)
  (ormap (lambda (team-schedule)
           (equal? (hash-ref employee
                             'schedule_name)
                   (cdr team-schedule)))
         team))
