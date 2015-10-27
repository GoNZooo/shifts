#lang racket/base

(require "api-call.rkt"
         "team-members.rkt")

(define (team-cached? team-name)
  (file-exists? (format "cache/teams/~a.cache"
                        team-name)))

(define (cache-team team-name team-data)
  (call-with-output-file (format "cache/teams/~a.cache"
                                 team-name)
                         (lambda (out)
                           (write team-data out))
                         #:exists 'replace))

(define (read-cached-team team-name)
  (call-with-input-file (format "cache/teams/~a.cache"
                                team-name)
                        read))

(define (employees-cached?)
  (file-exists? "cache/employees.cache"))

(define (cache-employees employee-data)
  (call-with-output-file "cache/employees.cache"
                         (lambda (out)
                           (write employee-data out))
                         #:exists 'replace))

(define (read-cached-employees)
  (call-with-input-file "cache/employees.cache"
                        read))

(provide get/employees)
(define (get/employees #:cache? [cache? #t])
  (if (and cache?
           (employees-cached?))
    (read-cached-employees)
    (api-call #:module "staff.employees"
              #:method "GET")))

(define (get/employees/team [team-name "cs_row"]
                            #:cache? [cache? #t])
  (if (and cache?
           (team-cached? team-name))
    (read-cached-team team-name)
    (let* ([es (get/employees #:cache? cache?)]
           [team-employees
             (filter (lambda (employee)
                       (employee-in-team?/schedule employee
                                                   (schedules team-name)))
                     es)])
      (cache-team team-name team-employees)
      team-employees)))

(provide employee-in-team?)
(define (employee-in-team? employee-id team
                           #:cache? [cache? #t])
  (hash-has-key? (team->employee-ids team
                                     #:cache? cache?)
                 employee-id))

(provide employee-in-team?/schedule)
(define (employee-in-team?/schedule employee schedules)
  (for/or ([s schedules])
    (hash-has-key? (hash-ref employee 'schedules)
                   (car s))))

(provide team->employee-ids)
(define (team->employee-ids team
                            #:cache? [cache? #t])
  (for/hash ([e (get/employees/team team
                                    #:cache? cache?)])
    (values (hash-ref e
                      'id)
            (hash-ref e
                      'name))))

(module+ main
  (require racket/pretty)
  (pretty-print (team->employee-ids "cs_row")))
