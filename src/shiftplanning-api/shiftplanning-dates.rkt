#lang racket/base

(require racket/date)

(define (number->month n)
  (define months
    #hash((1 . "Jan")
          (2 . "Feb")
          (3 . "Mar")
          (4 . "Apr")
          (5 . "May")
          (6 . "Jun")
          (7 . "Jul")
          (8 . "Aug")
          (9 . "Sep")
          (10 . "Oct")
          (11 . "Nov")
          (12 . "Dec")))
  (hash-ref months n))

(provide date->shiftplanning-date)
(define (date->shiftplanning-date d)
  (format "~a ~a, ~a"
          (date-month d)
          (date-day d)
          (date-year d)))

(module+ main
  (date->shiftplanning-date (current-date)))
