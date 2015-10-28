#lang racket/base

(provide date->shiftplanning-date)
(define (date->shiftplanning-date d)
  (format "~a ~a, ~a"
          (date-month d)
          (date-day d)
          (date-year d)))

(module+ main
  (date->shiftplanning-date (current-date)))
