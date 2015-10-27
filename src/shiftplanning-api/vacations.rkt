#lang racket/base

(require "api-call.rkt")

(provide get/vacations)
(define (get/vacations)
  (api-call "schedule.vacations"
            "GET"))

(module+ main
  (require racket/pretty)
  (pretty-print (get/vacations)))
