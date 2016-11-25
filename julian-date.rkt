#lang racket/base
(require racket/date)
; Compute Julian day number (and fraction) given timezone, time of
; day, day of month, month, and year
; https://en.wikipedia.org/wiki/Julian_day#Calculation
(define (julian-date year month day hour minute second timezone-offset)
  (let* ([a (floor (/ (- 14 month) 12))]
         [y (+ year 4800 (* -1 a))]
         [m (+ month (* 12 a) -3)]
         [days-since-march (floor (/ (+ (* 153 m) 2) 5))]
         [jdn (+ day days-since-march
                 (* 365 y)
                 (floor (/ y 4))
                 (* -1 (floor (/ y 100)))
                 (floor (/ y 400))                        
                 -32045)]
         [jd (exact->inexact 
              (+ jdn
                (/ (- (- hour timezone-offset) 12) 24)
                (/ minute 1440)
                (/ second 86400)))])
    jd))

(let* ([d  (current-date)]
       [jd (julian-date (date-year d)
               (date-month d)
               (date-day d)
               (date-hour d)
               (date-minute d)
               (date-second d)
               (/ (date-time-zone-offset d) 3600))])
       (displayln (real->decimal-string jd 4)))