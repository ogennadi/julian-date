#lang racket
(require gregor
         rackunit)

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

(check-= (julian-date 2000 01 01 12 00 00 00) 2451545.0000 0.00005)
(check-= (julian-date 2011 07 29 14 00 00 +2) 2455772.0000 0.00005)
(check-= (julian-date 1961 08 01 00 00 00 00) 2437512.5000 0.00005)

; Display iso8601 date corresponding to the given Julian Date (not date
; in Julian calendar, mind you)
(define (julian-date->gregorian number)
  (string-append (datetime->iso8601  (jd->datetime number)) "Z"))

(check-equal? (julian-date->gregorian 2451545) "2000-01-01T12:00:00Z")
(check-equal? (julian-date->gregorian 2455772) "2011-07-29T12:00:00Z")
(check-equal? (julian-date->gregorian 2437512.5) "1961-08-01T00:00:00Z")