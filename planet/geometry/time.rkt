#lang typed/racket

(provide (all-defined-out))

(define days-per-year 365)
(define hours-per-day 24)
(define minutes-per-hour 60)
(define seconds-per-minute 60)

(define minutes-per-day (* hours-per-day minutes-per-hour))
(define seconds-per-day (* minutes-per-day seconds-per-minute))

(define hours-per-year (* days-per-year hours-per-day))
(define minutes-per-year (* hours-per-year minutes-per-hour))
(define seconds-per-year (* minutes-per-year seconds-per-minute))
