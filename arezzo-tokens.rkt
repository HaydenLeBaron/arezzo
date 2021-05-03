#lang racket

(require arezzo-lite/structs)

(provide
 is-hold?
 events->tempo-markers
 events->unitr-markers
 events->root-markers
 events->oct-markers
 events->part-markers)

#|This file contains information related to the
meaning of tokens / chars in the arezzo language.|#

;; Defines chars that represent "holds"
(define (is-hold? c)
  (or (equal? c #\-) (equal? c #\=)))

;; Converts generalized onset-events and hold-events to specific based on TEMPO channel.
(define (events->tempo-markers event-list)
  (map (lambda (event)
         (cond
           [(hold-event? event) (tm null)]
           [(onset-event? event)
            (case (onset-event-char event)
              [(#\A) (tm 10)]
              [(#\B) (tm 20)]
              [(#\C) (tm 30)]
              [(#\D) (tm 40)]
              [(#\E) (tm 50)]
              [(#\F) (tm 60)]
              [(#\G) (tm 70)]
              [(#\H) (tm 80)]
              [(#\I) (tm 90)]
              [(#\J) (tm 100)]
              [(#\K) (tm 110)]
              [(#\L) (tm 120)]
              [(#\M) (tm 130)]
              [(#\N) (tm 140)]
              [(#\O) (tm 150)]
              [(#\P) (tm 160)]
              [(#\Q) (tm 170)]
              [(#\R) (tm 180)]
              [(#\S) (tm 190)]
              [(#\T) (tm 200)]
              [(#\U) (tm 210)]
              [(#\V) (tm 220)]
              [(#\W) (tm 230)]
              [(#\X) (tm 240)]
              [(#\Y) (tm 250)]
              [(#\Z) (tm 260)])]
           [else (raise-argument-error
                  'events->tempo-markers
                  "expected onset or hold event, but received neither."
                  event)]
           ))
       event-list))

;; Converts generalized onset-events and hold-events to specific based on UNITR channel.
(define (events->unitr-markers event-list)
  (map (lambda (event)
         (if (onset-event? event)
             (case (onset-event-char event)
               [(#\1) (um 1)]
               [(#\2) (um 2)]
               [(#\3) (um 3)]
               [(#\4) (um 4)]
               [(#\5) (um 5)]
               [(#\6) (um 6)]
               [(#\7) (um 7)]
               [(#\8) (um 8)]
               [(#\9) (um 9)]
               [(#\S) (um 16)]
               [(#\T) (um 32)]
               [(#\X) (um 64)]
               [(#\O) (um 128)])
            (raise-argument-error
                  'events->unitr-markers
                  "expected onset event but didn't receive it."
                  event)))
       event-list))


;; Converts generalized onset-events and hold-events to specific based on ROOT channel.
(define (events->root-markers event-list)
  (map (lambda (event)
         (if (onset-event? event)
             (case (onset-event-char event)
               [(#\0) (rm-C)]
               [(#\1) (rm-Csharp/Dflat)]
               [(#\2) (rm-D)]
               [(#\3) (rm-Dsharp/Eflat)]
               [(#\4) (rm-E)]
               [(#\5) (rm-F)]
               [(#\6) (rm-Fsharp/Gflat)]
               [(#\7) (rm-G)]
               [(#\8) (rm-Gsharp/Aflat)]
               [(#\9) (rm-A)]
               [(#\A) (rm-Asharp/Bflat)]
               [(#\B) (rm-B)])
            (raise-argument-error
                  'events->root-markers
                  "expected onset event but didn't receive it."
                  event)))
       event-list))


;; Converts generalized onset-events and hold-events to specific based on OCT channel.
(define (events->oct-markers event-list)
  (map (lambda (event)
         (cond
           [(hold-event? event) (om null)]
           [(onset-event? event)
            (case (onset-event-char event)
              [(#\0) (om 0)]
              [(#\1) (om 1)]
              [(#\2) (om 2)]
              [(#\3) (om 3)]
              [(#\4) (om 4)]
              [(#\5) (om 5)]
              [(#\6) (om 6)]
              [(#\7) (om 7)]
              [(#\8) (om 8)]
              [(#\9) (om 9)])]
           [else (raise-argument-error
                  'events->oct-markers
                  "expected onset or hold event, but received neither."
                  event)]
           ))
       event-list))

;; Converts generalized onset-events and hold-events to specific based on PART channel.
(define (events->part-markers event-list)
  (map (lambda (event)
         (cond
           [(hold-event? event) (pm-hold)]
           [(onset-event? event)
            (case (onset-event-char event)
              [(#\.) (pm-rest)]
              [(#\1) (pm-1)]
              [(#\!) (pm-1.5)]
              [(#\2) (pm-2)]
              [(#\@) (pm-2.5)]
              [(#\3) (pm-3)]
              [(#\4) (pm-4)]
              [(#\$) (pm-4.5)]
              [(#\5) (pm-5)]
              [(#\%) (pm-5.5)]
              [(#\6) (pm-6)]
              [(#\^) (pm-6.5)]
              [(#\7) (pm-7)])]
           [else (raise-argument-error
                  'events->part-markers
                  "expected onset or hold event, but received neither."
                  event)]
           ))
       event-list))
