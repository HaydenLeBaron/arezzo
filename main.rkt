#lang racket

(struct onset-event (char) #:transparent)
(struct hold-event ())


(define (symbol->list sym)
  (string->list (symbol->string sym)))

;; Defines chars that represent "holds"
(define (is-hold? c)
  (or (equal? c #\-) (equal? c #\=)))

(define (char->event c)
  (if (is-hold? c)
      (hold-event)
      (onset-event c)))



(define (listof-symbol->listof-events los)
    (map char->event (symbol->list los)))


;; Takes a hash-table representation of a composition `comp`
;; and returns a string of alda code representing it.


  
  ;; t-i ur-i v*-r-i v*-o-i v*-p-i
  ;; => (t-i | "") (v*-o-i | "") (v*-p-i + v*-r-i)(v*-ur-i)
  ;; => (tempo 140) o5           c+++++3
  ;; |  ""          ""           ~3

(define (comp-table->alda comp)
  (map (lambda (t-i ur-i
                   v1-r-i v2-r-i v3-r-i v4-r-i
                   v1-o-i v2-o-i v3-o-i v4-o-i
                   v1-p-i v2-p-i v3-p-i v4-p-i)
         (list 'voice-1 t-i v1-o-i (list (list v1-p-i v1-r-i) ur-i)
               'voice-2 t-i v2-o-i (list (list v2-p-i v2-r-i) ur-i)
               'voice-3 t-i v3-o-i (list (list v3-p-i v3-r-i) ur-i)
               'voice-4 t-i v4-o-i (list (list v4-p-i v4-r-i) ur-i)
           )
         )

       (hash-ref comp 'gl-tempo) (hash-ref comp 'gl-unitr)
       (hash-ref comp 'v1-root) (hash-ref comp 'v2-root) (hash-ref comp 'v3-root) (hash-ref comp 'v4-root)
       (hash-ref comp 'v1-oct) (hash-ref comp 'v2-oct) (hash-ref comp 'v3-oct) (hash-ref comp 'v4-oct)
       (hash-ref comp 'v1-part) (hash-ref comp 'v2-part) (hash-ref comp 'v3-part) (hash-ref comp 'v4-part)
  ))

;;===============================================
;; tm-* (tempo marker)
;;===============================================

(struct tm-10 ()) ;; tempo = 10
(struct tm-20 ()) ;; tempo = 20
(struct tm-30 ()) ;; ....
(struct tm-40 ())
(struct tm-50 ())
(struct tm-60 ())
(struct tm-70 ())
(struct tm-80 ())
(struct tm-90 ())
(struct tm-100 ())
(struct tm-110 ())
(struct tm-120 ())
(struct tm-130 ())
(struct tm-140 ())
(struct tm-150 ())
(struct tm-160 ())
(struct tm-170 ())
(struct tm-180 ())
(struct tm-190 ())
(struct tm-200 ())
(struct tm-210 ())
(struct tm-220 ())
(struct tm-230 ())
(struct tm-240 ())
(struct tm-250 ())
(struct tm-260 ())
(struct tm-null ()) ;; No tempo change (maps to single alda whitespace)

;;===============================================
;; rm-* (root marker)
;;===============================================

(struct rm-C ())
(struct rm-Csharp/Dflat ())
 
(struct rm-D ())
(struct rm-Dsharp/Eflat ())

(struct rm-E ())

(struct rm-F ())
(struct rm-Fsharp/Gflat ())

(struct rm-G ())
(struct rm-Gsharp/Aflat ())

(struct rm-A ())
(struct rm-Asharp/Bflat ())

(struct rm-B ())



;;===============================================
;; um-* (unitr [unit rhythm] marker)
;;===============================================

(struct um-1 ()) ;; whole note
(struct um-2 ()) ;; half note
(struct um-3 ()) ;; ....
(struct um-4 ())
(struct um-5 ())
(struct um-6 ())
(struct um-7 ())
(struct um-8 ())
(struct um-9 ())
(struct um-16 ())
(struct um-32 ())
(struct um-64 ())
(struct um-128 ())


;;===============================================
;; om-* (oct [octave] marker)
;;===============================================

(struct om-0 ()) ;; octave 0
(struct om-1 ()) ;; octave 1
(struct om-2 ()) ;; octave 2
(struct om-3 ()) ;; ....
(struct om-4 ())
(struct om-5 ())
(struct om-6 ())
(struct om-7 ())
(struct om-8 ())
(struct om-9 ())
(struct om-null ()) ;; no octave change (maps to single alda whitespace)



;;===============================================
;; pm=* (part marker)
;;===============================================

(struct pm-1 ())   ;; 1st note of chromatic scale (imagine c key on piano, but transposed)
(struct pm-1.5 ()) ;; 2nd note of chromatic scale (imagine c# key on piano, but transposed)

(struct pm-2 ()) ;; ....
(struct pm-2.5 ()) 

(struct pm-3 ())

(struct pm-4 ())
(struct pm-4.5 ()) 

(struct pm-5 ())
(struct pm-5.5 ()) 

(struct pm-6 ())
(struct pm-6.5 ())

(struct pm-7 ()) ;; ....

(struct pm-rest ()) ;; rest (r in alda)

(struct pm-hold()) ;; hold (~ in alda)





;; Converts generalized onset-events and hold-events to specific based on TEMPO channel.
(define (events->tempo-markers event-list)
  (map (lambda (event)
         (cond
           [(hold-event? event) (tm-null)]
           [(onset-event? event)
            (case (onset-event-char event)
              [(#\A) (tm-10)]
              [(#\B) (tm-20)]
              [(#\C) (tm-30)]
              [(#\D) (tm-40)]
              [(#\E) (tm-50)]
              [(#\F) (tm-60)]
              [(#\G) (tm-70)]
              [(#\H) (tm-80)]
              [(#\I) (tm-90)]
              [(#\J) (tm-100)]
              [(#\K) (tm-110)]
              [(#\L) (tm-120)]
              [(#\M) (tm-130)]
              [(#\N) (tm-140)]
              [(#\O) (tm-150)]
              [(#\P) (tm-160)]
              [(#\Q) (tm-170)]
              [(#\R) (tm-180)]
              [(#\S) (tm-190)]
              [(#\T) (tm-200)]
              [(#\U) (tm-210)]
              [(#\V) (tm-220)]
              [(#\W) (tm-230)]
              [(#\X) (tm-240)]
              [(#\Y) (tm-250)]
              [(#\Z) (tm-260)])]
           [else (raise-argument-error
                  'events->tempo-markers
                  "expected onset or hold event, but received neither."
                  event)]
           ))
       event-list))

;; Converts generalized onset-events and hold-events to specific based on UNITR channel.
;; TODO: NEXT
(define (events->unitr-markers event-list)
  (map (lambda (event)
         (identity event)
         )
       event-list))

;; Converts generalized onset-events and hold-events to specific based on ROOT channel.
(define (events->root-markers event-list)
  (map (lambda (event)
         (identity event)
         )
       event-list))

;; Converts generalized onset-events and hold-events to specific based on OCT channel.
(define (events->oct-markers event-list)
  (map (lambda (event)
         (cond
           [(hold-event? event) (om-null)]
           [(onset-event? event)
            (case (onset-event-char event)
              [(#\0) (om-0)]
              [(#\1) (om-1)]
              [(#\2) (om-2)]
              [(#\3) (om-3)]
              [(#\4) (om-4)]
              [(#\5) (om-5)]
              [(#\6) (om-6)]
              [(#\7) (om-7)]
              [(#\8) (om-8)]
              [(#\9) (om-9)])]
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



;; Takes every event in event list and returns an identical list
;; except that all hold events are converted to being the same event
;; as the last non-hold event.
;; example: '(X hold Y hold hold Z hold) => '(X X Y Y Y Z Z)
(define (holds->repeats event-list)
  event-list ;; TODO: implement
  )

(define-syntax-rule
  (composition
   comp-name
   gl-tempo gl-tempo-body
   gl-unitr gl-unitr-body
   v1-root v1-root-body
   v2-root v2-root-body
   v3-root v3-root-body
   v4-root v4-root-body
   v1-oct v1-oct-body
   v2-oct v2-oct-body
   v3-oct v3-oct-body
   v4-oct v4-oct-body
   v1-part v1-part-body
   v2-part v2-part-body
   v3-part v3-part-body
   v4-part v4-part-body)

  ;; TODO: translate events and holds into very specific structs identifying exactly what kind
  ;; of entity something is, regardless of context
  ;; hold in tempo and *-oct channels = empty string
  ;; hold in unitr and *-root channels = repeat last
  ;; hold in part channel = ~
  (define comp-name
    (comp-table->alda
     (hasheqv
      gl-tempo (events->tempo-markers (listof-symbol->listof-events gl-tempo-body))
      gl-unitr (events->unitr-markers (holds->repeats
                                       (listof-symbol->listof-events gl-unitr-body)))
      v1-root (events->root-markers (holds->repeats
                                     (listof-symbol->listof-events v1-root-body)))
                                    
      v2-root (events->root-markers (holds->repeats
                                     (listof-symbol->listof-events v2-root-body)))
      
      v3-root (events->root-markers (holds->repeats
                                     (listof-symbol->listof-events v3-root-body)))
      
      v4-root (events->root-markers (holds->repeats
                                     (listof-symbol->listof-events v4-root-body)))
      v1-oct (events->oct-markers (listof-symbol->listof-events v1-oct-body))
      v2-oct (events->oct-markers (listof-symbol->listof-events v2-oct-body))
      v3-oct (events->oct-markers (listof-symbol->listof-events v3-oct-body))
      v4-oct (events->oct-markers (listof-symbol->listof-events v4-oct-body))
      v1-part (events->part-markers (listof-symbol->listof-events v1-part-body))
      v2-part (events->part-markers (listof-symbol->listof-events v2-part-body))
      v3-part (events->part-markers (listof-symbol->listof-events v3-part-body))
      v4-part (events->part-markers (listof-symbol->listof-events v4-part-body))))))


     


;; arezzo lite. Defines a composition named "sonata"
(composition
 sonata
 'gl-tempo 'X---Y-----
 'gl-unitr '4---3-----
 'v1-root 'c---0---d-
 'v2-root 'c---------
 'v3-root 'c---------
 'v4-root 'c---------
 'v1-oct '4----5----
 'v2-oct '4---------
 'v3-oct '3---------
 'v4-oct '3---------
 'v1-part '1!=2.-3===
 'v2-part '3=@55-1===
 'v3-part '5==5543===
 'v4-part '1==5=5==.-
 )





