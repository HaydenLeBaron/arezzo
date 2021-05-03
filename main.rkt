#lang racket

(struct onset-event (char) #:transparent)
(struct hold-event () #:transparent)


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


(define (group-by-voice v-slice)
  (let ([pkg-note (lambda (x y) (note-pkg x y))])
  (match v-slice
    [      
     `(voice-1 ,tm-1 ,om-1 ((,pm-1 ,rm-1) ,um-1)
       voice-2 ,tm-2 ,om-2 ((,pm-2 ,rm-2) ,um-2)
       voice-3 ,tm-3 ,om-3 ((,pm-3 ,rm-3) ,um-3)
       voice-4 ,tm-4 ,om-4 ((,pm-4 ,rm-4) ,um-4))
     

(let ([note-1 (pkg-note pm-1 rm-1)]
      [note-2 (pkg-note pm-2 rm-2)]
      [note-3 (pkg-note pm-3 rm-3)]
      [note-4 (pkg-note pm-4 rm-4)])
     `(
       (voice-1 ,tm-1 ,om-1 ((,note-1) ,um-1))
       (voice-2 ,tm-2 ,om-2 ((,note-2) ,um-2))
       (voice-3 ,tm-3 ,om-3 ((,note-3) ,um-3))
       (voice-4 ,tm-4 ,om-4 ((,note-4) ,um-4))
       ))
     ])))




;; Converts from "vertical slices" to "horizontal slices"
(define (v-slices->h-slices vs-list)
  (for/list ([k (in-range 4)])
    (flatten
     (list-ref
      (for/list ([j (in-range 4)])       ;; TODO: replace 4 with number of channels
        (for/list ([i (in-range (length vs-list))])
          (list-ref 
           (list-ref (map group-by-voice vs-list)
                     i) j)))
      k))
    ))

;; TODO: implement ;; BKMRK
;; Converts note-pkg to an equivalent alda string
(define (note-pkg->alda-str npkg)
  (let ([npkg-pm (note-pkg-pm npkg)]
        [npkg-rm (note-pkg-rm npkg)])
    (cond
      [(pm-hold? npkg-pm) "~"]
      [(pm-rest? npkg-pm) "r"]
      [(rm-C? npkg-rm)
       (cond
         [(pm-1? npkg-pm)   "c"]
         [(pm-1.5? npkg-pm) "c+"]
         [(pm-2? npkg-pm)   "c++"]
         [(pm-2.5? npkg-pm) "c+++"]
         [(pm-3? npkg-pm)   "c++++"]
         [(pm-4? npkg-pm)   "c+++++"]
         [(pm-4.5? npkg-pm) "c++++++"]
         [(pm-5? npkg-pm)   "c+++++++"]
         [(pm-5.5? npkg-pm) "c++++++++"]
         [(pm-6? npkg-pm)   "c+++++++++"]
         [(pm-6.5? npkg-pm) "c++++++++++"]
         [(pm-7? npkg-pm)   "c+++++++++++"]
         )
       ]
      [(rm-Csharp/Dflat? npkg-rm)
       (cond
         [(pm-1? npkg-pm)   "c+"]
         [(pm-1.5? npkg-pm) "c++"]
         [(pm-2? npkg-pm)   "c+++"]
         [(pm-2.5? npkg-pm) "c++++"]
         [(pm-3? npkg-pm)   "c+++++"]
         [(pm-4? npkg-pm)   "c++++++"]
         [(pm-4.5? npkg-pm) "c+++++++"]
         [(pm-5? npkg-pm)   "c++++++++"]
         [(pm-5.5? npkg-pm) "c+++++++++"]
         [(pm-6? npkg-pm)   "c++++++++++"]
         [(pm-6.5? npkg-pm) "c+++++++++++"]
         [(pm-7? npkg-pm)   "c++++++++++++"]
         )
       ]
      [(rm-D? npkg-rm)
       (cond
         [(pm-1? npkg-pm)   "d"]
         [(pm-1.5? npkg-pm) "d+"]
         [(pm-2? npkg-pm)   "d++"]
         [(pm-2.5? npkg-pm) "d+++"]
         [(pm-3? npkg-pm)   "d++++"]
         [(pm-4? npkg-pm)   "d+++++"]
         [(pm-4.5? npkg-pm) "d++++++"]
         [(pm-5? npkg-pm)   "d+++++++"]
         [(pm-5.5? npkg-pm) "d++++++++"]
         [(pm-6? npkg-pm)   "d+++++++++"]
         [(pm-6.5? npkg-pm) "d++++++++++"]
         [(pm-7? npkg-pm)   "d+++++++++++"]
         )
       ]
      [(rm-Dsharp/Eflat? npkg-rm)
       (cond
         [(pm-1? npkg-pm)   "d+"]
         [(pm-1.5? npkg-pm) "d++"]
         [(pm-2? npkg-pm)   "d+++"]
         [(pm-2.5? npkg-pm) "d++++"]
         [(pm-3? npkg-pm)   "d+++++"]
         [(pm-4? npkg-pm)   "d++++++"]
         [(pm-4.5? npkg-pm) "d+++++++"]
         [(pm-5? npkg-pm)   "d++++++++"]
         [(pm-5.5? npkg-pm) "d+++++++++"]
         [(pm-6? npkg-pm)   "d++++++++++"]
         [(pm-6.5? npkg-pm) "d+++++++++++"]
         [(pm-7? npkg-pm)   "d++++++++++++"]
         )
       ]
      [(rm-E? npkg-rm)
       (cond
         [(pm-1? npkg-pm)   "e"]
         [(pm-1.5? npkg-pm) "e+"]
         [(pm-2? npkg-pm)   "e++"]
         [(pm-2.5? npkg-pm) "e+++"]
         [(pm-3? npkg-pm)   "e++++"]
         [(pm-4? npkg-pm)   "e+++++"]
         [(pm-4.5? npkg-pm) "e++++++"]
         [(pm-5? npkg-pm)   "e+++++++"]
         [(pm-5.5? npkg-pm) "e++++++++"]
         [(pm-6? npkg-pm)   "e+++++++++"]
         [(pm-6.5? npkg-pm) "e++++++++++"]
         [(pm-7? npkg-pm)   "e+++++++++++"]
         )
       ]
      [(rm-F? npkg-rm)
       (cond
         [(pm-1? npkg-pm)   "f"]
         [(pm-1.5? npkg-pm) "f+"]
         [(pm-2? npkg-pm)   "f++"]
         [(pm-2.5? npkg-pm) "f+++"]
         [(pm-3? npkg-pm)   "f++++"]
         [(pm-4? npkg-pm)   "f+++++"]
         [(pm-4.5? npkg-pm) "f++++++"]
         [(pm-5? npkg-pm)   "f+++++++"]
         [(pm-5.5? npkg-pm) "f++++++++"]
         [(pm-6? npkg-pm)   "f+++++++++"]
         [(pm-6.5? npkg-pm) "f++++++++++"]
         [(pm-7? npkg-pm)   "f+++++++++++"]
         )
       ]
      [(rm-Fsharp/Gflat? npkg-rm)
       (cond
         [(pm-1? npkg-pm)   "f+"]
         [(pm-1.5? npkg-pm) "f++"]
         [(pm-2? npkg-pm)   "f+++"]
         [(pm-2.5? npkg-pm) "f++++"]
         [(pm-3? npkg-pm)   "f+++++"]
         [(pm-4? npkg-pm)   "f++++++"]
         [(pm-4.5? npkg-pm) "f+++++++"]
         [(pm-5? npkg-pm)   "f++++++++"]
         [(pm-5.5? npkg-pm) "f+++++++++"]
         [(pm-6? npkg-pm)   "f++++++++++"]
         [(pm-6.5? npkg-pm) "f+++++++++++"]
         [(pm-7? npkg-pm)   "f++++++++++++"]
         )
       ]
      [(rm-G? npkg-rm)
       (cond
         [(pm-1? npkg-pm)   "g"]
         [(pm-1.5? npkg-pm) "g+"]
         [(pm-2? npkg-pm)   "g++"]
         [(pm-2.5? npkg-pm) "g+++"]
         [(pm-3? npkg-pm)   "g++++"]
         [(pm-4? npkg-pm)   "g+++++"]
         [(pm-4.5? npkg-pm) "g++++++"]
         [(pm-5? npkg-pm)   "g+++++++"]
         [(pm-5.5? npkg-pm) "g++++++++"]
         [(pm-6? npkg-pm)   "g+++++++++"]
         [(pm-6.5? npkg-pm) "g++++++++++"]
         [(pm-7? npkg-pm)   "g+++++++++++"]
         )
       ]
      [(rm-Gsharp/Aflat? npkg-rm)
       (cond
         [(pm-1? npkg-pm)   "g+"]
         [(pm-1.5? npkg-pm) "g++"]
         [(pm-2? npkg-pm)   "g+++"]
         [(pm-2.5? npkg-pm) "g++++"]
         [(pm-3? npkg-pm)   "g+++++"]
         [(pm-4? npkg-pm)   "g++++++"]
         [(pm-4.5? npkg-pm) "g+++++++"]
         [(pm-5? npkg-pm)   "g++++++++"]
         [(pm-5.5? npkg-pm) "g+++++++++"]
         [(pm-6? npkg-pm)   "g++++++++++"]
         [(pm-6.5? npkg-pm) "g+++++++++++"]
         [(pm-7? npkg-pm)   "g++++++++++++"]
         )
       ]
      [(rm-A? npkg-rm)
       (cond
         [(pm-1? npkg-pm)   "a"]
         [(pm-1.5? npkg-pm) "a+"]
         [(pm-2? npkg-pm)   "a++"]
         [(pm-2.5? npkg-pm) "a+++"]
         [(pm-3? npkg-pm)   "a++++"]
         [(pm-4? npkg-pm)   "a+++++"]
         [(pm-4.5? npkg-pm) "a++++++"]
         [(pm-5? npkg-pm)   "a+++++++"]
         [(pm-5.5? npkg-pm) "a++++++++"]
         [(pm-6? npkg-pm)   "a+++++++++"]
         [(pm-6.5? npkg-pm) "a++++++++++"]
         [(pm-7? npkg-pm)   "a+++++++++++"]
         )
       ]
      [(rm-Asharp/Bflat? npkg-rm)
       (cond
         [(pm-1? npkg-pm)   "a+"]
         [(pm-1.5? npkg-pm) "a++"]
         [(pm-2? npkg-pm)   "a+++"]
         [(pm-2.5? npkg-pm) "a++++"]
         [(pm-3? npkg-pm)   "a+++++"]
         [(pm-4? npkg-pm)   "a++++++"]
         [(pm-4.5? npkg-pm) "a+++++++"]
         [(pm-5? npkg-pm)   "a++++++++"]
         [(pm-5.5? npkg-pm) "a+++++++++"]
         [(pm-6? npkg-pm)   "a++++++++++"]
         [(pm-6.5? npkg-pm) "a+++++++++++"]
         [(pm-7? npkg-pm)   "a++++++++++++"]
         )
       ]
      [(rm-B? npkg-rm)
       (cond
         [(pm-1? npkg-pm)   "b"]
         [(pm-1.5? npkg-pm) "b+"]
         [(pm-2? npkg-pm)   "b++"]
         [(pm-2.5? npkg-pm) "b+++"]
         [(pm-3? npkg-pm)   "b++++"]
         [(pm-4? npkg-pm)   "b+++++"]
         [(pm-4.5? npkg-pm) "b++++++"]
         [(pm-5? npkg-pm)   "b+++++++"]
         [(pm-5.5? npkg-pm) "b++++++++"]
         [(pm-6? npkg-pm)   "b+++++++++"]
         [(pm-6.5? npkg-pm) "b++++++++++"]
         [(pm-7? npkg-pm)   "b+++++++++++"]
         )
       ]
      [else npkg]
      ))
  )


#| Converts list of form:

'((voice-1
   #<tm-240>
   #<om-4>
   #<pm-1>
   #<rm-C>
   #<um-4>
   voice-1
   #<tm-null>
   #<om-null>
   #<pm-1.5>
   #<rm-C>
   #<um-4>
   voice-1
   #<tm-null>
   #<om-null>
   #<pm-hold>
   #<rm-C>
   #<um-4>
   voice-1
   ....
  (voice-2
  ....))

to a list of list of strings, where each inner list represents an alda line
and each string represents an alda token.
|#
(define (h-slices->alda-tok-list hs-list)
  ;; Convert tm, om, and um
  (map
   (lambda (line)
     (map
      (lambda (elt)
        (cond
          [(symbol? elt) "|"]
          [else (cond
                  [(tm? elt)
                   (if (null? (tm-tempo elt))
                       ""
                       (string-append
                        "(tempo " (number->string (tm-tempo elt)) ")"))]
                  [(om? elt)
                   (if (null? (om-octave elt))
                       ""
                       (string-append " o" (number->string (om-octave elt)) " "))]
                  [(um? elt)
                   (number->string (um-denom elt))]
                  [(note-pkg? elt)
                   (note-pkg->alda-str elt)
                   ]
                  [else (raise-argument-error
                         'h-slices->alda-tok-list
                         "expected tm, om, um, or note-pkg--but didn't recieve any."
                         elt)]
                  )]))
      line))
   hs-list))


(define (format-alda-tok-list alda-toks)
  (let* ([lines
         (map (lambda (line) (string-join line "")) alda-toks
       )]
         [line-1 (list-ref lines 0)]
         [line-2 (list-ref lines 1)]
         [line-3 (list-ref lines 2)]
         [line-4 (list-ref lines 3)])
    (string-append
     "piano \"piano-1\":" line-1 " "
     "piano \"piano-2\":" line-2 " "
     "piano \"piano-3\":" line-3 " "
     "piano \"piano-4\":" line-4 " "
    )))
    
    

;; Takes a hash-table representation of a composition `comp`
;; and returns a string of alda code representing it.


  
  ;; t-i ur-i v*-r-i v*-o-i v*-p-i
  ;; => (t-i | "") (v*-o-i | "") (v*-p-i + v*-r-i)(v*-ur-i)
  ;; => (tempo 140) o5           c+++++3
  ;; |  ""          ""           ~3

(define (comp-table->alda comp)
  (format-alda-tok-list
   (h-slices->alda-tok-list
    (v-slices->h-slices
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
          
          )))
   ))

;;===============================================
;; tm-* (tempo marker)
;;===============================================

;; tempo must be a multiple of 10 or null.
;; Null corresponds to no tempo change.
(struct tm (tempo) #:transparent) 


;;===============================================
;; rm-* (root marker)
;;===============================================

(struct rm-C () #:transparent)
(struct rm-Csharp/Dflat () #:transparent)
 
(struct rm-D () #:transparent)
(struct rm-Dsharp/Eflat () #:transparent)

(struct rm-E () #:transparent)

(struct rm-F ())
(struct rm-Fsharp/Gflat () #:transparent)

(struct rm-G () #:transparent)
(struct rm-Gsharp/Aflat () #:transparent)

(struct rm-A () #:transparent)
(struct rm-Asharp/Bflat () #:transparent)

(struct rm-B () #:transparent)



;;===============================================
;; um-* (unitr [unit rhythm] marker)
;;===============================================

(struct um (denom) #:transparent)

;;===============================================
;; om-* (oct [octave] marker)
;;===============================================

;; `octave` should be either a number or null
;; a null octave indicates no change.
(struct om (octave) #:transparent)



;;===============================================
;; pm=* (part marker)
;;===============================================

(struct pm-1 () #:transparent)   ;; 1st note of chromatic scale (imagine c key on piano, but transposed)
(struct pm-1.5 () #:transparent) ;; 2nd note of chromatic scale (imagine c# key on piano, but transposed)

(struct pm-2 () #:transparent) ;; ....
(struct pm-2.5 () #:transparent) 

(struct pm-3 () #:transparent)

(struct pm-4 () #:transparent)
(struct pm-4.5 () #:transparent) 

(struct pm-5 () #:transparent)
(struct pm-5.5 () #:transparent) 

(struct pm-6 () #:transparent)
(struct pm-6.5 () #:transparent)

(struct pm-7 () #:transparent) ;; ....

(struct pm-rest () #:transparent) ;; rest (r in alda)

(struct pm-hold() #:transparent) ;; hold (~ in alda)

;; NOTE PACKAGE
(struct note-pkg (pm rm) #:transparent)



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



;; Takes every event in event list and returns an identical list
;; except that all hold events are converted to being the same event
;; as the last non-hold event.
;; example: '(X hold Y hold hold Z hold) => '(X X Y Y Y Z Z)
(define (holds->repeats event-list)
  (holds->repeats-helper (car event-list) event-list empty))

;; Helper for holds->repeats (driver of this function)
(define (holds->repeats-helper last-nonhold rest-of-list acc-list)
  (cond
    [(empty? rest-of-list) (reverse acc-list)]
    [else
     (let ([curr (car rest-of-list)])
     (cond
       [(hold-event? curr)
        (holds->repeats-helper last-nonhold (rest rest-of-list) (cons last-nonhold acc-list))]
       [else
        (holds->repeats-helper curr (rest rest-of-list) (cons curr acc-list))]))]))


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

(provide composition)


;; Takes the id of a composition and plays it out loud. Requires
;; Alda server to be running. Start the alda server by running `alda up`.
(define (play! comp-id)
  (let ([alda-exec-path
         (car (string-split (with-output-to-string (λ()
                                                     (system "which alda"))) "\n"))])
    (system* alda-exec-path "play" "--code" comp-id))
  )

(provide play!)
