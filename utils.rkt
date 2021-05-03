#lang racket

(require arezzo-lite/arezzo-tokens
         arezzo-lite/structs
         arezzo-lite/note-pkg-to-alda-str
         )

(provide
 symbol->list
 char->event
 listof-symbol->listof-events
 group-by-voice
 v-slices->h-slices
 h-slices->alda-tok-list
 format-alda-tok-list
 holds->repeats
 holds->repeats-helper)


(define (symbol->list sym)
  (string->list (symbol->string sym)))

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
