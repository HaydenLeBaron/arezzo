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

  ;; TODO: translate events and holds into very specific structs identifying exactly what kind of entity something is, regardless of context
  ;; hold in tempo and *-oct channels = empty string
  ;; hold in unitr and *-root channels = repeat last
  ;; hold in part channel = ~
  (define comp-name
    (comp-table->alda
     (hasheqv
      gl-tempo (listof-symbol->listof-events gl-tempo-body)
      gl-unitr (listof-symbol->listof-events gl-unitr-body)
      v1-root (listof-symbol->listof-events v1-root-body)
      v2-root (listof-symbol->listof-events v2-root-body)
      v3-root (listof-symbol->listof-events v3-root-body)
      v4-root (listof-symbol->listof-events v4-root-body)
      v1-oct (listof-symbol->listof-events v1-oct-body)
      v2-oct (listof-symbol->listof-events v2-oct-body)
      v3-oct (listof-symbol->listof-events v3-oct-body)
      v4-oct (listof-symbol->listof-events v4-oct-body)
      v1-part (listof-symbol->listof-events v1-part-body)
      v2-part (listof-symbol->listof-events v2-part-body)
      v3-part (listof-symbol->listof-events v3-part-body)
      v4-part (listof-symbol->listof-events v4-part-body)))))


     


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





