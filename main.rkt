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
   v4-part v4-part-body
  )

  
  (define comp-name
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
     v4-part (listof-symbol->listof-events v4-part-body)
     )))

     


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





