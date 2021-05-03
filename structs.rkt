#lang racket

(provide
 (struct-out note-pkg)
 (struct-out onset-event)
 (struct-out hold-event)
 (struct-out tm)
 (struct-out um)
 (struct-out om)
 (struct-out rm-C)
 (struct-out rm-Csharp/Dflat)
 (struct-out rm-D)
 (struct-out rm-Dsharp/Eflat)
 (struct-out rm-E)
 (struct-out rm-F)
 (struct-out rm-Fsharp/Gflat)
 (struct-out rm-G)
 (struct-out rm-Gsharp/Aflat)
 (struct-out rm-A)
 (struct-out rm-Asharp/Bflat)
 (struct-out rm-B)
 (struct-out pm-1)
 (struct-out pm-1.5)
 (struct-out pm-2)
 (struct-out pm-2.5)
 (struct-out pm-3)
 (struct-out pm-4)
 (struct-out pm-4.5)
 (struct-out pm-5)
 (struct-out pm-5.5)
 (struct-out pm-6)
 (struct-out pm-6.5)
 (struct-out pm-7)
 (struct-out pm-rest)
 (struct-out pm-hold))


;; misc --------------------------------------

(struct note-pkg (pm rm) #:transparent)
(struct onset-event (char) #:transparent)
(struct hold-event () #:transparent)


;; tm (tempo  marker) -------------------------

;; `tempo` must be a multiple of 10 or null.
;; Null corresponds to no tempo change.
(struct tm (tempo) #:transparent) 


;; rm-* (root marker) --------------------------

(struct rm-C () #:transparent)
(struct rm-Csharp/Dflat () #:transparent)
(struct rm-D () #:transparent)
(struct rm-Dsharp/Eflat () #:transparent)
(struct rm-E () #:transparent)
(struct rm-F () #:transparent)
(struct rm-Fsharp/Gflat () #:transparent)
(struct rm-G () #:transparent)
(struct rm-Gsharp/Aflat () #:transparent)
(struct rm-A () #:transparent)
(struct rm-Asharp/Bflat () #:transparent)
(struct rm-B () #:transparent)



;; um (unitr [unit rhythm] marker) -----------

(struct um (denom) #:transparent)

;; om (oct [octave] marker) ---------------

;; `octave` should be either a number or null
;; a null octave indicates no change.
(struct om (octave) #:transparent)


;; pm=* (part marker) ------------------------

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
(struct pm-rest () #:transparent) ;; rest
(struct pm-hold() #:transparent) ;; hold
