#lang racket

(require arezzo-lite/utils)
(provide comp-table->alda)

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
                      ))
          (hash-ref comp 'gl-tempo) (hash-ref comp 'gl-unitr)
          (hash-ref comp 'v1-root) (hash-ref comp 'v2-root) (hash-ref comp 'v3-root) (hash-ref comp 'v4-root)
          (hash-ref comp 'v1-oct) (hash-ref comp 'v2-oct) (hash-ref comp 'v3-oct) (hash-ref comp 'v4-oct)
          (hash-ref comp 'v1-part) (hash-ref comp 'v2-part) (hash-ref comp 'v3-part) (hash-ref comp 'v4-part)
          )))))

