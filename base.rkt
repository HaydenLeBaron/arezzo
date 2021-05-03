#lang racket

(require arezzo-lite/comp-table-to-alda
         arezzo-lite/utils
         arezzo-lite/arezzo-tokens)

(provide composition
         play!)

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



;; Takes the id of a composition and plays it out loud. Requires
;; Alda server to be running. Start the alda server by running `alda up`.
(define (play! comp-id)

  (with-handlers
    ([exn:fail:contract?
      (λ (exn) "play! does nothing when run from the racket REPL (or DrRacket). Run `racket this-file.rkt` in a shell while `alda up` is running to hear live playback.")])
    (let ([alda-exec-path
           (car (string-split (with-output-to-string (λ()
                                                       (system "which alda"))) "\n"))])
      (system* alda-exec-path "play" "--code" comp-id))
    ))


