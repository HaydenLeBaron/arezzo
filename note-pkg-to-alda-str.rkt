#lang racket

(require "structs.rkt")
(provide note-pkg->alda-str)


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
