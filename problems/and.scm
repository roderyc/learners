(define *and-boolean-problem*
  (make-learning-problem/default-weights
   '((a1 1 0)
     (a2 1 0))
   '((g1 1 0))
   '(((g1 . 0) (a1 . 0) (a2 . 0))
     ((g1 . 0) (a1 . 0) (a2 . 1))
     ((g1 . 0) (a1 . 1) (a2 . 0))
     ((g1 . 1) (a1 . 1) (a2 . 1)))))
