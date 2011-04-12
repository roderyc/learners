;;; Uses (subset learning-problem (consistent copy-learning-problem/weights))
;;;      (subset srfi-1 (first second third))

;;; Takes a learning-problem, a learning algorithm, and a number specifying the
;;; number of hypotheses to include in an ensemble. Produces a learning
;;; algorithm that is the result of running ADABOOST on the given inputs.
;;;
;;; For now, only works for problems with one goal that classifies with -1 and 1
;;; (learning-problem -> hypothesis) -> learning-problem -> integer -> hypothesis
(define (adaboost learner problem n-hypotheses)
  (let* ((hypotheses (make-vector n-hypotheses #f))
         (h-weights (make-vector n-hypotheses #f))
         (e-weights (learning-problem:weights problem)))
    (do ((k 0 (+ k 1))
         (new-problem problem (copy-learning-problem/weights problem e-weights)))
        ((>= k n-hypotheses) (weighted-majority
                              hypotheses h-weights
                              (car (learning-problem:goals problem))))
      (vector-set! hypotheses k (learner new-problem))
      (let* ((error (fold (lambda (e w error-acc)
                            (if (not (consistent
                                      (list e) (vector-ref hypotheses k)))
                                (+ error-acc w)
                                error-acc))
                          0
                          (learning-problem:examples new-problem)
                          (learning-problem:weights new-problem)))
             (new-weights (map (lambda (e w)
                                 (if (consistent (list e)
                                                 (vector-ref hypotheses k))
                                     (* w (/ error (- 1 error)))
                                     w))
                               (learning-problem:examples new-problem)
                               (learning-problem:weights new-problem))))
        (set! e-weights (normalize new-weights))
        (vector-set! h-weights k (log (/ (- 1 error) error)))))))

;;; Returns a hypothesis that uses the output value with the highest votes from
;;; the hypotheses in the given vector, using the given weights for each.
;;; vector of hypotheses -> vector of numbers -> (attr val ...) -> hypothesis
(define (weighted-majority hypotheses weights goal)
  (lambda (input)
    (let loop ((scores '())
               (i 0))
      (if (>= i (vector-length hypotheses))
          (cons (first goal) (if (> (apply + scores) 0)
                                (second goal)
                                (third goal)))
          (let* ((answer ((vector-ref hypotheses i) input))
                 (score (* (if (equal? (cdr answer) (second goal))
                               1 -1) (vector-ref weights i))))
            (loop (cons score scores) (+ i 1)))))))

;;; Normalizes the values in the given list
;;; list of numbers -> list of numbers
(define (normalize values)
  (let* ((sum (apply + values))
         (norm-factor (/ 1 sum)))
    (map (lambda (v)
           (* v norm-factor)) values)))
