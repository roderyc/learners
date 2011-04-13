;;; Uses (subset learning-problem (learning-problem:* attribute-value))
;;;      (subset srfi-1 (filter every count assoc))

;;; Returns a hypothesis based on a decision tree generated from problem.
;;; Currently handles only a single goal attribute
;;; learning-problem -> hypothesis
(define (decision-tree-learner problem)
  (let* ((tree (make-decision-tree problem)))
    (lambda (input)
      (execute-tree tree input))))

;;; Returns a hypothesis based on a decision stump generated from problem.
;;; Currently only handles a single goal attribute
;;; learning-problem -> hypothesis
(define (decision-stump-learner problem)
  (let* ((stump (make-decision-stump problem)))
    (lambda (input)
      (execute-tree stump input))))

(define (make-decision-tree problem)
  (construct-decision-tree
   (weighted-examples (learning-problem:examples problem)
                      (learning-problem:weights problem))
   (learning-problem:attributes problem)
   (car (learning-problem:goals problem))
   '()))

(define (make-decision-stump problem)
  (let* ((goal (car (learning-problem:goals problem)))
         (new-examples (weighted-examples (learning-problem:examples problem)
                                          (learning-problem:weights problem))))
    (construct-decision-tree
     new-examples
     (list (argmax
            (learning-problem:attributes problem)
            (lambda (a) (importance a new-examples goal)))) goal '())))

;;; Construct a decision tree from the given problem descriptions.
;;; list of ((attr . val) ...) -> list of (attr val ...) -> (attr val ...) ->
;;; list of ((attr . val) ...) -> decision tree
(define (construct-decision-tree examples attributes goal parent-examples)
  (cond ((null? examples) (plurality-value parent-examples goal))
        ((every (lambda (e) (equal? (attribute-value goal e)
                                    (attribute-value goal (car examples))))
		(cdr examples)) (plurality-value examples goal))
        ((null? attributes) (plurality-value examples goal))
        (else (let* ((A (argmax attributes (lambda (a)
                                             (importance a examples goal))))
                     (tree (list A)))
                (fold (lambda (v new-tree)
                        (append new-tree
                                (list (list v
                                            (construct-decision-tree
                                             (filter
                                              (lambda (e)
                                                (equal? (attribute-value A e) v))
                                              examples)
                                             (lset-difference equal? attributes (list A))
                                             goal examples)))))
                      tree (cdr A))))))

;;; Returns a new list of examples, multiplying the appearence of examples in
;;; the original list by their respective weights.
;;; list of (attr . val) -> list of numbers -> list of (attr . val)
(define (weighted-examples examples weights)
  (fold (lambda (e w new-examples)
          ;; normalized-weight may be a misnomer, but whatever.
          (let ((normalized-weight (floor (* w (length weights)))))
            (append (make-list normalized-weight e) new-examples)))
        '() examples weights))


;;; Returns the most common goal attribute among the examples.
;;; list of (attr . val) -> (attr val ...) -> (attr . val)
(define (plurality-value examples goal)
  (cons (attribute-name goal)
        (argmax (cdr goal) (lambda (v)
                             (count (lambda (e)
                                      (equal? (attribute-value goal e) v))
                                    examples)))))

;;; Returns the element in list that produces the largest value when given to
;;; the function f.
;;; list of 'a -> ('a -> number) -> 'a
(define (argmax list f)
  (cdr (fold (lambda (e biggest)
               (let ((size (f e)))
                 (if (> size (car biggest))
                     (cons size e) biggest)))
             (cons (f (car list)) (car list)) list)))

;;; Executes the given decision tree on input. Goal is the output attribute to
;;; use.
;;; decision tree -> list of (attr . val) -> list of (attr . val)
(define (execute-tree tree input)
  (if (not (list? (cdr tree)))
      (list tree)
      (execute-tree (cadr (assoc (attribute-value (car tree) input) (cdr tree)))
                    input)))

(define (gini examples goal)
  (if (null? examples) 0
      (fold (lambda (v difference)
              (- difference
                 (expt (/ (count (lambda (e)
                                   (equal? (attribute-value goal e) v)) examples)
                          (length examples)) 2))) 1 (cdr goal))))

;;; Calculates the GINI gain of using attribute as a node in a decision tree,
;;; given examples, and with goal as the output attribute.
;;; (attr val ...) -> list of (attr . val) -> (attr val ...) -> number
(define (importance attribute examples goal)
  (- (gini examples goal)
     (apply + (map (lambda (v)
                     (let ((exs (filter (lambda (e)
                                          (equal? (attribute-value attribute e) v))
                                        examples)))
                       (* (/ (length exs) (length examples))
                          (gini exs goal))))
                   (cdr attribute)))))
