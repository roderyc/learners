;;; Uses (subset srfi-1 (assoc every))
;;;      (subset srfi-9 (define-record-type))

;;; A type for learning problems, and utilities for manipulating example
;;; data. Examples are lists of (attribute . value) pairs. If an example has
;;; been classified, some of those pairs will be goals. Goals are the output
;;; attributes that examples havee been classified into.
(define-record-type :learning-problem
    (make-learning-problem attributes examples goals)
    learning-problem?
    (attributes learning-problem:attributes)
    (examples learning-problem:examples)
    (weights learning-problem:weights)
    (goals learning-problem:goals))

(define (make-learning-problem/weights problem new-weights)
  (make-learning-problem
   (learning-problem:attributes problem)
   (learning-problem:examples problem)
   new-weights
   (learning-problem:goals problem)))

;; (defun print-learning-problem (problem &optional stream depth)
;;   (declare (ignore depth))
;;   (format stream "#<~A with ~D examples and ~D attributes>"
;; 	  (type-of problem)
;; 	  (length (learning-problem-examples problem))
;; 	  (length (learning-problem-attributes problem))))

;;; Returns the name of a given attribute.
(define (attribute-name attribute) (car attribute))

;;; Returns the domain of a given attribute.
(define (attribute-domain attribute) (cdr attribute))

;;; Returns the value of the given attribute within the given example.
(define (attribute-value attribute example)
  (cdr (assoc (attribute-name attribute) example)))

;;; Takes a list of unclassified examples, runs the given hypotheses on them and
;;; returns those examples in a list, in their classified form.
(define (classify unclassified-examples h)
  (map (lambda (e)
         (append (h e) e))
       unclassified-examples))

;;; Determines if all the given classified examples are consistent with the
;;; classifications returned by h.
(define (consistent examples h)
  (every (lambda (e)
           (every (lambda (goal)
                    (equal? (attribute-value (car goal) e) (cdr goal)))
                  (h e)))
         examples))
