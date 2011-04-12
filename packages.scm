(define-structure learning-problem learning-problem-interface
  (open scheme
        (subset srfi-1 (every))
        (subset srfi-9 (define-record-type)))
  (files learning-problem))

(define-structure problems problems-interface
  (open scheme
        (subset learning-problem (make-learning-problem/default-weights)))
  (files (problems restaurant)))

(define-structure adaboost adaboost-interface
  (open scheme
        (subset learning-problem (consistent
                                  copy-learning-problem/weights
                                  learning-problem:weights
                                  learning-problem:goals
                                  learning-problem:examples))
        (subset srfi-1 (first second third fold)))
  (files adaboost))

(define-structure decision-tree decision-tree-interface
  (open scheme
        (subset learning-problem (learning-problem:goals
                                  learning-problem:examples
                                  learning-problem:attributes
                                  learning-problem:weights
                                  attribute-name
                                  attribute-value))
        (subset srfi-1 (filter every count lset-difference fold make-list)))
  (files decision-tree))

(define-structure high-learners high-learners-interface
  (open learning-problem problems adaboost decision-tree))
