(define-interface adaboost-interface
  (export adaboost))

(define-interface decision-tree-interface
  (export decision-tree-learner
          decision-stump-learner
          construct-decision-tree))

(define-interface learning-problem-interface
  (export make-learning-problem
          learning-problem?
          learning-problem:attributes
          learning-problem:examples
          learning-problem:weights
          learning-problem:goals
          make-learning-problem/weights
          attribute-name
          attribute-domain
          attribute-value
          classify
          consistent))

(define-interface high-learners-interface
  (compound-interface
   adaboost-interface
   decision-tree-interface
   learning-problem-interface))
