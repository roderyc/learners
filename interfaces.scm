(define-interface adaboost-interface
  (export adaboost))

(define-interface decision-tree-interface
  (export decision-tree-learner
          decision-stump-learner
          make-decision-tree
          make-decision-stump
          execute-tree))

(define-interface learning-problem-interface
  (export make-learning-problem
          learning-problem?
          learning-problem:attributes
          learning-problem:examples
          learning-problem:weights
          learning-problem:goals
          copy-learning-problem/weights
          make-learning-problem/default-weights
          attribute-name
          attribute-domain
          attribute-value
          classify
          consistent))

(define-interface problems-interface
  (export *restaurant-problem*))

(define-interface high-learners-interface
  (compound-interface
   adaboost-interface
   decision-tree-interface
   learning-problem-interface
   problems-interface))
