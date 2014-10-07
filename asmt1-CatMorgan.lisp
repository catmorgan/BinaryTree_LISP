;;; =======================================================
;;;  CMPU-365, Fall 2014
;;;  Solutions to Asmt 1 -- in Lisp
;;;  Cat Morgan
;;; =======================================================

(format t "==========================~%")
(format t "  CMPU-365, Fall 2014~%")
(format t "  Asmt. 1~%")
(format t "  Solutions in Lisp!~%")
(format t "==========================~%~%")

(load "bintrees-in-lisp-CatMorgan.lisp")


(format t "The sample bin-trees from 'bintrees-in-lisp-CatMorgan.lisp'...~%~%")

;;; ------------
;;;  (1)
;;; ------------

(problem "1:  INSERT-NUMS")

;;;  INSERT
;;; -----------------------------------------
;;;  INPUTS:  num, a number
;;;           tree, a binary tree
;;;  OUTPUT:  A tree with num as a new node

(defun insert 
  (num tree) 
    (cond
    ;;Base Case: tree is empty
    ((mt-tree? tree) 
        (make-node :lefty mt-tree :datum num :righty mt-tree))
      ;; Base Case 2:  num = datum 
      ((= num (node-datum tree))
       ;; Return tree unchanged...
       tree)

       ;; Recursive Case 1:  num < datum at root
      ((< num (node-datum tree))
       ;; Return a tree with num inserted into left-hand sub-tree
       (make-node :lefty (insert num (node-lefty tree))
                  :datum (node-datum tree)
                 :righty (node-righty tree)))
      
      ;; Recursive Case 2:  num > datum at root
      (t
       ;; Return a tree with num inserted into right-hand sub-tree
       (make-node :lefty (node-lefty tree)
                  :datum (node-datum tree)
                 :righty (insert num (node-righty tree))))))

        

;;;  INSERT-NUMS
;;; =======================================
;;;  INPUTS:  nums, a list of numbers
;;;           tree, a binary tree
;;;  OUTPUT:  A tree with all numbers in nums inserted into the tree

(defun insert-nums
      (nums tree)
    (cond
      ;; Base Case: nums is empty
      ((equal NIL nums)
       ;; Return tree
       tree)
      
      ;; Recursive Case: 
      (t
       ;; Insert the rest of the nums into the tree after adding 
       ;;in the first num
       (insert-nums (rest nums) 
                    (insert 
                      (first nums) 
                      tree)))))

(tester '(print-bin-tree (insert-nums '(10 2 8 7 32 100 64)
                                      mt-tree)))

(tester '(print-bin-tree (insert-nums
                          '(100 99 1 10 20 7 42 99)
                          mt-tree)))


;;;  CREATE-BIN-TREE
;;; =======================================
;;;  INPUTS:  nums, a list of numbers
;;;  OUTPUT:  A tree with all numbers in nums inserted into the tree (a wrapper)


(defun create-bin-tree
   (nums)
    (insert-nums nums mt-tree))

(tester '(print-bin-tree (create-bin-tree '(1 2 6 7 8))))
(tester '(print-bin-tree (create-bin-tree ())))
(tester '(print-bin-tree (create-bin-tree '(9 8 5 4 21))))
(tester '(print-bin-tree (create-bin-tree '(321 213 132))))
(tester '(print-bin-tree (create-bin-tree '(9 7 8 3 5))))

;;; -----------
;;;  (2)
;;; -----------

(problem "2:  DF listing")

;;;  GEN-DEPTH-FIRST-LISTING-ACC
;;; ======================================================
;;;  INPUTS:  tree, a binary tree
;;;           acc, a list-based accumulator
;;;  OUTPUT:  a list of nums representing the nodes in a tree, starting with
;;            depth first 

(defun gen-depth-first-listing-acc
  (tree acc)
    (cond
      ;; Base Case:  tree is empty, return accumulator
      ((mt-tree? tree)
       acc)
      
      ;; Recursive Case:  tree is not empty, so take right side of tree 
      ;; and left side of tree and cons the head to the accumulator
      (t
       (gen-depth-first-listing-acc
        (node-righty tree)
        (gen-depth-first-listing-acc  
          (node-lefty tree)
          (cons (node-datum tree) acc)))
       )))

;;;  GEN-DEPTH-FIRST-LISTING
;;; ----------------------------------------------------------
;;;  INPUT:  tree, a binary tree
;;;  OUTPUT:  A list of the nums in tree, representing the nodes in a tree, starting
;;            with depth first (wrapper)

(defun gen-depth-first-listing
  (tree)
    (reverse (gen-depth-first-listing-acc tree ())))

(tester '(gen-depth-first-listing mt-tree))
(tester '(gen-depth-first-listing (create-bin-tree '(6 7 8 9))))
(tester '(gen-depth-first-listing (create-bin-tree '(100 90 80 70))))
(tester '(gen-depth-first-listing (create-bin-tree '(0 3 5 1 9 8 7))))
(tester '(gen-depth-first-listing big-tree))
