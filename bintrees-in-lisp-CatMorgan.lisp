;;; =======================================
;;;  CMPU-365, Fall 2014
;;;  Binary Trees in LISP
;;;  Cat Morgan
;;; =======================================

;;;  TESTER
;;; -----------------------------
;;;  INPUT:  expr, an expression
;;;  OUTPUT:  The result of evaluating the expression
;;;  SIDE EFFECT:  Prints out the expression in interactions window

(defun tester 
    (expr)
  ;;prints the side effect described above
  (format t "~S ==> " expr)
  (eval expr))

;;;  PROBLEM
;;; -----------------------------------
;;;  INPUT:  str, a header describing the problem
;;;  OUTPUT:  none
;;;  SIDE EFFECT:  Displays a problem header in the interactions window

(defun problem
    (str)
  ;;prints the side effect that is listed above
  (format t "~%============================================~%")
  (format t " ~S ~%" str)
  (format t "============================================~%~%"))

;;; an empty tree
(defparameter mt-tree '_)

;;;  MT-TREE?
;;; --------------------------------
;;; INPUT: thing, whatever you are checking
;;; OUTPUT: return true if the thing is an empty tree, otherwise NIL

(defun mt-tree? 
    (thing) 
  (eq thing mt-tree))

;;;  The NODE Struct
;;; --------------------------------
;;;  FIELDS:  lefty, a binary tree 
;;;           datum, a piece of data
;;;           righty, a binary tree

(defstruct (node (:print-function print-bin-tree)) lefty datum righty)

;;; ==========================================
;;;  PRINTING OUT A BINARY TREE
;;; ==========================================

;;;  PRINT-N-SPACES
;;; --------------------------------------
;;;  INPUT:  n, a non-negative integer
;;;  OUTPUT:  none
;;;  SIDE EFFECT:  Prints out n spaces

(defun print-n-spaces
    (n)
  (cond
   ;;if there are no more spaces left to print return nil
   ((<= n 0) NIL)
   ;;otherwise, there are still spaces left to print
   (t
    (format t " ")
    ;;recursive call with one less space left
    (print-n-spaces (- n 1)))))

;;;  PRINT-BIN-TREE-HELPER
;;; --------------------------------------
;;;  INPUTS:  tree, a binary tree
;;;           indent, the indentation amount for printing the tree
;;;  OUTPUT:  None
;;;  SIDE EFFECT:  Displays the contents of the binary tree in
;;;   the Interactions Window, indented by the indicated number of spaces.

(defun print-bin-tree-helper
    (tree indent)
  (cond  
   ;; Base Case:  tree is empty
   ((mt-tree? tree)
    ;; print spaces for depth of tree
    (print-n-spaces indent)
    ;; print vertical bar for node
    (format t "|~%"))
   
   ;; Recursive Case:  the tree is at a node
   (t
    ;; Print the right sub-tree 
    (print-bin-tree-helper 
      (node-righty tree)
			   (+ indent 10))
    ;; Print the head
    (print-n-spaces indent)
    (format t "[~S]~%" (node-datum tree))
    ;; Print the left sub-tree
    (print-bin-tree-helper 
      (node-lefty tree)
			   (+ indent 10)))))

;;;  PRINT-BIN-TREE
;;; ----------------------------------------------
;;;  INPUT:  tree, a binary tree
;;;  OUTPUT:  None
;;;  SIDE EFFECT:  Displays the binary tree (wrapper)

(defun print-bin-tree
  (tree)
  (format t "~%")
  (print-bin-tree-helper tree 0))

;;;  define trees
(defparameter tree1 (make-node :lefty mt-tree :datum 33 :righty mt-tree))
(defparameter tree2 (make-node :lefty mt-tree :datum 14 :righty mt-tree))
(defparameter tree3 (make-node :lefty tree1 :datum 99 :righty tree2))
(tester '(print-bin-tree tree3))

(defparameter tree10 (make-node :lefty mt-tree :datum 24 :righty mt-tree))
(defparameter tree11 (make-node :lefty mt-tree :datum 67 :righty mt-tree))
(defparameter tree12 (make-node :lefty tree10 :datum 109 :righty tree11))
(defparameter big-tree (make-node :lefty tree3 :datum 9000 :righty tree12))
(tester '(print-bin-tree big-tree))
