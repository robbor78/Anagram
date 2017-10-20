(load "anagram.lisp")

(defparameter *tree* (make-instance 'node))

(fromfile *tree* "./words.txt")

