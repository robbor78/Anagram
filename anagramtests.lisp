(defparameter *tree* (make-instance 'node))

(add-word *tree* "cat")
(add-word *tree* "act")
(add-word *tree* "tac")
(add-word *tree* "at")
(add-sentence *tree* "rail safety")
(add-sentence *tree* "fairy tales")

(print-tree *tree*)

;(slot-value (gethash '#\t (slot-value (gethash '#\c (slot-value (gethash '#\a (slot-value *tree* 'nodes)) 'nodes)) 'nodes)) 'words)

;;(fromfile *tree* "./words.txt")

(find-anagram *tree*  "act")
(find-anagram *tree*  "at")

(find-sentence *tree* "rail safety")

(find-sentence2 *tree* "rail safety")

(my-split "hello world")
(my-split "rail safety")
