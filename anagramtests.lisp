(in-package :anagram)

(defparameter *tree* (make-instance 'node))


(add-word *tree* "cat")
(add-word *tree* "act")
(add-word *tree* "tac")
(add-word *tree* "at")
(add-sentence *tree* "rail safety")
(add-sentence *tree* "fairy tales")

(add-sentence *tree* "NBAE")
(add-sentence *tree* "NBAEF")
(add-sentence *tree* "NBD")
(add-sentence *tree* "NF")
(add-sentence *tree* "NQ")


(print-tree *tree*)

;(slot-value (gethash '#\t (slot-value (gethash '#\c (slot-value (gethash '#\a (slot-value *tree* 'nodes)) 'nodes)) 'nodes)) 'words)

;;(fromfile *tree* "./words.txt")

(find-anagram *tree*  "act")
(find-anagram *tree*  "at")

(find-sentence *tree* "rail safety")

(find-sentence2 *tree* "rail safety")

(find-sentence2 *tree* "nnffbae")


(my-split "hello world")
(my-split "rail safety")

(unittests::test-arithmetic)

;;(equal '((("ad" "da") ("bcef")) (("abc" "cba") '("def"))) '((("abc" "cba") '("def"))))

(unittests::deftest test1 ()
  (let ((tree (make-instance 'node)))

    (add-word tree "abc")
    (add-word tree "cba")

    (let ((result (find-sentence3 tree "bac")))

      (format t "~a~%" result)
      (unittests::check (equal result '((("abc" "cba")))))



    )))

(unittests::deftest test2 ()
  (let ((tree (make-instance 'node)))

    (add-sentence tree "rail safety")
    (add-sentence tree "fairy tale")

    (let ((result (find-sentence3 tree "rail safety")))

      (format t "~a~%" result)
      (unittests::check (equal result '((("abc" "cba")))))



    )))


