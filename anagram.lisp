(defclass node ()
  ((nodes
    :initform () ;;make-hash-table)
    :accessor nodes)
   (words
    :initform () ;;(make-array 0 :fill-pointer 0 :adjustable t)
    :accessor words)))



(defmethod initnodes ((obj node))
  (with-slots (nodes) obj
    (when (null nodes)
      (setf nodes (make-hash-table)))))

(defmethod initwords ((obj node))
  (with-slots (words) obj
    (when (null words)
      (setf words (make-array 0 :fill-pointer 0 :adjustable t)))))


(defun add-word (tree word)
  ;(format t "Adding ~a to tree.~%" word)
  (let ((curr tree))
    (loop for char across (nstring-upcase (sort (copy-seq word) #'char-lessp)) do
         (initnodes curr)
         (with-slots (nodes) curr
           (when (null (gethash char nodes))
             (setf (gethash char nodes) (make-instance 'node)))
           (setf curr (gethash char nodes))))
    (with-slots (words) curr
      (initwords curr)
      ;(format t "Adding word ~a to tree.~%" word)
      (vector-push-extend word words))
    ))

(defun add-sentence (tree sentence)
  (loop for word in (my-split sentence) do
       (format t "adding ~a~%" word)
       (addword tree word)))

(defun print-tree (tree)
  (with-slots (nodes words) tree
    (when (> (length words) 0)
      (format t " (~a) " words))
    (cond ((null nodes)
           (format t " (~a)~%" words))
          (t
           (loop for char being the hash-keys in nodes using (hash-value subnode) do
                (format t "~a" char)
                (printtree subnode))))))

(defun find-anagram (tree word)
   (let ((curr tree))
    (loop for char across (nstring-upcase (sort (copy-seq word) #'char-lessp)) do
         (with-slots (nodes) curr
           (when (null (gethash char nodes))
             (return-from find-anagram nil))
           (setf curr (gethash char nodes))))
    (with-slots (words) curr
      words)))

(defun _find-sentence (tree seq)
  (let ((curr tree))
    (loop
       for char across seq
       for i from 0 to (1- (length seq))
       do
         (format t "~a ~a~%" i char)
         (with-slots (nodes) curr
           (when (null (gethash char nodes))
             (return-from _find-sentence nil))

           (setf curr (gethash char nodes))
           (with-slots (words) curr
             (when (> (length words) 0)
               (format t "valid branch ~a~%" words)
               (_find-sentence tree (subseq seq (1+ i)))))
           ))
    ;;make copy of words array and return the copy
    (with-slots (words) curr
      (format t "valid subseq ~a~%" words)
      words)))

;;PROBLEM: need to consider character occurences!
(defun find-sentence (tree sentence)
  (let ((sorted (string-upcase (string-trim " " (sort (copy-seq sentence) #'char-lessp)))))
    (format t "~a -> ~a~%" sentence sorted)
    (_find-sentence tree sorted)))

(defun from-file (tree filename)
  (with-open-file (in filename :if-does-not-exist nil)
    (when in
      (loop for word = (read-line in nil) while word do
                                        ;(format t "~a~%" word)
           ;; (format t ".")
           (addword tree word)
           )))
    ;; (format t "~%")))
  (format t "done~%"))

;;from stackoverflow!
(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun delimiterp (c) (or (char= c #\Space) (char= c #\,)))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))
