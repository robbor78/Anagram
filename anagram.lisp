(in-package :anagram)

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
       ;; (format t "curr= ~a ~a~%" curr (slot-value curr 'nodes))
         (with-slots (nodes) curr
           (multiple-value-bind (value present) (gethash char nodes)
             value ;;avoid compiler warning
             (when (not present)
               (setf (gethash char nodes) (make-instance 'node))))
           (setf curr (gethash char nodes))
           ;; (format t "tree= ~a ~a~%" tree nodes)
           ;; (format t "curr= ~a ~a~%" curr (slot-value curr 'nodes))
           ))
    (with-slots (words) curr
      (initwords curr)
                                        ;(format t "Adding word ~a to tree.~%" word)
      (vector-push-extend word words))
    ))

(defun add-sentence (tree sentence)
  (loop for word in (my-split sentence) do
     ;; (format t "adding ~a~%" word)
       (add-word tree word)))

(defun print-tree (tree)
  (with-slots (nodes words) tree
                                        ;(format t "tree= ~a~%" tree)
                                        ;(format t "chars=~a~%" (hash-keys nodes))
    (when (> (length words) 0)
      (format t " (~a) " words))
    (cond ((null nodes)
           (format t " (~a)~%" words))
          (t
                                        ;(format t "chars=~a~%" (hash-keys nodes))
           (loop for char being the hash-keys in nodes using (hash-value subnode) do
                (format t "~a" char)
                (print-tree subnode))))))

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
           (add-word tree word)
           )))
  ;; (format t "~%")))
  (format t "done~%"))

(defun make-occurence (input)
  (let ((h (make-hash-table)))
    (loop for char across (nstring-upcase input) do
         (when (not (delimiterp char))
           (when (null (gethash char h))
             (setf (gethash char h) 0 ))
           (incf (gethash char h))))
    h))

(defun all-zerop (o)
  (loop for count being the hash-values in o always (= count 0)))

(defun exhaustedp (occurence)
  (all-zerop occurence))

(defun has-wordsp (node)
  (> (length (slot-value node 'words)) 0))

(defun add-words (partial solution)
  (vector-push-extend partial solution))

(defun add-words-from-node (node store)
  (add-words (slot-value node 'words) store))

(defun remove-words (store)
  (vector-pop store))

(defun find-sentence2 (tree sentence)
  (let ((o (make-occurence sentence)))
    (defun find-sentence2-local (curr)
                                        ;(print-occurence o)
      (when (all-zerop o)
        (format t "exhausted~%")
        (when (> (length (slot-value curr 'words)) 0)
          (format t "matched~%"))
        (return-from find-sentence2-local nil))
      (with-slots (nodes) curr
        (when (not (null nodes))
          (format t "chars=~a~%" (hash-keys nodes))
          (loop for char being the hash-keys in nodes using (hash-value subnode) do
               (format t "char=~a~%" char)
               (multiple-value-bind (value present) (gethash char o)
                 (format t "present=~a value=~a~%" present value)
                 (when (and present (> value  0))
                   (decf (gethash char o))
                   (with-slots (words) curr
                     (when (> (length words) 0)
                       (format t "going back to tree ~a~%" words)
                       (find-sentence2-local tree)))
                   (format t "preceding~%")
                   (find-sentence2-local subnode)
                   (incf (gethash char o)))))))
      (with-slots (words) curr
        (when (> (length words) 0)
          (format t "Words final= ~a~%" words)
          (find-sentence2-local tree))))
    (format t "starting ...~%")
    (print-occurence o)
    (find-sentence2-local tree)
    (format t "done")))

(defun find-sentence3 (tree-root sentence)
  ;;local variables
  (let (
        ;;partial solution
        (partial (make-array 0 :fill-pointer 0 :adjustable t))
        ;;solutions
        (solutions (make-array 0 :fill-pointer 0 :adjustable t))
        ;;input with occurences (make occurences)
        (o (make-occurence sentence))
        ;;stack to store last search char from tree-root
        (tree-root-chars ()))

    ;;local function

    (defun peek-last-tree-root-char ()
      (car tree-root-chars))

    (defun push-char-if-at-tree-root (current-node char)
      (when (equal current-node tree-root)
        (setf tree-root-chars (cons char tree-root-chars))))

    (defun pop-char-if-at-tree-root (current-node)
      (when (equal current-node tree-root)
        (setf tree-root-chars (cdr tree-root-chars))))

    (defun ok-to-process-charp (current-node char start-from-char ok-to-process-char)
      (cond
        ((not (equal current-node tree-root)) 2)
        ((= ok-to-process-char 2) ok-to-process-char)
        ((= ok-to-process-char 1) 2)
        ((not start-from-char) 2)
        ((= char start-from-char) 1)))

    (defun find-sentence3-local (current-node &key start-from-char)

      ;;exhausted?
      (when (exhaustedp o)
        ;;yes

        (format t "exhausted")

    ;;;words at current node?
        (when (has-wordsp current-node)
    ;;;yes
    ;;;;add words at current node to partial solution and add partial solution to solutions
          (add-words partial solutions)
          (add-words-from-node current-node solutions))
        ;;return
        (return-from find-sentence3-local))

      ;;not exchausted

      ;;words at current node? (either at leaf or node)
      (when (has-wordsp current-node)
        ;;yes
    ;;;1. add words at current node to partial solution
        (add-words-from-node current-node partial)
    ;;;2. recurse from tree-root AND indicate which nodes not to search, i.e. call find-sentence3-local with tree-root
        (find-sentence3-local tree-root :start-from-char (peek-last-tree-root-char))
    ;;;3. remove words at current node from partial solution
        (remove-words partial)
        )

      ;;loop through all chars at current node
      (with-slots (nodes) current-node
        (when (not (null nodes))
          (let ((ok-to-process-char 0))
            (loop for char being the hash-keys in nodes using (hash-value char-node) do

                 (format t "checking is ok to process...~%")

                 (when (= 2
                          (setf ok-to-process-char (ok-to-process-charp
                                                    current-node
                                                    char
                                                    start-from-char
                                                    ok-to-process-char)))

;;;is char in occurence and count > 0?
                   (multiple-value-bind (value present) (gethash char o)
                     (when (and present (> value 0))
    ;;;yes
    ;;;;decrement count
                       (decf (gethash char o))
    ;;;;if at tree-root push char
                       (push-char-if-at-tree-root current-node char)

                       (format t "char=~a~%" char)

    ;;;;recurse to char-node, i.e. call find-sentence3-local with char-node
                       (find-sentence3-local char-node)

    ;;;;if at tree-root pop char
                       (pop-char-if-at-tree-root current-node)
    ;;;;increment count
                       (incf (gethash char o))))))))))


    (format t "start~%")

    ;;start a tree-root
    (find-sentence3-local tree-root)

    (format t "end~%")

    ;;return solutions
    solutions
    ))

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

(defun hash-values (hash-table)
  (loop for value being the hash-values of hash-table collect value))

(defun print-occurence (o)
  (format t "o=~a~%" (hash-keys o))
  (format t "o=~a~%" (hash-values o))  )
