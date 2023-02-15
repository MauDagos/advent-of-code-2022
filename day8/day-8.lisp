(in-package :cl-user)


(defun parse-trees (file)
  (with-open-file (stream file :direction :input)
    (loop
      with rows = nil
      with current-row = nil
      for char = (read-char stream nil)
      while char
      if (eq char #\NewLine)
        do (push (nreverse current-row) rows)
           (setq current-row nil)
      else
        do (push (digit-char-p char) current-row)
      finally
         (when current-row
           (push (nreverse current-row) rows))
         (return
           (coerce (mapcar (lambda (row) (coerce row 'simple-vector))
                           (nreverse rows))
                   'simple-vector)))))


(defun trees-row-count (trees)
  (length trees))


(defun trees-column-count (trees)
  (length (svref trees 0)))


(defun get-tree (trees row-idx column-idx)
  (svref (svref trees row-idx) column-idx))


(defun tree-visible-p (trees row-idx column-idx)
  (let ((row-count (trees-row-count trees))
        (column-count (trees-column-count trees))
        (this-tree (get-tree trees row-idx column-idx)))
    (or
     ;; This tree is visible from the top.
     (loop
       for other-row-idx from (1- row-idx) downto 0
       always (> this-tree (get-tree trees other-row-idx column-idx)))
     ;; This tree is visible from the bottom.
     (loop
       for other-row-idx from (1+ row-idx) below row-count
       always (> this-tree (get-tree trees other-row-idx column-idx)))
     ;; This tree is visible from the left.
     (loop
       for other-column-idx from (1- column-idx) downto 0
       always (> this-tree (get-tree trees row-idx other-column-idx)))
     ;; This tree is visible from the right.
     (loop
       for other-column-idx from (1+ column-idx) below column-count
       always (> this-tree (get-tree trees row-idx other-column-idx))))))


(defun count-visible-trees (trees)
  (let* ((row-count (trees-row-count trees))
         (column-count (trees-column-count trees))
         (edges-count (cond
                        ;; Consider a 1xm matrix.
                        ((= 1 row-count) column-count)
                        ;; Consider a nx1 matrix.
                        ((= 1 column-count) row-count)
                        ;; For every row there are two visible on edge columns.
                        ;; Then add the remaining visible from the first and
                        ;; last rows.
                        (t (+ (* row-count 2)
                              (* 2 (- column-count 2)))))))
    (loop
      for row-idx from 1 below (1- row-count)
      do (loop
           for column-idx from 1 below (1- column-count)
           when (tree-visible-p trees row-idx column-idx)
             do (incf edges-count)))
    edges-count))


(defun scenic-score (trees row-idx column-idx)
  (let ((row-count (trees-row-count trees))
        (column-count (trees-column-count trees))
        (this-tree (get-tree trees row-idx column-idx)))
    (*
     ;; Count of trees visible from the top.
     (loop
       with count = 0
       for other-row-idx from (1- row-idx) downto 0
       do (incf count)
       while (> this-tree (get-tree trees other-row-idx column-idx))
       finally (return count))
     ;; Count of trees visible from the bottom.
     (loop
       with count = 0
       for other-row-idx from (1+ row-idx) below row-count
       do (incf count)
       while (> this-tree (get-tree trees other-row-idx column-idx))
       finally (return count))
     ;; Count of trees visible from the left.
     (loop
       with count = 0
       for other-column-idx from (1- column-idx) downto 0
       do (incf count)
       while (> this-tree (get-tree trees row-idx other-column-idx))
       finally (return count))
     ;; Count of trees visible from the right.
     (loop
       with count = 0
       for other-column-idx from (1+ column-idx) below column-count
       do (incf count)
       while (> this-tree (get-tree trees row-idx other-column-idx))
       finally (return count)))))


(defun highest-scenic-score (trees)
  (loop
    with row-count = (trees-row-count trees)
    with column-count = (trees-column-count trees)
    for row-idx from 0 below row-count
    maximize (loop
               for column-idx from 0 below column-count
               maximize (scenic-score trees row-idx column-idx))))


(defun day-8 (&optional (file #p"day8/example-input.txt"))
  (let ((trees (parse-trees file)))
    (format t "[Part 1] Amount of visible trees: ~d~%~
               [Part 2] Highest scenic score: ~d"
            (count-visible-trees trees)
            (highest-scenic-score trees))))
