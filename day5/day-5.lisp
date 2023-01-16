(in-package :cl-user)


(defun skip-chars (stream count)
  (loop
    repeat count
    do (read-char stream)))


(defun read-integer (stream)
  (let ((int-str
          (with-output-to-string (out)
            (loop
              for char = (read-char stream nil)
              while (and char
                         (member char (load-time-value
                                       (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                                 :test 'eq))
              do (write-char char out)))))
    (parse-integer int-str)))


(defun make-crates-level (stream)
  ;; NOTE: the first character of the line has already been consumed (i.e. #\[
  ;; or #\Space).
  ;;
  ;; 0 1 2 3 4 5 6 7 8 9 10
  ;;         [ D ]
  ;; [ N ]   [ C ]
  ;; [ Z ]   [ M ]   [ P ]
  (loop
    for read-count from 0               ; read during this loop, that is
    for char = (read-char stream)
    until (eq char #\NewLine)
    ;; Every 4 chars we read, we should find the crate label
    when (zerop (mod read-count 4))
      collect char))


(defun make-crates-matrix (stream)
  (loop
    for char = (read-char stream)
    while (or (eq char #\[)
              (and (eq char #\Space)
                   (eq (peek-char nil stream) #\Space)))
    collect (make-crates-level stream)))


(defun make-crates-map (stream)
  (let* ((map (make-hash-table :test 'eq))
         (matrix (make-crates-matrix stream))
         (matrix-inverse (nreverse matrix)))
    (loop
      for level in matrix-inverse
      do (loop
           for crate in level
           for pillar from 1
           unless (eq crate #\Space)
             do (push crate (gethash pillar map))))
    map))


;;; Part 1 of the exercise
#+crate-mover-9000
(defun move-crates (crates-map amount origin destination)
  (loop
    repeat amount
    for crate = (pop (gethash origin crates-map))
    do (push crate (gethash destination crates-map))))


;;; Part 2 of the exercise
#-crate-mover-9000
(defun move-crates (crates-map amount origin destination)
  (loop
    with crates-origin = (gethash origin crates-map)
    with new-pillar = crates-origin
    repeat (1- amount)
    for (nil . rest) on crates-origin
    finally
       (unless rest (setq rest crates-origin))
       ;; 1. Remove the link from the origin pillar
       ;; 2. Update the new pillar's link to the destination pillar
       ;; 3. Overwrite the destination pillar
       (setf (gethash origin crates-map) (cdr rest)
             (cdr rest) (gethash destination crates-map)
             (gethash destination crates-map) new-pillar)))


(defun execute-next-step (stream crates-map)
  (let ((amount      (progn (skip-chars stream #.(length "move "))
                            (read-integer stream)))
        (origin      (progn (skip-chars stream #.(length "from "))
                            (read-integer stream)))
        (destination (progn (skip-chars stream #.(length "to "))
                            (read-integer stream))))
    (move-crates crates-map amount origin destination)))


(defun day-5 (&optional (file #p"day5/example-input.txt"))
  (with-open-file (stream file :direction :input)
    (let ((crates-map (make-crates-map stream)))
      (read-line stream)                ; Skip line of pillar numbers
      (read-line stream)                ; Skip empty line
      (loop
        while (peek-char nil stream nil)
        do (execute-next-step stream crates-map))
      (let ((top-crates (loop
                          for pillar from 1 upto (hash-table-count crates-map)
                          collect (car (gethash pillar crates-map)))))
        (format t "The top crates are: ~{~a~^~}"
                top-crates)))))
