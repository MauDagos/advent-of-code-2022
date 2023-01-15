(in-package :cl-user)


(defun read-integer (stream)
  (let ((int-str
          (with-output-to-string (out)
            (loop
              ;; NOTE: we're not peeking here, we're always reading. That means
              ;; we'll consume '-' and ','.
              for char = (read-char stream nil)
              while (and char
                         (member char (load-time-value
                                       (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                                 :test 'eq))
              do (write-char char out)))))
    (parse-integer int-str)))


(defun read-range (stream)
  (let ((range-start (read-integer stream))
        (range-end   (read-integer stream)))
    (loop
      for section from range-start upto range-end
      collect section)))


(defun read-ranges (stream)
  (values (read-range stream)
          (read-range stream)))


(defun ranges-fully-contained-p (range-1 range-2)
  (or (subsetp range-1 range-2 :test '=)
      (subsetp range-2 range-1 :test '=)))


(defun ranges-overlap-p (range-1 range-2)
  (intersection range-1 range-2 :test '=))


(defun day-4 (&optional (file #p"day4/example-input.txt"))
  (with-open-file (stream file :direction :input)
    (loop
      while (peek-char nil stream nil)
      for range-1 = nil
      for range-2 = nil
      do (multiple-value-setq (range-1 range-2) (read-ranges stream))
      when (ranges-fully-contained-p range-1 range-2)
        sum 1 into fully-contained
      when (ranges-overlap-p range-1 range-2)
        sum 1 into overlaps
      finally (format t "Assignment pairs with one range fully containing another: ~d"
                      fully-contained)
              (format t "~2&Assignment pairs with overlaps: ~d"
                      overlaps))))
