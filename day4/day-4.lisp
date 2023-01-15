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
  (values (read-integer stream)
          (read-integer stream)))


(defun ranges-fully-contained-p (range-1-start range-1-end
                                 range-2-start range-2-end)
  (flet ((%fully-contained-p (start1 end1 start2 end2)
           (and (<= start1 start2)
                (>=   end1   end2))))
    (or (%fully-contained-p range-1-start range-1-end
                            range-2-start range-2-end)
        (%fully-contained-p range-2-start range-2-end
                            range-1-start range-1-end))))


(defun ranges-overlap-p (range-1-start range-1-end
                         range-2-start range-2-end)
  (and (<= range-1-start range-2-end)
       (>= range-1-end   range-2-start)))


(defun day-4 (&optional (file #p"day4/example-input.txt"))
  (with-open-file (stream file :direction :input)
    (loop
      while (peek-char nil stream nil)
      for range-1-start = nil
      for range-1-end   = nil
      for range-2-start = nil
      for range-2-end   = nil
      do (multiple-value-setq (range-1-start range-1-end) (read-range stream))
         (multiple-value-setq (range-2-start range-2-end) (read-range stream))
      ;; Part 1
      when (ranges-fully-contained-p range-1-start range-1-end
                                     range-2-start range-2-end)
        sum 1 into fully-contained
      ;; Part 2
      when (ranges-overlap-p range-1-start range-1-end
                             range-2-start range-2-end)
        sum 1 into overlaps
      finally (format t "Assignment pairs with one range fully containing another: ~d"
                      fully-contained)
              (format t "~2&Assignment pairs with overlaps: ~d"
                      overlaps))))
