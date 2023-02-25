(in-package :cl-user)


;;; Parsing packets

(defun read-integer (stream)
  (loop
    with out = nil
    for char = (peek-char nil stream)
    while (member char (load-time-value
                        (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                  :test 'eq)
    do (unless out
         (setq out (make-string-output-stream)))
       (write-char (read-char stream) out)
    finally (return (when out
                      (parse-integer (get-output-stream-string out))))))


(defun parse-packet-data (stream)
  ;; The starting '[' has already been consumed
  (loop
    with data = '()
    for char = (peek-char nil stream)
    do (case char
         (#\,
          (read-char stream))
         (#\[
          (read-char stream)
          (push (parse-packet-data stream) data))
         (#\]
          (read-char stream)
          (return (nreverse data)))
         (t
          (push (read-integer stream) data)))))


;;; Value ordering

(defun call-xor (&rest condition-fns)
  (loop
    with true-found-p = nil
    for condition-fn in condition-fns
    when (funcall condition-fn)
      do (if true-found-p
             (return nil)
             (setq true-found-p t))
    finally (return true-found-p)))


(defmacro xor (&rest conditions)
  `(call-xor ,@(mapcar (lambda (condition)
                         `(lambda () ,condition))
                       conditions)))


(defun %packet< (packet1 packet2)
  "Return two values:
1) Does PACKET1 come before PACKET2.
2) Should we continue comparing the packets."
  (cond
    ;; Both packets are empty. Consider PACKET1 less, but continue checking.
    ((not (or packet1 packet2))
     (values t t))
    ;; PACKET1 is empty (and PACKET2 isn't). PACKET1 comes before.
    ((null packet1)
     (values t nil))
    ;; PACKET2 is empty (and PACKET1 isn't). PACKET1 does not come before.
    ((null packet2)
     (values nil nil))
    ;; Both packets are non-empty. Compare each value in the same position.
    (t
     (loop
       for rest1 = packet1 then (cdr rest1)
       for rest2 = packet2 then (cdr rest2)
       while (and rest1 rest2)
       for value1 = (car rest1)
       for value2 = (car rest2)
       do
          ;; Exactly one of the values is an integer. Consider it a list with
          ;; just that value.
          (when (xor (integerp value1) (integerp value2))
            (cond
              ((integerp value1) (setq value1 (list value1)))
              ((integerp value2) (setq value2 (list value2)))))
          (cond
            ;; Both are integers. If they're the same, then continue checking.
            ((and (integerp value1) (integerp value2))
             (cond
               ((< value1 value2) (return (values t nil)))
               ((> value1 value2) (return (values nil nil)))))
            ;; Both are lists. Check them as individual packets.
            ((and (listp value1) (listp value2))
             (multiple-value-bind (lessp continuep)
                 (%packet< value1 value2)
               (unless continuep
                 (return (values lessp nil))))))
       finally
          ;; We've reached the end of either of the packets.
          (return (cond
                    ;; We reached the end of both packets. Consider PACKET1
                    ;; less, but continue checking.
                    ((not (or rest1 rest2))
                     (values t t))
                    ;; PACKET2 had more values. PACKET1 comes before.
                    (rest2
                     (values t nil))
                    ;; PACKET1 had more values. PACKET1 does not come before.
                    (t
                     (values nil nil))))))))


(defun packet< (packet &rest more-packets)
  (loop
    for packet1 = packet then packet2
    for packet2 in more-packets
    always (%packet< packet1 packet2)))


;;; Part 2

(defun decoder-key (all-packets)
  (let* ((divider-packets (list '((2))
                                '((6))))
         (ordered (sort (append divider-packets all-packets) 'packet<)))
    (loop
      with key = 1
      for divider in divider-packets
      do (setq key (* key (1+ (position divider ordered :test 'eq))))
      finally (return key))))


;;; Entrypoint

(defun day-13 (&optional (file #p"day13/example-input.txt"))
  (with-open-file (stream file :direction :input)
    (loop
      with correct-packet-indexes = '()
      with all-packets = '()
      with packet-pair = '()
      with packet-index = 1
      for char = (read-char stream nil)
      do (ecase char
           (#\[
            (let ((packet (parse-packet-data stream)))
              (push packet packet-pair)
              (push packet all-packets))
            (read-char stream))         ; Consume new line
           ((nil #\NewLine)
            (when (apply 'packet< (nreverse packet-pair))
              (push packet-index correct-packet-indexes))
            (setq packet-pair '())
            (incf packet-index)))
      while char
      finally (format t "[Part 1] The sum of the indices of the correct ~
                         packet pairs is: ~d~%~
                         [Part 2] The decoder key for the distress signal is: ~d"
                      (apply '+ correct-packet-indexes)
                      (decoder-key all-packets)))))
;;; Testing

(defun test-packet< ()
  ;; Test for lower than
  (loop
    for (value1 . value2)
      in '(((1 1 3 1 1) . (1 1 5 1 1))
           ((1) . (1))
           ((2 3 4) . (4))
           (((1) (2 3 4)) . ((1) 4))
           (((4 4) 4 4) . ((4 4) 4 4 4))
           (() . (3))
           ((1 (1)) . (1 (1)))
           ((()) . ((()))))
    unless (packet< value1 value2)
      do (format t "~&F: Value ~a should be lower than ~a"
                 value1 value2))
  ;; Test for not lower than
  (loop
    for (value1 . value2)
      in '(((9) . (8 7 6))
           ((9) . ((8 7 6)))
           ((7 7 7 7) . (7 7 7))
           (((())) . (()))
           ((1 (2 (3 (4 (5 6 7)))) 8 9) . (1 (2 (3 (4 (5 6 0)))) 8 9))
           ((() (9)) . (() ((4))))
           )
    when (packet< value1 value2)
      do (format t "~&F: Value ~a should NOT be lower than ~a"
                 value1 value2))
  ;; Test &rest
  (unless (packet< '(1))
    (format t "~&F: Test for one value should always be T"))
  (unless (packet< '(0) '(1) '(2))
    (format t "~&F: Test for 2+ values is broken")))
