(in-package :cl-user)


;;; Monkey data

(defstruct monkey
  id
  items
  operation
  test
  if-true
  if-false
  (inspected-items 0))


;;; Parsing monkeys

(defun skip-chars (stream count)
  (loop
    repeat count
    do (read-char stream)))


(defun parse-monkey-id (stream)
  (skip-chars stream #.(length "Monkey "))
  (parse-integer (read-line stream) :junk-allowed t))


(defun parse-starting-items (stream)
  (skip-chars stream #.(length "  Starting items: "))
  (let ((line (read-line stream))
        (items (make-instance 'mp:queue)))
    (loop
      with start = 0
      while (< start (length line))
      do (multiple-value-bind (item position)
             (parse-integer line :junk-allowed t :start start)
           (setq start (1+ position))
           (mp:enqueue items item)))
    items))


(defun parse-operation (stream)
  (skip-chars stream #.(length "  Operation: new = old "))
  (let* ((operation (read-char stream))
         (value (progn (read-char stream) ; skip whitespace
                       (read-line stream)))
         (op (ecase operation
               (#\+ '+)
               (#\* '*)))
         (val (unless (equal value "old")
                (parse-integer value))))
    (lambda (worry-level)
      (funcall op worry-level (or val worry-level)))))


(defun parse-test (stream)
  (skip-chars stream #.(length "  Test: divisible by "))
  (parse-integer (read-line stream)))


(defun parse-if-true (stream)
  (skip-chars stream #.(length "    If true: throw to monkey "))
  (parse-integer (read-line stream)))


(defun parse-if-false (stream)
  (skip-chars stream #.(length "    If false: throw to monkey "))
  (parse-integer (read-line stream)))


(defun parse-next-monkey (stream)
  (make-monkey :id (parse-monkey-id stream)
               :items (parse-starting-items stream)
               :operation (parse-operation stream)
               :test (parse-test stream)
               :if-true (parse-if-true stream)
               :if-false (parse-if-false stream)))


(defun parse-monkeys (file)
  (with-open-file (stream file :direction :input)
    (loop
      for monkey = (parse-next-monkey stream)
      collect monkey into monkeys
      while (read-line stream nil)      ; Consume new line
      finally (return (coerce monkeys 'simple-vector)))))


;;; Monkey business

(defun execute-turn (monkey monkeys &key mod-denominator)
  ;; Inspect item's worry level
  (let ((worry-level (mp:dequeue (monkey-items monkey))))
    ;; Update worry level by monkey's operation
    (setq worry-level (funcall (monkey-operation monkey) worry-level))
    ;; Keep worry level manageable; see 'MONKEY-BUSINESS
    (setq worry-level (if mod-denominator
                          (mod worry-level mod-denominator)
                          (floor worry-level 3)))
    ;; Test divisibility and throw item to new monkey
    (let ((throw-id (if (zerop (mod worry-level (monkey-test monkey)))
                        (monkey-if-true monkey)
                        (monkey-if-false monkey))))
      (mp:enqueue (monkey-items (svref monkeys throw-id))
                  worry-level))))


(defun execute-round (monkeys &key mod-denominator)
  (loop
    for monkey across monkeys
    do (loop
         until (mp:queue-empty-p (monkey-items monkey))
         do (execute-turn monkey monkeys :mod-denominator mod-denominator)
            (incf (monkey-inspected-items monkey)))))


(defun execute-rounds (rounds monkeys &key mod-denominator)
  (loop
    repeat rounds
    do (execute-round monkeys :mod-denominator mod-denominator)))


(defun monkey-business (file rounds &key (damage-relief-p t))
  (let* ((monkeys (parse-monkeys file))
         ;; I admit I don't really understand what's going on here, but kudos to
         ;; this user for trying to explain it:
         ;; https://github.com/dedolence/advent-of-code/blob/main/2022/day11.py
         (mod-denominator (unless damage-relief-p
                            (loop
                              with result = 1
                              for monkey across monkeys
                              do (setq result (* result (monkey-test monkey)))
                              finally (return result)))))
    (execute-rounds rounds monkeys :mod-denominator mod-denominator)
    (setq monkeys (sort monkeys '> :key 'monkey-inspected-items))
    (* (monkey-inspected-items (svref monkeys 0))
       (monkey-inspected-items (svref monkeys 1)))))


;;; Entrypoint

(defun day-11 (&optional (file #p"day11/example-input.txt"))
  (format t "[Part 1] Monkey business with damage relief after 20 rounds: ~d~%~
             [Part 2] Monkey business without damage relief after 10000 rounds: ~d"
          (monkey-business file 20)
          (monkey-business file 10000 :damage-relief-p nil)))


;;; Command line testing:
;;; alisp -L day11/day-11.lisp -e "(unwind-protect (day-11) (excl:exit 0))"
;;;
;;; alisp -L day11/day-11.lisp -e "(unwind-protect (print (monkey-business #p\"day11/example-input.txt\" 10000 :damage-relief-p nil)) (excl:exit 0))"
