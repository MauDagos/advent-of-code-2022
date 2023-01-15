(in-package :cl-user)


(defparameter *item-priorities*
  (let ((priorities (make-hash-table :test 'eq)))
    (loop
      for code from (char-code #\a) upto (char-code #\z)
      for priority from 1
      do (setf (gethash code priorities) priority))
    (loop
      for code from (char-code #\A) upto (char-code #\Z)
      for priority from 27
      do (setf (gethash code priorities) priority))
    priorities))


(defun item-priority (item)
  (gethash (char-code item) *item-priorities*))


(defun find-failure (rucksack)
  (loop
    with middle = (/ (length rucksack) 2)
    for item across rucksack
    for index from 0
    while (< index middle)
    do (loop
         for other-index from 0 below middle
         for other-item = (aref rucksack (+ other-index middle))
         when (eq item other-item)
           do (return-from find-failure item))))


(defun find-badge (rucksack-1 rucksack-2 rucksack-3)
  (let* ((len-1 (length rucksack-1))
         (len-2 (length rucksack-2))
         (len-3 (length rucksack-3))
         (shortest (min len-1 len-2 len-3))
         the-rucksack
         other-rucksack-1
         other-rucksack-2)
    (cond
      ((= shortest len-1) (setq the-rucksack     rucksack-1
                                other-rucksack-1 rucksack-2
                                other-rucksack-2 rucksack-3))
      ((= shortest len-2) (setq the-rucksack     rucksack-2
                                other-rucksack-1 rucksack-1
                                other-rucksack-2 rucksack-3))
      ((= shortest len-3) (setq the-rucksack     rucksack-3
                                other-rucksack-1 rucksack-2
                                other-rucksack-2 rucksack-1)))
    (loop
      for item across the-rucksack
      when (and (find item other-rucksack-1 :test 'eq)
                (find item other-rucksack-2 :test 'eq))
        return item)))


(defun day-3 (&optional (file #p"day3/example-input.txt"))
  ;; Part 1
  (with-open-file (stream file :direction :input)
    (loop
      for rucksack = (read-line stream nil)
      while rucksack
      for failure = (find-failure rucksack)
      sum (item-priority failure) into sum-priorites
      finally (format t "The sum of the priorities of the failures is: ~d"
                      sum-priorites)))
  ;; Part 2
  (with-open-file (stream file :direction :input)
    (loop
      for rucksack-1 = (read-line stream nil)
      for rucksack-2 = (read-line stream nil)
      for rucksack-3 = (read-line stream nil)
      while (and rucksack-1 rucksack-2 rucksack-3)
      for badge = (find-badge rucksack-1 rucksack-2 rucksack-3)
      sum (item-priority badge) into sum-priorites
      finally (format t "~2&The sum of the priorities of the badges is: ~d"
                      sum-priorites))))
