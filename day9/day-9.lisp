(in-package :cl-user)


(defun update-tail-position (head-position tail-position)
  "Return T or NIL depending if TAIL-POSITION was updated or not."
  (let (updatedp)
    (destructuring-bind (head-x . head-y) head-position
      (destructuring-bind (tail-x . tail-y) tail-position
        (let* ((x-dist (- head-x tail-x))
               (y-dist (- head-y tail-y))
               (x-far-p (> (abs x-dist) 1))
               (y-far-p (> (abs y-dist) 1)))
          (cond
            ;; Not far in any axis, no need to do anything.
            ((not (or x-far-p y-far-p)))
            ;; On the same X position. Check if the head has gotten too far up or
            ;; down.
            ((= head-x tail-x)
             (when y-far-p
               (incf (cdr tail-position) (if (plusp y-dist) 1 -1))
               (setq updatedp t)))
            ;; On the same Y position. Check if the head has gotten too far left
            ;; or right.
            ((= head-y tail-y)
             (when x-far-p
               (incf (car tail-position) (if (plusp x-dist) 1 -1))
               (setq updatedp t)))
            ;; Move the tail diagonally.
            (t
             (incf (car tail-position) (if (plusp x-dist) 1 -1))
             (incf (cdr tail-position) (if (plusp y-dist) 1 -1))
             (setq updatedp t))))))
    (values updatedp tail-position)))


(defun update-tail-trail (tail-position tail-trail)
  (unless (gethash tail-position tail-trail)
    (setf (gethash (copy-seq tail-position) tail-trail) t)))


(defun update-tails (head-position tails tail-trail)
  (loop
    for follow-position = head-position then tail-position
    for tail-position in tails
    ;; If we haven't updated this position, then no need to check the rest of
    ;; the tail and, therefore, no need to update the trail.
    do (unless (update-tail-position follow-position tail-position)
         (return))
    finally (update-tail-trail tail-position tail-trail)))


(defun execute-motion (motion steps head-position tails tail-trail)
  ;; HEAD-POSITION and TAIL-POSITION are CONS keeping track of each item's
  ;; position on the X and Y axis, respectively.
  ;;
  ;; Moving up or down goes through the Y axis. Moving left or right goes
  ;; through the X axis.
  (let ((update-head-fn (ecase motion
                          (#\U (lambda () (incf (cdr head-position))))
                          (#\D (lambda () (decf (cdr head-position))))
                          (#\R (lambda () (incf (car head-position))))
                          (#\L (lambda () (decf (car head-position)))))))
    (loop
      repeat steps
      do (funcall update-head-fn)
         (update-tails head-position tails tail-trail))))


(defun day-9 (&optional (file #p"day9/example-input.txt") (tails-amount 1))
  (with-open-file (stream file :direction :input)
    (loop
      with head-position = (cons 0 0)
      with tails = (loop repeat tails-amount collect (cons 0 0))
      with tail-trail = (make-hash-table :test 'equal)
      initially (update-tail-trail (cons 0 0) tail-trail)
      for motion = (read-char stream nil)
      while motion
      do (read-char stream)             ; consume whitespace
         (let ((steps (parse-integer (read-line stream))))
           (execute-motion motion steps head-position tails tail-trail))
      finally (format t "~&[Tails: ~d] Amount of positions visited by the tail: ~d"
                      tails-amount
                      (hash-table-count tail-trail))
              (return tail-trail))))


;;; Testing

(defun test-update-tail-position ()
  ;; Not all cases are covered
  (loop
    for (head-position tail-position expected-tail-position)
      in '( ; Basic cases for not moving
           ((0 . 0) (0 . 0) (0 . 0))
           ((1 . 0) (0 . 0) (0 . 0))
           ((0 . 1) (0 . 0) (0 . 0))
           ((1 . 1) (0 . 0) (0 . 0))
           ;; Moving diagonally up-right
           ((1 . 2) (0 . 0) (1 . 1))
           ;; Moving diagonally down-right
           ((7 . 2) (5 . 3) (6 . 2))
           ;; Moving diagonally up-left
           ((3 . 6) (4 . 4) (3 . 5))
           ;; Moving diagonally down-left
           ((1 . 4) (3 . 5) (2 . 4))
           )
    for new-tail-position
      = (nth-value 1 (update-tail-position head-position (copy-seq tail-position)))
    unless (equalp new-tail-position expected-tail-position)
      do (format t "~&F: for H ~a and T ~a expected ~a but got ~a"
                 head-position tail-position expected-tail-position new-tail-position)))


(defun test-day-9 ()
  (loop
    for (file tails-amount expected-count)
      in '((#p"day9/example-input.txt" 1 13)
           (#p"day9/example-input.txt" 9 1)
           (#p"day9/example-input-2.txt" 9 36))
    for tail-trail = (day-9 file tails-amount)
    for count = (hash-table-count tail-trail)
    unless (= expected-count count)
      do (format t "~&F: for file ~a and tails ~a, expected ~d positions but got ~d"
                 file tails-amount expected-count count)))
