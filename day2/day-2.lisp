(in-package :cl-user)


(defparameter *shape-score-map*
  '((#\A . 1)                           ; rock
    (#\B . 2)                           ; paper
    (#\C . 3)))                         ; scissors


(defparameter *win/lose-map*
  '((#\A . (#\C . #\B))            ; rock:     beats scissors, loses to paper
    (#\B . (#\A . #\C))            ; paper:    beats rock,     loses to scissors
    (#\C . (#\B . #\A))))          ; scissors: beats paper,    loses to rock


(defun score (opponent outcome)
  (case outcome
    ;; Lose
    (#\X
     ;; Pick the option your opponent beats
     (let ((you (cadr (assoc opponent *win/lose-map* :test 'eq))))
       (cdr (assoc you *shape-score-map* :test 'eq))))
    ;; Draw
    (#\Y
     (+ (cdr (assoc opponent *shape-score-map* :test 'eq))
        3))
    ;; Win
    (#\Z
     ;; Pick the option your opponent loses to
     (let ((you (cddr (assoc opponent *win/lose-map* :test 'eq))))
       (+ (cdr (assoc you *shape-score-map* :test 'eq))
          6)))))


(defun day-2 (&optional (file #p"day2/example-input.txt"))
  (with-open-file (stream file :direction :input)
    (loop
      for opponent = (read-char stream nil)
      for outcome = (progn (read-char stream nil) ; Skip whitespace
                           (read-char stream nil))
      while (and opponent outcome)
      sum (score opponent outcome)
      ;; Read new line. Exit if there is none
      while (read-char stream nil))))
