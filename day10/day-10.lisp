(in-package :cl-user)


(defparameter *crt-width* 40)


(defvar *program-state*)


(defstruct state
  (cycle 0)
  (x 1)
  (cycles-to-check nil)
  (total-signal-strength 0)
  (crt-out-stream nil))


(defun call-with-program-state (cycles body-fn)
  (let ((*program-state* (make-state :cycles-to-check cycles
                                     :crt-out-stream (make-string-output-stream))))
    (funcall body-fn)))


(defmacro with-program-state ((&optional cycles) &body body)
  `(call-with-program-state ,cycles (lambda () ,@body)))


(defun update-total-signal-strength ()
  (let ((cycle (state-cycle *program-state*))
        (cycles-to-check (state-cycles-to-check *program-state*)))
    (when (and cycles-to-check (= cycle (first cycles-to-check)))
      (pop (state-cycles-to-check *program-state*))
      (incf (state-total-signal-strength *program-state*)
            (* cycle (state-x *program-state*))))))


(defun update-crt ()
  (let* ((crt-out-stream (state-crt-out-stream *program-state*))
         (cycle (state-cycle *program-state*))
         (x (state-x *program-state*))
         (pixel-position (rem (1- cycle) *crt-width*))
         (sprite-position (rem x *crt-width*)))
    (write-char (if (or (= pixel-position (1- sprite-position))
                        (= pixel-position sprite-position)
                        (= pixel-position (1+ sprite-position)))
                    #\#
                    #\.)
                crt-out-stream)
    (when (zerop (mod cycle *crt-width*))
      (terpri crt-out-stream))))


(defun incf-cycle (&optional (delta 1))
  (loop
    repeat delta
    do (incf (state-cycle *program-state*))
       (update-total-signal-strength)
       (update-crt)))


(defun noop ()
  (incf-cycle))


(defun addx (v)
  (incf-cycle 2)
  (incf (state-x *program-state*) v))


(defun read-instruction (stream)
  (read-from-string
   (with-output-to-string (out)
     (loop
       for char = (read-char stream nil)
       while (and char
                  (not (member char '(#\NewLine #\Space) :test 'eq)))
       do (write-char char out)))))


(defun day-10 (&optional (file #p"day10/example-input.txt")
                 (cycles (list 20 60 100 140 180 220)))
  (with-open-file (stream file :direction :input)
    (with-program-state (cycles)
      (loop
        while (peek-char nil stream nil)
        for instruction = (read-instruction stream)
        for argument = (unless (eq instruction 'noop)
                         (parse-integer (read-line stream)))
        do (apply instruction (when argument (list argument))))
      (format t "~&[Part 1] The total signal strength is: ~d~
                 ~&[Part 2] CRT:~%~a"
              (state-total-signal-strength *program-state*)
              (get-output-stream-string (state-crt-out-stream *program-state*)))
      *program-state*)))


;;; Testing

(defun test-day-10 ()
  ;; No testing for CRT output
  (loop
    for (file cycles expected-strength)
      in '((#p"day10/example-input.txt" (20) 0)
           (#p"day10/example-input.txt" (1 2 3) 6)
           (#p"day10/example-input.txt" (1 2 3 4 5) 42)
           (#p"day10/example-input-2.txt" (20 60 100 140 180 220) 13140))
    for state = (day-10 file cycles)
    for strength = (state-total-signal-strength state)
    unless (= strength expected-strength)
      do (format t "~&F: for file ~a and cycles ~a expected strength ~d but got ~d"
                 file cycles expected-strength strength)))
