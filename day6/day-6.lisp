(in-package :cl-user)


(defclass packet (mp:queue)
  ((size :initarg :size
         :initform (error "Must supply SIZE")
         :reader size)))


(defun track-char (packet char)
  (mp:enqueue packet char)
  (when (> (mp:queue-length packet) (size packet))
    (mp:dequeue packet)))


(defun has-duplicates-p (list)
  (cond ((null list) nil)
        ((member (car list) (cdr list) :test 'eq) t)
        (t (has-duplicates-p (cdr list)))))


(defun validate-packet (packet)
  (with-slots (mp::head) packet
    (not (has-duplicates-p mp::head))))


(defun day-6 (packet-size &optional (file #p"day6/example-input-1.txt"))
  (with-open-file (stream file :direction :input)
    (loop
      with packet = (make-instance 'packet :size packet-size)
      for marker from 1
      for char = (read-char stream nil)
      while char
      do (track-char packet char)
         (when (and (= (mp:queue-length packet) (size packet))
                    (validate-packet packet))
           (return-from day-6 marker)))))


;;; Testing

(defun test-day-6 ()
  (format t "~&Packet size: 4~&")
  (loop
    for (file . expected-marker)
      in '((#p"day6/example-input-1.txt" . 7)
           (#p"day6/example-input-2.txt" . 5)
           (#p"day6/example-input-3.txt" . 6)
           (#p"day6/example-input-4.txt" . 10)
           (#p"day6/example-input-5.txt" . 11))
    for marker = (day-6 4 file)
    do (format t "~&~a~&" (= marker expected-marker)))
  (format t "~&Packet size: 14~&")
  (loop
    for (file . expected-marker)
      in '((#p"day6/example-input-1.txt" . 19)
           (#p"day6/example-input-2.txt" . 23)
           (#p"day6/example-input-3.txt" . 23)
           (#p"day6/example-input-4.txt" . 29)
           (#p"day6/example-input-5.txt" . 26))
    for marker = (day-6 14 file)
    do (format t "~&~a~&" (= marker expected-marker))))
