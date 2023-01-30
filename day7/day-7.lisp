(in-package :cl-user)


(defclass dir ()
  ((name :initarg :name
         :initform (error "Must supply NAME")
         :reader name)
   (parent-dir :initarg :parent-dir
               :initform nil
               :reader parent-dir)
   (files-size :initform 0
               :accessor files-size)
   (directories :initform nil
                :accessor directories)
   (total-size :initform 0
               :accessor total-size)))


(defmethod initialize-instance :after ((dir dir) &key)
  (let ((parent-dir (parent-dir dir)))
    (when parent-dir
      (push dir (directories parent-dir)))))


(defmethod print-object ((dir dir) stream)
  (print-unreadable-object (dir stream :type t :identity t)
    (format stream "~s ~d ~d~@[ (~{~s~^, ~})~]"
            (name dir)
            (files-size dir)
            (total-size dir)
            (mapcar 'name (directories dir)))))


(defvar *root*)


(defvar *current-dir*)


(defun parse-command (line)
  (cond
    ;; $ cd
    ((and (eq #\c (char line 2))
          (eq #\d (char line 3)))
     (let ((argument (subseq line 5)))
       (cond
         ((equal argument "..")
          (setq *current-dir* (parent-dir *current-dir*)))
         ((equal argument "/")
          (setq *current-dir* *root*))
         (t
          (setq *current-dir* (make-instance 'dir
                                             :name argument
                                             :parent-dir *current-dir*))))))
    ;; $ ls
    ((and (eq #\l (char line 2))
          (eq #\s (char line 3)))
     ;; Do nothing
     nil)
    (t (error "Unknown command ~a" line))))


(defun parse-output (line)
  ;; Ignore outputs like "dir a"
  (when (digit-char-p (char line 0))
    ;; Ignore the file names
    (let ((size (parse-integer line :junk-allowed t)))
      (incf (files-size *current-dir*) size))))


(defun parse-line (line)
  (if (eq #\$ (char line 0))
      ;; Parse the command
      (parse-command line)
      ;; Parse the output
      (parse-output line)))


(defun compute-total-sizes (dir)
  (let ((directories-size (loop
                            for d in (directories dir)
                            sum (compute-total-sizes d))))
    (setf (total-size dir)
          (+ directories-size (files-size dir)))))


(defun small-directories-total-size ()
  (let ((max-size 100000)
        (candidates-size 0))
    (labels ((find-candidates (dir)
               (when (<= (total-size dir) max-size)
                 (incf candidates-size (total-size dir)))
               (dolist (d (directories dir))
                 (find-candidates d))))
      (find-candidates *root*)
      candidates-size)))


(defun smallest-significant-directory-total-size ()
  (let* ((disk-space 70000000)
         (free-space-needed 30000000)
         (used-space (total-size *root*))
         (unused-space (- disk-space used-space))
         (threshold (- free-space-needed unused-space))
         (candidate nil))
    (labels ((find-candidate (dir)
               (when (and (>= (total-size dir) threshold)
                          (or (null candidate)
                              (< (total-size dir) (total-size candidate))))
                 (setq candidate dir))
               (dolist (d (directories dir))
                 (find-candidate d))))
      (find-candidate *root*)
      (total-size candidate))))


(defun day-7 (&optional (file #p"day7/example-input.txt"))
  (with-open-file (stream file :direction :input)
    (loop
      with *root* = (make-instance 'dir :name "/")
      with *current-dir* = nil
      for line = (read-line stream nil)
      while line
      do (parse-line line)
      finally (compute-total-sizes *root*)
              (return (values
                       ;; Part 1
                       (small-directories-total-size)
                       ;; Part 2
                       (smallest-significant-directory-total-size)))
      )))
