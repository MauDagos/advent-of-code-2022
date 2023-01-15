(in-package :cl-user)


(defun day-1 (&optional (file #p"day1/example-input.txt"))
  (with-open-file (stream file :direction :input)
    (loop
      with inventories = '()
      with current-elf = 1
      with current-calories = nil
      for line = (read-line stream nil)
      while line
      if (zerop (length line))
        do (when current-calories
             (push (cons current-elf current-calories) inventories)
             (setq current-calories nil)
             (incf current-elf))
      else
        do (let ((calories (parse-integer line)))
             (if current-calories
                 (incf current-calories calories)
                 (setq current-calories calories)))
      finally
         (when current-calories
           (push (cons current-elf current-calories) inventories))
         (let ((top-elfs (subseq (sort inventories '> :key 'cdr) 0 3)))
           (format t "The top three elves carrying the most calories are ~
                      ~{~d~^, ~}, carrying in total: ~d"
                   (mapcar 'car top-elfs)
                   (apply '+ (mapcar 'cdr top-elfs)))))))
