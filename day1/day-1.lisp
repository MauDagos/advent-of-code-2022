(in-package :cl-user)


(defun day-1 (&optional (file #p"day1/example-input.txt"))
  (with-open-file (stream file :direction :input)
    (loop
      with winner-elf = nil
      with winner-calories = nil
      with current-elf = 1
      with current-calories = nil
      for line = (read-line stream nil)
      while line
      if (zerop (length line))
        do (when current-calories
             (when (or (null winner-elf)
                       (> current-calories winner-calories))
               (setq winner-elf current-elf
                     winner-calories current-calories))
             (setq current-calories nil)
             (incf current-elf))
      else
        do (let ((calories (parse-integer line)))
             (if current-calories
                 (incf current-calories calories)
                 (setq current-calories calories)))
      finally (format t "Elf #~d is carrying ~d calories"
                      winner-elf winner-calories))))
