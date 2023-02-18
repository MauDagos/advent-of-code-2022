(in-package :cl-user)


;;; Data preparation

(defun parse-heightmap (file)
  (with-open-file (stream file :direction :input)
    (let (heightmap start-position end-position)
      (loop
        with rows = nil
        with current-row = nil
        with row-count = 0
        with column-count = 0
        with current-row-column-index = 0
        for char = (read-char stream nil)
        while char
        if (eq char #\NewLine)
          do (push (nreverse current-row) rows)
             (setq current-row nil
                   column-count current-row-column-index
                   current-row-column-index 0)
             (incf row-count)
        else
          do (case char
               (#\S
                (setq start-position (cons row-count current-row-column-index))
                (push #\a current-row))
               (#\E
                (setq end-position (cons row-count current-row-column-index))
                (push #\z current-row))
               (t
                (push char current-row)))
             (incf current-row-column-index)
        finally
           (when current-row
             (push (nreverse current-row) rows)
             (incf row-count))
           (setq heightmap (make-array (list row-count column-count)
                                       :initial-contents (nreverse rows))))
      (values heightmap start-position end-position))))


(defun find-positions (char heightmap)
  (destructuring-bind (row-count column-count) (array-dimensions heightmap)
    (loop
      with positions = '()
      for row from 0 below row-count
      do (loop
           for column from 0 below column-count
           when (eq char (aref heightmap row column))
             do (push (cons row column) positions))
      finally (return positions))))


;;; A* implementation

(defclass node ()
  ((code :initform (error "CODE must be supplied")
         :initarg :code
         :reader node-code
         :documentation "The char-code of the char found in the heightmap.")
   (position :initform (error "POSITION must be supplied")
             :initarg :position
             :reader node-position
             :documentation "A CONS with 2 integers: row position & column position.")
   (parent :initform nil
           :initarg :parent
           :accessor node-parent
           :documentation "The node immediately preceding this node on the
                           cheapest path from the start node.")
   (g :initform 0
      :initarg :g
      :accessor node-g
      :documentation "The cost of the cheapest path to this node from the start node.")
   (h :initform 0
      :accessor node-h
      :documentation "The calculated cost, using the heuristic, of the path from
                      this node to the end node.")
   (f :initform 0
      :accessor node-f
      :documentation "The sum of G and F. Our current best guess as to how cheap
                      a path could be from start to end if it goes through this node.")))


(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "CODE ~d POS ~a g ~d"
            (node-code node)
            (node-position node)
            (node-g node))))


(defmethod initialize-instance :after ((node node) &key heuristic-fn all-nodes
                                       &allow-other-keys)
  "Compute the h and f scores and register the new node."
  (setf (node-h node) (funcall heuristic-fn (node-position node))
        (node-f node) (+ (node-g node) (node-h node))
        (gethash (node-position node) all-nodes) node))


(defun reconstruct-path (node)
  "Return the path of the search to NODE."
  (loop
    with path = '()
    for current-node = node then (node-parent current-node)
    while current-node
    do (push (node-position current-node) path)
    finally (return path)))


(defun find-best-node (open-set)
  "Find the node with the lowest f score."
  (loop
    with best-node = nil
    for node being the hash-key in open-set
    when (or (null best-node)
             (< (node-f node) (node-f best-node)))
      do (setq best-node node)
    finally (return best-node)))


(defun get/make-neighbor (all-nodes node-position node-code node-parent
                          heuristic-fn)
  "Return a node in ALL-NODES with position NODE-POSITION or a new node."
  (or (gethash node-position all-nodes)
      (make-instance 'node
                     :position node-position
                     :code node-code
                     :parent node-parent
                     ;; Sum 1 to account for the cost of moving from the parent
                     ;; to this neighbor.
                     :g (1+ (node-g node-parent))
                     :heuristic-fn heuristic-fn
                     :all-nodes all-nodes)))


(defun neighbors (node heightmap all-nodes heuristic-fn)
  "Return the valid neighbors of NODE.

A neighbor is valid if it's char-code is lower or at most 1 greater."
  (destructuring-bind (row-pos . col-pos) (node-position node)
    (destructuring-bind (row-count col-count) (array-dimensions heightmap)
      (loop
        with neighbors = '()
        for (row-delta . col-delta) in '((-1 .  0)  ; up
                                         (0  .  1)  ; right
                                         (1  .  0)  ; down
                                         (0  . -1)) ; left
        do (let ((neighbor-row (+ row-pos row-delta))
                 (neighbor-col (+ col-pos col-delta)))
             (when (and (<= 0 neighbor-row (1- row-count))
                        (<= 0 neighbor-col (1- col-count)))
               (let ((neighbor-code
                       (char-code (aref heightmap neighbor-row neighbor-col))))
                 (when (or (<= neighbor-code (node-code node))
                           (<= (- neighbor-code (node-code node)) 1))
                   (push (get/make-neighbor all-nodes
                                            (cons neighbor-row neighbor-col)
                                            neighbor-code
                                            node
                                            heuristic-fn)
                         neighbors)))))
        finally (return neighbors)))))


(define-condition no-path-found (error) ())


(defun a-star (heightmap start-position end-position heuristic-fn)
  "Return the shortest path from START-POSITION to END-POSITION."
  (let* ( ; Keep track of all discovered nodes by position.
         (all-nodes (make-hash-table :test 'equal))
         ;; Start off by creating the start node.
         (start-node (make-instance 'node
                                    :code (char-code #\a)
                                    :position start-position
                                    :heuristic-fn heuristic-fn
                                    :all-nodes all-nodes))
         ;; The set of discovered nodes that may need to be expanded.
         (open-set (make-hash-table :test 'eq)))
    ;; Initially, only the start node is known.
    (setf (gethash start-node open-set) t)
    (loop
      while (plusp (hash-table-count open-set))
      for current-node = (find-best-node open-set)
      if (equal (node-position current-node) end-position)
        do (return-from a-star
             (values (reconstruct-path current-node)
                     current-node))
      else
        do (remhash current-node open-set)
           (loop
             for neighbor-node in (neighbors current-node heightmap all-nodes
                                             heuristic-fn)
             ;; The new g score for NEIGHBOR-NODE by going through CURRENT-NODE.
             ;; Sum 1 to account for the cost of moving from the parent to this
             ;; neighbor.
             for tentative-g-score = (1+ (node-g current-node))
             when (<= tentative-g-score (node-g neighbor-node))
               do ;; The path to neighbor is better than any previous one.
                  (setf (node-parent neighbor-node) current-node
                        (node-g neighbor-node) tentative-g-score
                        (node-f neighbor-node) (+ tentative-g-score
                                                  (node-h neighbor-node))
                        (gethash neighbor-node open-set) t)))
    (error 'no-path-found)))


(defun distance (pos1 pos2)
  "Return the manhattan distance between two positions."
  (+ (abs (- (car pos1) (car pos2)))
     (abs (- (cdr pos1) (cdr pos2)))))


;;; Entrypoint

(defun day-12 (&optional (file #p"day12/example-input.txt"))
  (multiple-value-bind (heightmap start-position end-position)
      (parse-heightmap file)
    (let ((heuristic-fn (lambda (node-position)
                          (funcall 'distance node-position end-position))))
      (multiple-value-bind (shortest-path end-node)
          (a-star heightmap start-position end-position heuristic-fn)
        ;; I could print a drawing of the path like the exercise...
        (declare (ignore shortest-path))
        ;; Since the cost of moving between positions is 1, the g score of the
        ;; end node is the same as the amount of steps. Also equal to:
        ;;   (1- (length shortest-path))
        (format t "~&[Part 1] The shortest path from S to E has steps: ~d"
                (node-g end-node)))
      (let* ((lowest-positions (find-positions #\a heightmap))
             (least-steps
               (loop
                 for position in lowest-positions
                 for end-node
                   = (handler-case
                         (nth-value 1 (a-star heightmap position end-position
                                              heuristic-fn))
                       (no-path-found () nil))
                 when end-node
                   minimize (node-g end-node))))
        (format t "~&[Part 2] The shortest path from the lowest positions has ~
                   steps: ~d"
                least-steps)))))
