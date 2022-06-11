(in-package cl-user)

(defclass priority-queue ()
  ((queue :initform nil)
   (compare-function :initarg :compare-function
		     :initform #'<)))

(defmethod insert-thing ((self priority-queue) thing queue &key (compare #'<))
  (cond
   ((null queue)
    (setq queue (list thing)))
   ((funcall compare thing (car queue))
    ;; less than, should go here:
    (setf (cdr queue) (cons (car queue) (cdr queue)))
    (setf (car queue) thing))
   ((null (cdr queue))
    (setf (cdr queue) (list thing))
    queue)
   (t (insert-thing self thing (cdr queue) :compare compare)))
  queue)

(defmethod enqueue ((self priority-queue) thing)
  (with-slots (compare-function queue) self
    (setq queue (insert-thing self thing queue :compare compare-function))))


(defmethod dequeue ((self priority-queue))
  (with-slots (queue) self
    (pop queue)))

(defmethod print-queue ((self priority-queue))
  (with-slots (queue) self
    (format t "~s~%" 
	    (mapcar #'(lambda (a) (with-slots (cost coord) a
				    (list :node a :coord coord :cost cost)))
		    queue))))

;;;check to see if queue is empty
(defmethod is-empty ((self priority-queue))
  (with-slots (queue) self
    (null queue)
  )
)

(defmethod length_queue ((self priority-queue))
  (with-slots (queue) self
    (length queue)
  )
)

;;;get a specific node object based on a coordinate
(defmethod get-node-from-location ((self priority-queue) loc)
  (with-slots (queue) self
    (setq node nil)
    (loop for element in queue do
      (if (equal (slot-value element 'coord) loc)
          (setf node element))
    )
    node
  )
)

;;;check to see if a specific node exist based on a coordinate
(defmethod is-in-queue ((self priority-queue) loc)
  (with-slots (queue) self
    (setq bool nil)
    (loop for element in queue do
      (if (equal (slot-value element 'coord) loc)
          (setf bool t))
    )
    bool
  )
)

;;;pass a node to remove it from the queue
(defmethod remove-from-queue ((self priority-queue) node)
  (with-slots (queue) self
    (setq queue (delete node queue))
  )
)



;;;Sample Node Class

(defclass sample-node ()
  ((coord :initarg :coord :initform nil)
   (cost :initarg :cost :initform 0 :accessor cost)
   (gcost :initarg :gcost :initform 0) ;;;g-cost of the node for A*
   (hcost :initarg :hcost :initform 0) ;;;h-cost of the node for A*
   (actions :initarg :actions :initform '()) ;;;way to keep track of all actions we took to get to a node
   (parent :initarg :parent :initform nil)))

(defmethod cmp-nodes ((node1 sample-node) (node2 sample-node))
  (< (cost node1) (cost node2)))




