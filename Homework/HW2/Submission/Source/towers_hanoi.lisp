;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This document contains all tower of hanoi code
;;; including tower of hanoi objects, modified priority queue,
;;; search algorithms and agent program.
;;;
;;; Author: Nicholas Soucy
;;;
;;; COS 570 F2021
;;;

;;;Global Variables for Testing
(setq node_number_tower 0)

;;;create towers of hanoi object
(defclass towers_hanoi ()
  ((cost :initarg :cost :initform 0 :accessor cost)
   (gcost :initarg :gcost :initform 0 :accessor gcost)
   (hcost :initarg :hcost :initform 0 :accessor hcost)
   (A :initarg :A :initform '() :accessor A) ;;;start peg
   (B :initarg :B :initform '() :accessor B) ;;;middle peg
   (C :initarg :C :initform '() :accessor C) ;;;final peg
   (parents :initarg :parents :initform '()))) ;;;list of all previous states


(defmethod cmp-towers_hanoi ((node1 towers_hanoi) (node2 towers_hanoi))
  (< (cost node1) (cost node2)))


;;;prints tower_hanoi state: the cost and the stack on each peg
(defmethod print-state ((self towers_hanoi))
    (format t "Cost: ~s ~% A: ~s ~% B: ~s ~% C: ~s ~%" (slot-value self 'cost) (slot-value self 'A) (slot-value self 'B) (slot-value self 'C))
)


;;;modified simple-pqueue to work with tower objects 

(defclass pqueue-towers ()
  ((queue :initform nil)
   (compare-function :initarg :compare-function
		     :initform #'<)))

(defmethod insert-thing ((self pqueue-towers) thing queue &key (compare #'<))
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

(defmethod enqueue ((self pqueue-towers) thing)
  (with-slots (compare-function queue) self
    (setq queue (insert-thing self thing queue :compare compare-function))))


(defmethod dequeue ((self pqueue-towers))
  (with-slots (queue) self
    (pop queue)))

(defmethod peek ((self pqueue-towers))
  (with-slots (queue) self
    (car queue)))

(defmethod length_queue ((self pqueue-towers))
  (with-slots (queue) self
    (length queue)
  )
)

;;;check to see if queue is empty
(defmethod is-empty ((self pqueue-towers))
  (with-slots (queue) self
    (null queue)
  )
)

;;;get a specific state object in queue given a state
(defmethod get-node-from-state ((self pqueue-towers) state)
  (with-slots (queue) self
    (setq node nil)
    (loop for element in queue do
      (if (and (equal (slot-value element 'A) (slot-value state 'A)) (equal (slot-value element 'B) (slot-value state 'B)) (equal (slot-value element 'C) (slot-value state 'C)))
          (setf node element))
    )
    node
  )
)

;;;check to see if a specific state is on queue
(defmethod is-in-queue ((self pqueue-towers) state)
  (with-slots (queue) self
    (setq bool nil)
    (loop for element in queue do
      (if (and (equal (slot-value element 'A) (slot-value state 'A)) (equal (slot-value element 'B) (slot-value state 'B)) (equal (slot-value element 'C) (slot-value state 'C)))
          (setf bool t))
    )
    bool
  )
)

;;;remove given state from queue
(defmethod remove-from-queue ((self pqueue-towers) node)
  (with-slots (queue) self
    (setq queue (delete node queue))
  )
)




;;;Uniform Cost

;;;gets all possible moves based on current state
(defun get_children_towers_uniform (state)

    (setq answer '())

    ;pop off A and see where we can go 
    (setq tm1 (car (slot-value state 'A)))
    (if (and tm1 (car (slot-value state 'B)))
        (progn (cond ((< tm1 (car (slot-value state 'B)))
                    (setq p (slot-value state 'parents))
                    (push state p)
                    (push (make-instance 'towers_hanoi :cost (1+ (slot-value state 'cost)) :A (cdr (slot-value state 'A)) :B (concatenate 'list (list tm1) (slot-value state 'B)) :C (slot-value state 'C) :parents p) answer))
        )))
    (if (and tm1 (not (car (slot-value state 'B))))
        (progn
            (setq p (slot-value state 'parents))
            (push state p)
            (push (make-instance 'towers_hanoi :cost (1+ (slot-value state 'cost)) :A (cdr (slot-value state 'A)) :B (concatenate 'list (list tm1) (slot-value state 'B)) :C (slot-value state 'C) :parents p) answer))
    )
        
    (if (and tm1 (car (slot-value state 'C)))
        (progn (cond ((< tm1 (car (slot-value state 'C)))
                    (setq p (slot-value state 'parents))
                    (push state p)
                    (push (make-instance 'towers_hanoi :cost (1+ (slot-value state 'cost)) :A (cdr (slot-value state 'A)) :B (slot-value state 'B) :C (concatenate 'list (list tm1) (slot-value state 'C)) :parents p) answer))
        )))
    (if (and tm1 (not (car (slot-value state 'C))))
        (progn
            (setq p (slot-value state 'parents))
            (push state p)
            (push (make-instance 'towers_hanoi :cost (1+ (slot-value state 'cost)) :A (cdr (slot-value state 'A)) :B (slot-value state 'B) :C (concatenate 'list (list tm1) (slot-value state 'C)) :parents p) answer))
    )



    ;pop off B and see where we can go 
    (setq tm2 (car (slot-value state 'B)))
    (if (and tm2 (car (slot-value state 'A)))
        (progn (cond ((< tm2 (car (slot-value state 'A)))
                (setq p (slot-value state 'parents))
                (push state p)
                (push (make-instance 'towers_hanoi :cost (1+ (slot-value state 'cost)) :A (concatenate 'list (list tm2) (slot-value state 'A)) :B (cdr (slot-value state 'B)) :C (slot-value state 'C) :parents p) answer))
        )))
    (if (and tm2 (not (car (slot-value state 'A))))
        (progn
            (setq p (slot-value state 'parents))
            (push state p)
            (push (make-instance 'towers_hanoi :cost (1+ (slot-value state 'cost)) :A (concatenate 'list (list tm2) (slot-value state 'A)) :B (cdr (slot-value state 'B)) :C (slot-value state 'C) :parents p) answer))
    )


    (if (and tm2 (car (slot-value state 'C)))
        (progn (cond ((< tm2 (car (slot-value state 'C)))
                (setq p (slot-value state 'parents))
                (push state p)
                (push (make-instance 'towers_hanoi :cost (1+ (slot-value state 'cost)) :A (slot-value state 'A) :B (cdr (slot-value state 'B)) :C (concatenate 'list (list tm2) (slot-value state 'C)) :parents p) answer))
        )))
    (if (and tm2 (not (car (slot-value state 'C))))
        (progn
            (setq p (slot-value state 'parents))
            (push state p)
            (push (make-instance 'towers_hanoi :cost (1+ (slot-value state 'cost)) :A (slot-value state 'A) :B (cdr (slot-value state 'B)) :C (concatenate 'list (list tm2) (slot-value state 'C)) :parents p) answer))
    )

    ;pop off C and see where we can go
    (setq tm3 (car (slot-value state 'C)))
    (if (and tm3 (car (slot-value state 'A)))
        (progn (cond ((< tm3 (car (slot-value state 'A)))
                (setq p (slot-value state 'parents))
                (push state p)               
                (push (make-instance 'towers_hanoi :cost (1+ (slot-value state 'cost)) :A (concatenate 'list (list tm3) (slot-value state 'A)) :B (slot-value state 'B) :C (cdr (slot-value state 'C)) :parents p) answer))
        )))
    (if (and tm3 (not (car (slot-value state 'A))))
        (progn
            (setq p (slot-value state 'parents))
            (push state p)
            (push (make-instance 'towers_hanoi :cost (1+ (slot-value state 'cost)) :A (concatenate 'list (list tm3) (slot-value state 'A)) :B (slot-value state 'B) :C (cdr (slot-value state 'C)) :parents p) answer))
    )

    (if (and tm3 (car (slot-value state 'B)))
        (progn (cond ((< tm3 (car (slot-value state 'B)))
                (setq p (slot-value state 'parents))
                (push state p)
                (push (make-instance 'towers_hanoi :cost (1+ (slot-value state 'cost)) :A (slot-value state 'A) :B (concatenate 'list (list tm3) (slot-value state 'B)) :C (cdr (slot-value state 'C)) :parents p) answer))
        )))
    (if (and tm3 (not (car (slot-value state 'B))))
        (progn
            (setq p (slot-value state 'parents))
            (push state p)
            (push (make-instance 'towers_hanoi :cost (1+ (slot-value state 'cost)) :A (slot-value state 'A) :B (concatenate 'list (list tm3) (slot-value state 'B)) :C (cdr (slot-value state 'C)) :parents p) answer))
    )

    ;;;return list of all possible child states
    answer

)

(defun uniform-cost-towers (goal)
    ;;;made visited a queue to use custum 'is-in-queue' function
    (setq visited (make-instance 'pqueue-towers :compare-function #'cmp-towers_hanoi))
    (setq parents '()) ;;; list of all previous states
    (setq node_number_tower 0)
    ;;;setup the fringe priority queue and queue the start state with cost 0
    (setq fringe (make-instance 'pqueue-towers :compare-function #'cmp-towers_hanoi))
    (enqueue fringe (make-instance 'towers_hanoi :cost 0 :A goal :parents parents))
    (setf node_number_tower (1+ node_number_tower))
    ;;;loop while there is elements in fringe
    (loop while (not (is-empty fringe)) do
        ;;;dequeue priority node
        (setq curr_path (dequeue fringe))
        (cond 
            ;;;if priority node's state is the end state, return current state and exit
            ((equal (slot-value curr_path 'C) goal) (return-from uniform-cost-towers curr_path))
            (t
                 ;;;if not, mark state as visited and get all children of state
                (enqueue visited curr_path)
                (setq branches (get_children_towers_uniform curr_path))
                ;;;loop for each child...
                (loop for route in branches do
                    (cond
                        ;;;if the child is not in visited or fringe, queue it into fringe
                        ((not (or (is-in-queue visited route) (is-in-queue fringe route)))
                            (enqueue fringe route)
                            (setf node_number_tower (1+ node_number_tower))) 
                        ;;;else if the child is in the fringe and has a smaller cost, remove old node and add new one
                        ((and (is-in-queue fringe route) (>= (slot-value (get-node-from-state fringe route) 'cost) (1+ (slot-value curr_path 'cost)))) 
                            (remove-from-queue fringe (get-node-from-state fringe route))
                            (enqueue fringe route)
                            (setf node_number_tower (1+ node_number_tower))) 
                    )
                )
            )
        )
    )
    nil
)

(defmethod towers-hanoi-unifom-cost (n)
    
    ;;;initilize goal stack given the number of disk
    (setq goal_c '())
    (dotimes (i n)
        (push (1+ i) goal_c))
    (setf goal_c (reverse goal_c))

    ;;;calulate uniform-cost path
    (setq final (uniform-cost-towers goal_c))
    (setq final_path (slot-value final 'parents))
    (setf final_path (reverse final_path))

    ;;;print out path of states
    (loop for x in final_path do
        (print-state x)
    )
    (print-state final)

    ; (print '(***Node Number***))
    ; (print node_number_tower)
    ; (print '(********))

    ; (setq move_number (1+ (length final_path)))
    ; (print '(***Number of Moves***))
    ; (print move_number)
    ; (print '(********))

    

)


;;;A*

(defun tower-heuristic (n state)
    (- n (length (slot-value state 'C)))
)

; (defun tower-heuristic (n state)
;     (+ (length (slot-value state 'A)) (length (slot-value state 'B)))
; )

; (defun tower-heuristic (n state)
;     0
; )

;;;gets all possible moves based on current state
(defun get_children_towers_A* (n state)

    (setq answer '())

    ;pop off A and see where we can go 
    (setq tm1 (car (slot-value state 'A)))
    (if (and tm1 (car (slot-value state 'B)))
        (progn (cond ((< tm1 (car (slot-value state 'B)))
                    (setq p (slot-value state 'parents))
                    (push state p)
                    (push (make-instance 'towers_hanoi :cost (+ (1+ (slot-value state 'gcost)) (tower-heuristic n state)) :gcost (1+ (slot-value state 'gcost)) :A (cdr (slot-value state 'A)) :B (concatenate 'list (list tm1) (slot-value state 'B)) :C (slot-value state 'C) :parents p) answer))
        )))
    (if (and tm1 (not (car (slot-value state 'B))))
        (progn
            (setq p (slot-value state 'parents))
            (push state p)
            (push (make-instance 'towers_hanoi :cost (+ (1+ (slot-value state 'gcost)) (tower-heuristic n state)) :gcost (1+ (slot-value state 'gcost)) :A (cdr (slot-value state 'A)) :B (concatenate 'list (list tm1) (slot-value state 'B)) :C (slot-value state 'C) :parents p) answer))
    )
        
    (if (and tm1 (car (slot-value state 'C)))
        (progn (cond ((< tm1 (car (slot-value state 'C)))
                    (setq p (slot-value state 'parents))
                    (push state p)
                    (push (make-instance 'towers_hanoi :cost (+ (1+ (slot-value state 'gcost)) (tower-heuristic n state)) :gcost (1+ (slot-value state 'gcost)) :A (cdr (slot-value state 'A)) :B (slot-value state 'B) :C (concatenate 'list (list tm1) (slot-value state 'C)) :parents p) answer))
        )))
    (if (and tm1 (not (car (slot-value state 'C))))
        (progn
            (setq p (slot-value state 'parents))
            (push state p)
            (push (make-instance 'towers_hanoi :cost (+ (1+ (slot-value state 'gcost)) (tower-heuristic n state)) :gcost (1+ (slot-value state 'gcost)) :A (cdr (slot-value state 'A)) :B (slot-value state 'B) :C (concatenate 'list (list tm1) (slot-value state 'C)) :parents p) answer))
    )



    ;pop off B and see where we can go 
    (setq tm2 (car (slot-value state 'B)))
    (if (and tm2 (car (slot-value state 'A)))
        (progn (cond ((< tm2 (car (slot-value state 'A)))
                (setq p (slot-value state 'parents))
                (push state p)
                (push (make-instance 'towers_hanoi :cost (+ (1+ (slot-value state 'gcost)) (tower-heuristic n state)) :gcost (1+ (slot-value state 'gcost)) :A (concatenate 'list (list tm2) (slot-value state 'A)) :B (cdr (slot-value state 'B)) :C (slot-value state 'C) :parents p) answer))
        )))
    (if (and tm2 (not (car (slot-value state 'A))))
        (progn
            (setq p (slot-value state 'parents))
            (push state p)
            (push (make-instance 'towers_hanoi :cost (+ (1+ (slot-value state 'gcost)) (tower-heuristic n state)) :gcost (1+ (slot-value state 'gcost)) :A (concatenate 'list (list tm2) (slot-value state 'A)) :B (cdr (slot-value state 'B)) :C (slot-value state 'C) :parents p) answer))
    )


    (if (and tm2 (car (slot-value state 'C)))
        (progn (cond ((< tm2 (car (slot-value state 'C)))
                (setq p (slot-value state 'parents))
                (push state p)
                (push (make-instance 'towers_hanoi :cost (+ (1+ (slot-value state 'gcost)) (tower-heuristic n state)) :gcost (1+ (slot-value state 'gcost)) :A (slot-value state 'A) :B (cdr (slot-value state 'B)) :C (concatenate 'list (list tm2) (slot-value state 'C)) :parents p) answer))
        )))
    (if (and tm2 (not (car (slot-value state 'C))))
        (progn
            (setq p (slot-value state 'parents))
            (push state p)
            (push (make-instance 'towers_hanoi :cost (+ (1+ (slot-value state 'gcost)) (tower-heuristic n state)) :gcost (1+ (slot-value state 'gcost)) :A (slot-value state 'A) :B (cdr (slot-value state 'B)) :C (concatenate 'list (list tm2) (slot-value state 'C)) :parents p) answer))
    )

    ;pop off C and see where we can go
    (setq tm3 (car (slot-value state 'C)))
    (if (and tm3 (car (slot-value state 'A)))
        (progn (cond ((< tm3 (car (slot-value state 'A)))
                (setq p (slot-value state 'parents))
                (push state p)               
                (push (make-instance 'towers_hanoi :cost (+ (1+ (slot-value state 'gcost)) (tower-heuristic n state)) :gcost (1+ (slot-value state 'gcost)) :A (concatenate 'list (list tm3) (slot-value state 'A)) :B (slot-value state 'B) :C (cdr (slot-value state 'C)) :parents p) answer))
        )))
    (if (and tm3 (not (car (slot-value state 'A))))
        (progn
            (setq p (slot-value state 'parents))
            (push state p)
            (push (make-instance 'towers_hanoi :cost (+ (1+ (slot-value state 'gcost)) (tower-heuristic n state)) :gcost (1+ (slot-value state 'gcost)) :A (concatenate 'list (list tm3) (slot-value state 'A)) :B (slot-value state 'B) :C (cdr (slot-value state 'C)) :parents p) answer))
    )

    (if (and tm3 (car (slot-value state 'B)))
        (progn (cond ((< tm3 (car (slot-value state 'B)))
                (setq p (slot-value state 'parents))
                (push state p)
                (push (make-instance 'towers_hanoi :cost (+ (1+ (slot-value state 'gcost)) (tower-heuristic n state)) :gcost (1+ (slot-value state 'gcost)) :A (slot-value state 'A) :B (concatenate 'list (list tm3) (slot-value state 'B)) :C (cdr (slot-value state 'C)) :parents p) answer))
        )))
    (if (and tm3 (not (car (slot-value state 'B))))
        (progn
            (setq p (slot-value state 'parents))
            (push state p)
            (push (make-instance 'towers_hanoi :cost (+ (1+ (slot-value state 'gcost)) (tower-heuristic n state)) :gcost (1+ (slot-value state 'gcost)) :A (slot-value state 'A) :B (concatenate 'list (list tm3) (slot-value state 'B)) :C (cdr (slot-value state 'C)) :parents p) answer))
    )

    ;;;return list of all possible child states
    answer

)

(defun A*-cost-towers (n goal)
    ;;;made visited a queue to use custum 'is-in-queue' function
    (setq visited (make-instance 'pqueue-towers :compare-function #'cmp-towers_hanoi))
    (setq parents '()) ;;; list of all previous states
    (setq node_number_tower 0)
    ;;;setup the fringe priority queue and queue the start state with cost 0
    (setq fringe (make-instance 'pqueue-towers :compare-function #'cmp-towers_hanoi))
    (enqueue fringe (make-instance 'towers_hanoi :cost 0 :A goal :parents parents))
    (setf node_number_tower (1+ node_number_tower))
    ;;;loop while there is elements in fringe
    (loop while (not (is-empty fringe)) do
        ;;;dequeue priority node
        (setq curr_path (dequeue fringe))
        (cond 
            ;;;if priority node's state is the end state, return current state and exit
            ((equal (slot-value curr_path 'C) goal) (return-from A*-cost-towers curr_path))
            (t
                 ;;;if not, mark state as visited and get all children of state
                (enqueue visited curr_path)
                (setq branches (get_children_towers_A* n curr_path))
                ;;;loop for each child...
                (loop for route in branches do
                    (cond
                        ;;;if the child is not in visited or fringe, queue it into fringe
                        ((not (or (is-in-queue visited route) (is-in-queue fringe route)))
                            (enqueue fringe route)
                            (setf node_number_tower (1+ node_number_tower))) 
                        ;;;else if the child is in the fringe and has a smaller cost, remove old node and add new one
                        ((and (is-in-queue fringe route) (>= (slot-value (get-node-from-state fringe route) 'cost) (1+ (slot-value curr_path 'cost)))) 
                            (remove-from-queue fringe (get-node-from-state fringe route))
                            (enqueue fringe route)
                            (setf node_number_tower (1+ node_number_tower))) 
                    )
                )
            )
        )
    )
    nil
)




(defmethod towers-hanoi-A*-cost (n)
    
    ;;;initilize goal stack given the number of disk
    (setq goal_c '())
    (dotimes (i n)
        (push (1+ i) goal_c))
    (setf goal_c (reverse goal_c))

    ;;;calulate uniform-cost path
    (setq final (A*-cost-towers n goal_c))
    (setq final_path (slot-value final 'parents))
    (setf final_path (reverse final_path))

    ;;;print out path of states
    (loop for x in final_path do
        (print-state x)
    )
    (print-state final)

    ; (print '(***Node Number***))
    ; (print node_number_tower)
    ; (print '(********))

    ; (setq move_number (1+ (length final_path)))
    ; (print '(***Number of Moves***))
    ; (print move_number)
    ; (print '(********))
    
)