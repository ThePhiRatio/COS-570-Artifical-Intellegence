;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This document contains all the agent programs
;;;
;;; Author: Nicholas Soucy
;;;
;;; COS 570 F2021
;;;

;;;Global Variables for Testing


;;;Functions
(defun man_calc (loc_a loc_b)
  "Caclucates the manhattan distance between two locations"
  (+ (abs (- (car loc_a) (car loc_b))) (abs (- (cadr loc_a) (cadr loc_b)))))

(defun eu_calc (loc_a loc_b)
  "Caclucates the euclidean distance between two locations"
  (sqrt (+ (* (- (car loc_b) (car loc_a)) (- (car loc_b) (car loc_a))) (* (- (cadr loc_b) (cadr loc_a)) (- (cadr loc_b) (cadr loc_a))))))

;;;1) Simple Reflex Agent
(defclass reflex-agent (robot) ((steps :initform 0)))

(defmethod agent-program ((self reflex-agent) percept)
  (with-slots (name location steps) self
    (setf steps (1+ steps))
    (if (or (equal location '(1 1)) (equal location '(1 25)) (equal location '(25 1)) (equal location '(25 25)))
        (setf steps (1- steps))
    )
    ;;;if there is something in front of us, go right, else, go forward
    (if (cadr (assoc :forward-sensor percept)) 
        :right
        :forward)
    

  )
)


;;;2) Model-Based Agent
(defclass model-agent (robot)
  ((action :initform nil) ;;;previous action
   (action-stack :initform '(0))
   (back-stack :initform nil)
   (previous :initform nil)
   (steps :initform 0))) ;;;previous action stack

(defmethod agent-program ((self model-agent) percept)
    (with-slots (name previous action-stack back-stack steps) self
        (setq action nil)
        (setf steps (1+ steps))
        (cond
            ;;;if we are at 'true' corner, do :nop
            ((and action-stack (eql (car action-stack) :nop)) 
                (setf steps (1- steps))
                (setf action :nop)) 
            ;;;if there is something on the action-stack, perform it
            (action-stack 
                (setf action (pop action-stack)) 
                (setq previous action))


            ;;;If there isn't something in front of us and something is behind us and our previous action is backward, check left
            ((and (not (cadr (assoc :forward-sensor percept))) (eql previous :backward) (cadr(assoc :rear-bump percept)) (not back-stack)) 
                (setf action :forward) 
                (setq previous action) 
                (push :backward back-stack) 
                (push :left action-stack))
            ;;;if there isn't something behind us or in front of us and our previous action is backward, go forward.
            ((and (not (cadr (assoc :forward-sensor percept))) (eql previous :backward) (not(cadr(assoc :rear-bump percept))) (not back-stack)) 
                (setf action :forward) 
                (setq previous action)) ;no corner on left

            
            ;;;If there is something on the backstack, but nothing to our left, clear back-stack as we found false corner
            ((and back-stack (not(cadr(assoc :left-bump percept)))) 
                (setf action :nop) 
                (setq previous action) 
                (setq back-stack nil))


            ;;;If there is something on the backstack and to our left, but not in front, go forward as corner is currently valid
            ((and (not (cadr (assoc :forward-sensor percept))) back-stack (cadr(assoc :left-bump percept))) 
                (setf action :forward) 
                (setq previous action) 
                (push :backward back-stack) 
                (push :left action-stack))
            
            ;;;Then check one last time to see if there is something on the back stack, and to the front and left of us.
            ;;;we then load back-stack into action-stack to back track to true corner
            ((and (cadr (assoc :forward-sensor percept)) back-stack (cadr(assoc :left-bump percept))) 
                (push :nop action-stack) 
                (loop for i in back-stack do 
                    (push (pop back-stack) action-stack)) 
                (setf action :nop) 
                (setq previous action))
            
            ;;;if there is something in front of us, turn-right
            ((cadr (assoc :forward-sensor percept)) 
                (setf action :turn-right) 
                (setq previous action) 
                (push :backward action-stack))

            ;;;if there isn't something in front of us, go forward.
            ((not (cadr (assoc :forward-sensor percept))) 
                (setf action :forward) 
                (setq previous action))
        )
    action
    )
)



;;;3) Reflex Hill-Climbing Agent
(defclass reflex-hill-agent (robot)
  ((action :initform nil) ;;;previous action
   (goal_location :initform '(25 25) :initarg :goal)
   ;;;Heuristic values for each square the robot can be at
   (f :initform nil)  
   (b :initform nil)
   (l :initform nil)
   (r :initform nil)
   (c :initform nil)
   (steps :initform 0)))


(defmethod reflex-hill-heuristic ((self reflex-hill-agent) direction)
  (with-slots (location orientation goal_location) self
    (setq final nil)

      (cond
          ((eql orientation :north) 

              (cond
                  ((eql direction :forward) (setf final (man_calc (list (car location) (+ (cadr location) 1)) goal_location)))

                  ((eql direction :backward) (setf final (man_calc (list (car location) (- (cadr location) 1)) goal_location)))

                  ((eql direction :left) (setf final (man_calc (list (- (car location) 1) (cadr location)) goal_location)))

                  ((eql direction :right) (setf final (man_calc (list (+ (car location) 1) (cadr location)) goal_location)))

                  ((eql direction :current) (setf final (man_calc location goal_location)))

                  (t (print '(What are we in?! The fourth dimension?!)))
              )
          )
        
          ((eql orientation :south)

              (cond
                  ((eql direction :forward) (setf final (man_calc (list (car location) (- (cadr location) 1)) goal_location)))

                  ((eql direction :backward) (setf final (man_calc (list (car location) (+ (cadr location) 1)) goal_location)))

                  ((eql direction :left) (setf final (man_calc (list (+ (car location) 1) (cadr location)) goal_location)))

                  ((eql direction :right) (setf final (man_calc (list (- (car location) 1) (cadr location)) goal_location)))

                  ((eql direction :current) (setf final (man_calc location goal_location)))

                  (t (print '(What are we in?! The fourth dimension?!)))
              )
          )

          ((eql orientation :west)

              (cond
                  ((eql direction :forward) (setf final (man_calc (list (- (car location) 1) (cadr location)) goal_location)))

                  ((eql direction :backward) (setf final (man_calc (list (+ (car location) 1) (cadr location)) goal_location)))

                  ((eql direction :left) (setf final (man_calc (list (car location) (- (cadr location) 1)) goal_location)))

                  ((eql direction :right) (setf final (man_calc (list (car location) (+ (cadr location) 1)) goal_location)))

                  ((eql direction :current) (setf final (man_calc location goal_location)))

                  (t (print '(What are we in?! The fourth dimension?!)))
              )
          )


          ((eql orientation :east)

              (cond
                  ((eql direction :forward) (setf final (man_calc (list (+ (car location) 1) (cadr location)) goal_location)))

                  ((eql direction :backward) (setf final (man_calc (list (- (car location) 1) (cadr location)) goal_location)))

                  ((eql direction :left) (setf final (man_calc (list (car location) (+ (cadr location) 1)) goal_location)))

                  ((eql direction :right) (setf final (man_calc (list (car location) (- (cadr location) 1)) goal_location)))

                  ((eql direction :current) (setf final (man_calc location goal_location)))

                  (t (print '(What are we in?! The fourth dimension?!)))
              )
          )
          
          (t (print '(What are we in?! The fourth dimension?!)))
          
      )
    final
  )
)

(defmethod agent-program ((self reflex-hill-agent) percept)
  (with-slots (name action f b l r c steps) self
    (setf steps (1+ steps))
    ;;;get heuristic values
    (setf c (reflex-hill-heuristic self :current))
    (setf f (reflex-hill-heuristic self :forward))
    (setf b (reflex-hill-heuristic self :backward))
    (setf l (reflex-hill-heuristic self :left))
    (setf r (reflex-hill-heuristic self :right))

    ;;;find minimum cost direction
    (setf tmp (min c f b l r))

    ;;;go in that direction
    (cond
        ((eql tmp c) (setf action :nop) (setf steps (1- steps)))
        ((eql tmp f) (setf action :forward))
        ((eql tmp b) (setf action :backward))
        ((eql tmp l) (setf action :left))
        ((eql tmp r) (setf action :right))
        (t (print '(That is quite impossible good sir!))))

    action

  )
)


;;;4) Uniform-Cost Search Agent
(defclass uniform-agent (robot)
  ((action :initform nil) ;;;previous action
   (goal_location :initform '() :initarg :goal)
   (action_stack :initform '()) ;;;way to keep track of all actions we took to get to a node
   (obstacle-locations :initform '() :initarg :ob_loc)
   (world_size :initform '() :initarg :world_size)
   (is_done :initform nil)
   (node_number :initform 0))
)

;;;get children gets all possible locations we can go to based on the current location we are at
(defmethod get_children ((self uniform-agent) location)
  (with-slots (obstacle-locations orientation world_size) self
    (setq answer '())

      (cond
          ((eql orientation :north) 

              (if (and (not (member (list (car location) (+ (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (+ (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (+ (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (+ (cadr location) 1)) :forward) answer))

              (if (and (not (member (list (car location) (- (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (- (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (- (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (- (cadr location) 1)) :backward) answer))

              (if (and (not (member (list (- (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (- (car location) 1) 0) (> (cadr location) 0)) (and (<= (- (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (- (car location) 1) (cadr location)) :left) answer))

              (if (and (not (member (list (+ (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (+ (car location) 1) 0) (> (cadr location) 0)) (and (<= (+ (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (+ (car location) 1) (cadr location)) :right) answer))


          )
        
          ((eql orientation :south)

              (if (and (not (member (list (car location) (- (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (- (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (- (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (- (cadr location) 1)) :forward) answer))

              (if (and (not (member (list (car location) (+ (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (+ (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (+ (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (+ (cadr location) 1)) :backward) answer))

              (if (and (not (member (list (+ (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (+ (car location) 1) 0) (> (cadr location) 0)) (and (<= (+ (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (+ (car location) 1) (cadr location)) :left) answer))

              (if (and (not (member (list (- (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (- (car location) 1) 0) (> (cadr location) 0)) (and (<= (- (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (- (car location) 1) (cadr location)) :right) answer))


          )

          ((eql orientation :west)

              (if (and (not (member (list (- (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (- (car location) 1) 0) (> (cadr location) 0)) (and (<= (- (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (- (car location) 1) (cadr location)) :forward) answer))

              (if (and (not (member (list (+ (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (+ (car location) 1) 0) (> (cadr location) 0)) (and (<= (+ (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (+ (car location) 1) (cadr location)) :backward) answer))

              (if (and (not (member (list (car location) (- (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (- (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (- (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (- (cadr location) 1)) :left) answer))

              (if (and (not (member (list (car location) (+ (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (+ (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (+ (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (+ (cadr location) 1)) :right) answer))

          )


          ((eql orientation :east)

              (if (and (not (member (list (+ (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (+ (car location) 1) 0) (> (cadr location) 0)) (and (<= (+ (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (+ (car location) 1) (cadr location)) :forward) answer))

              (if (and (not (member (list (- (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (- (car location) 1) 0) (> (cadr location) 0)) (and (<= (- (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (- (car location) 1) (cadr location)) :backward) answer))

              (if (and (not (member (list (car location) (+ (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (+ (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (+ (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (+ (cadr location) 1)) :left) answer))

              (if (and (not (member (list (car location) (- (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (- (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (- (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (- (cadr location) 1)) :right) answer))

          )
          
          (t (print '(What are we in?! The fourth dimension?!)))
          
      )
    ;;;returns a list where the first element is the coordinates and the second is the action it took to get there
    answer
  )
)


(defmethod uniform-cost ((self uniform-agent))
  (with-slots (location goal_location node_number) self
      (setq visited '())
      (setq actions '())
      ;;;setup the fringe priority queue and queue the start node with cost 0
      (setq fringe (make-instance 'priority-queue :compare-function #'cmp-nodes))
      (enqueue fringe (make-instance 'sample-node :cost 0 :coord location :actions actions))
      (setf node_number (1+ node_number))
      ;;;loop while there is elements in fringe
      (loop while (not (is-empty fringe)) do
          ;;;dequeue priority node
          (setq curr_path (dequeue fringe))
          ;;;holds all actions it took to get to node
          (setq actions (slot-value curr_path 'actions))
          (cond 
                ;;;if priority node's location is the goal, return all actions it took to get there and exit
                ((equal (slot-value curr_path 'coord) goal_location) (return-from uniform-cost actions))
                (t
                    ;;;if not, mark node as visited and get all children of node
                    (push (slot-value curr_path 'coord) visited)
                    (setq branches (get_children self (slot-value curr_path 'coord)))
                    ;;;loop for each child...
                    (loop for route in branches do

                        (cond
                            ;;;if the child is not in visited or fringe, queue it into fringe
                            ((not (or (member (car route) visited :test #'equal) (is-in-queue fringe (car route))))
                              (setq moves actions)
                              (push (cadr route) moves)
                              (enqueue fringe (make-instance 'sample-node :cost (1+ (slot-value curr_path 'cost)) :coord (car route) :actions moves))
                              (setf node_number (1+ node_number))) 
                            ;;;else if the child is in the fringe and has a smaller cost, remove old node and add new one
                            ((and (is-in-queue fringe (car route)) (>= (slot-value (get-node-from-location fringe (car route)) 'cost) (1+ (slot-value curr_path 'cost)))) 
                              (remove-from-queue fringe (get-node-from-location fringe (car route)))
                              (setq moves actions)
                              (push (cadr route) moves)
                              (enqueue fringe (make-instance 'sample-node :cost (1+ (slot-value curr_path 'cost)) :coord (car route) :actions moves))
                              (setf node_number (1+ node_number))) 
                        )
                    )
                )
          )
      )
      ;;;return nil list if no path exist
      actions
  )
)



(defmethod agent-program ((self uniform-agent) percept)
  (with-slots (name action_stack action is_done) self

    ;;;check if we have calculated path
    (cond
      ((not is_done) 
        (setf action_stack (uniform-cost self))
        (setf action_stack (reverse action_stack))
        (setq tot_cost (length action_stack)))
    )
    (setf is_done 1)

    ;;;do pop'ed action off action_stack until at goal, then do :nop
    (setf action (pop action_stack))

    (if (not action)
        (setf action :nop))

    ; (print '(***Node Number***))
    ; (print node_number)
    ; (print '(********))

    ; (print '(***Total Path Cost***))
    ; (print tot_cost)
    ; (print '(********))


    action

  )
)

;;;5a) A* Heuristic Function 1 Agent
(defclass A*1-agent (robot) 
  ((world_size :initform '() :initarg :world_size)
   (obstacle-locations :initform '() :initarg :ob_loc)
   (action :initform nil)
   (goal_location :initform '() :initarg :goal)
   (action_stack :initform '()) ;;;way to keep track of all actions we took to get to a node
   (is_done :initform nil)
   (node_number :initform 0))
)

;;;get children gets all possible locations we can go to based on the current location we are at
(defmethod get_children ((self A*1-agent) location)
  (with-slots (obstacle-locations orientation world_size) self
    (setq answer '())

      (cond
          ((eql orientation :north) 

              (if (and (not (member (list (car location) (+ (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (+ (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (+ (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (+ (cadr location) 1)) :forward) answer))

              (if (and (not (member (list (car location) (- (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (- (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (- (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (- (cadr location) 1)) :backward) answer))

              (if (and (not (member (list (- (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (- (car location) 1) 0) (> (cadr location) 0)) (and (<= (- (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (- (car location) 1) (cadr location)) :left) answer))

              (if (and (not (member (list (+ (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (+ (car location) 1) 0) (> (cadr location) 0)) (and (<= (+ (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (+ (car location) 1) (cadr location)) :right) answer))


          )
        
          ((eql orientation :south)

              (if (and (not (member (list (car location) (- (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (- (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (- (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (- (cadr location) 1)) :forward) answer))

              (if (and (not (member (list (car location) (+ (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (+ (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (+ (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (+ (cadr location) 1)) :backward) answer))

              (if (and (not (member (list (+ (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (+ (car location) 1) 0) (> (cadr location) 0)) (and (<= (+ (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (+ (car location) 1) (cadr location)) :left) answer))

              (if (and (not (member (list (- (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (- (car location) 1) 0) (> (cadr location) 0)) (and (<= (- (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (- (car location) 1) (cadr location)) :right) answer))


          )

          ((eql orientation :west)

              (if (and (not (member (list (- (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (- (car location) 1) 0) (> (cadr location) 0)) (and (<= (- (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (- (car location) 1) (cadr location)) :forward) answer))

              (if (and (not (member (list (+ (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (+ (car location) 1) 0) (> (cadr location) 0)) (and (<= (+ (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (+ (car location) 1) (cadr location)) :backward) answer))

              (if (and (not (member (list (car location) (- (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (- (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (- (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (- (cadr location) 1)) :left) answer))

              (if (and (not (member (list (car location) (+ (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (+ (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (+ (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (+ (cadr location) 1)) :right) answer))

          )


          ((eql orientation :east)

              (if (and (not (member (list (+ (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (+ (car location) 1) 0) (> (cadr location) 0)) (and (<= (+ (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (+ (car location) 1) (cadr location)) :forward) answer))

              (if (and (not (member (list (- (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (- (car location) 1) 0) (> (cadr location) 0)) (and (<= (- (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (- (car location) 1) (cadr location)) :backward) answer))

              (if (and (not (member (list (car location) (+ (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (+ (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (+ (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (+ (cadr location) 1)) :left) answer))

              (if (and (not (member (list (car location) (- (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (- (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (- (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (- (cadr location) 1)) :right) answer))

          )
          
          (t (print '(What are we in?! The fourth dimension?!)))
          
      )
    ;;;returns a list where the first element is the coordinates and the second is the action it took to get there
    answer
  )
)


(defmethod A*1-cost ((self A*1-agent))
  (with-slots (location goal_location node_number) self
      (setq closed '())
      (setq actions '())
      ;;;setup the open priority queue and queue the start node with cost manhattan distance
      (setq open_q (make-instance 'priority-queue :compare-function #'cmp-nodes))
      (enqueue open_q (make-instance 'sample-node :cost (man_calc location goal_location) :hcost (man_calc location goal_location) :gcost 0 :coord location :actions actions))
      (setf node_number (1+ node_number))
      ;;;loop while there is elements in open queue
      (loop while (not (is-empty open_q)) do
          ;;;dequeue priority node
          (setq node_current (dequeue open_q))
          ;;;holds all actions it took to get to node
          (setq actions (slot-value node_current 'actions))
          (cond 
                ;;;if priority node's location is the goal, return all actions it took to get there and exit
                ((equal (slot-value node_current 'coord) goal_location) (return-from A*1-cost actions))
                (t
                    ;;;if not, mark node as closed and get all children of node
                    (push (slot-value node_current 'coord) closed)
                    (setq branches (get_children self (slot-value node_current 'coord)))
                    ;;;loop for each child...
                    (loop for node_successor in branches do

                        (cond
                            ;;;if the child is not in closed or open, queue it into open
                            ((not (or (member (car node_successor) closed :test #'equal) (is-in-queue open_q (car node_successor))))
                              (setq moves actions)
                              (push (cadr node_successor) moves)
                              (setq full_cost (+ (man_calc (car node_successor) goal_location) (1+ (slot-value node_current 'gcost))))
                              (enqueue open_q (make-instance 'sample-node :cost full_cost :hcost (man_calc (car node_successor) goal_location) :gcost (1+ (slot-value node_current 'gcost)) :coord (car node_successor) :actions moves))
                              (setf node_number (1+ node_number)))                       
                            ;;;else if the child is in open and has a smaller cost, remove old node and add new one
                            ((and (is-in-queue open_q (car node_successor)) (>= (slot-value (get-node-from-location open_q (car node_successor)) 'cost) (1+ (slot-value node_current 'cost)))) 
                              (remove-from-queue open_q (get-node-from-location open_q (car node_successor)))
                              (setq moves actions)
                              (push (cadr node_successor) moves)
                              (setq full_cost (+ (man_calc (car node_successor) goal_location) (1+ (slot-value node_current 'gcost))))
                              (enqueue open_q (make-instance 'sample-node :cost full_cost :hcost (man_calc (car node_successor) goal_location) :gcost (1+ (slot-value node_current 'gcost)) :coord (car node_successor) :actions moves))
                              (setf node_number (1+ node_number)))                       
                        )
                    )
                )
          )
      )
      ;;;return nil list if no path exist
      actions
  )
)



(defmethod agent-program ((self A*1-agent) percept)
  (with-slots (name action_stack is_done location) self

    ;;;check if we have calculated path
    (cond
      ((not is_done) 
        (setf action_stack (A*1-cost self))
        (setf action_stack (reverse action_stack))
        (setq tot_cost (length action_stack)))
    )
    (setf is_done 1)

    ;;;do pop'ed action off action_stack until at goal, then do :nop
    (setf action (pop action_stack))

    (if (not action)
        (setf action :nop))

    ; (print '(***Node Number***))
    ; (print node_number)
    ; (print '(********))

    ; (print '(***Total Path Cost***))
    ; (print tot_cost)
    ; (print '(********))

    action

  )
)


;;;5b) A* Heuristic Function 2 Agent
(defclass A*2-agent (robot) 
  ((world_size :initform '() :initarg :world_size)
   (obstacle-locations :initform '() :initarg :ob_loc)
   (action :initform nil)
   (goal_location :initform '() :initarg :goal)
   (action_stack :initform '()) ;;;way to keep track of all actions we took to get to a node
   (is_done :initform nil)
   (node_number :initform 0))
)

;;;get children gets all possible locations we can go to based on the current location we are at
(defmethod get_children ((self A*2-agent) location)
  (with-slots (obstacle-locations orientation world_size) self
    (setq answer '())

      (cond
          ((eql orientation :north) 

              (if (and (not (member (list (car location) (+ (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (+ (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (+ (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (+ (cadr location) 1)) :forward) answer))

              (if (and (not (member (list (car location) (- (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (- (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (- (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (- (cadr location) 1)) :backward) answer))

              (if (and (not (member (list (- (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (- (car location) 1) 0) (> (cadr location) 0)) (and (<= (- (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (- (car location) 1) (cadr location)) :left) answer))

              (if (and (not (member (list (+ (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (+ (car location) 1) 0) (> (cadr location) 0)) (and (<= (+ (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (+ (car location) 1) (cadr location)) :right) answer))


          )
        
          ((eql orientation :south)

              (if (and (not (member (list (car location) (- (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (- (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (- (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (- (cadr location) 1)) :forward) answer))

              (if (and (not (member (list (car location) (+ (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (+ (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (+ (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (+ (cadr location) 1)) :backward) answer))

              (if (and (not (member (list (+ (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (+ (car location) 1) 0) (> (cadr location) 0)) (and (<= (+ (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (+ (car location) 1) (cadr location)) :left) answer))

              (if (and (not (member (list (- (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (- (car location) 1) 0) (> (cadr location) 0)) (and (<= (- (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (- (car location) 1) (cadr location)) :right) answer))


          )

          ((eql orientation :west)

              (if (and (not (member (list (- (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (- (car location) 1) 0) (> (cadr location) 0)) (and (<= (- (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (- (car location) 1) (cadr location)) :forward) answer))

              (if (and (not (member (list (+ (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (+ (car location) 1) 0) (> (cadr location) 0)) (and (<= (+ (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (+ (car location) 1) (cadr location)) :backward) answer))

              (if (and (not (member (list (car location) (- (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (- (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (- (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (- (cadr location) 1)) :left) answer))

              (if (and (not (member (list (car location) (+ (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (+ (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (+ (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (+ (cadr location) 1)) :right) answer))

          )


          ((eql orientation :east)

              (if (and (not (member (list (+ (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (+ (car location) 1) 0) (> (cadr location) 0)) (and (<= (+ (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (+ (car location) 1) (cadr location)) :forward) answer))

              (if (and (not (member (list (- (car location) 1) (cadr location)) obstacle-locations :test #'equal)) (and (> (- (car location) 1) 0) (> (cadr location) 0)) (and (<= (- (car location) 1) (car world_size)) (<= (cadr location) (cadr world_size))))
                  (push (list (list (- (car location) 1) (cadr location)) :backward) answer))

              (if (and (not (member (list (car location) (+ (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (+ (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (+ (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (+ (cadr location) 1)) :left) answer))

              (if (and (not (member (list (car location) (- (cadr location) 1)) obstacle-locations :test #'equal)) (and (> (car location) 0) (> (- (cadr location) 1) 0)) (and (<= (car location) (car world_size)) (<= (- (cadr location) 1) (cadr world_size))))
                  (push (list (list (car location) (- (cadr location) 1)) :right) answer))

          )
          
          (t (print '(What are we in?! The fourth dimension?!)))
          
      )
    ;;;returns a list where the first element is the coordinates and the second is the action it took to get there
    answer
  )
)


(defmethod A*2-cost ((self A*2-agent))
  (with-slots (location goal_location node_number) self
      (setq closed '())
      (setq actions '())
      ;;;setup the open priority queue and queue the start node with cost euclidean distance
      (setq open_q (make-instance 'priority-queue :compare-function #'cmp-nodes))
      (enqueue open_q (make-instance 'sample-node :cost (eu_calc location goal_location) :hcost (eu_calc location goal_location) :gcost 0 :coord location :actions actions))
      (setf node_number (1+ node_number))
      ;;;loop while there is elements in open queue
      (loop while (not (is-empty open_q)) do
          ;;;dequeue priority node
          (setq node_current (dequeue open_q))
          ;;;holds all actions it took to get to node
          (setq actions (slot-value node_current 'actions))
          (cond 
                ;;;if priority node's location is the goal, return all actions it took to get there and exit
                ((equal (slot-value node_current 'coord) goal_location) (return-from A*2-cost actions))
                (t
                    ;;;if not, mark node as closed and get all children of node
                    (push (slot-value node_current 'coord) closed)
                    (setq branches (get_children self (slot-value node_current 'coord)))
                    ;;;loop for each child...
                    (loop for node_successor in branches do

                        (cond
                            ;;;if the child is not in closed or open, queue it into open
                            ((not (or (member (car node_successor) closed :test #'equal) (is-in-queue open_q (car node_successor))))
                              (setq moves actions)
                              (push (cadr node_successor) moves)
                              (setq full_cost (+ (eu_calc (car node_successor) goal_location) (1+ (slot-value node_current 'gcost))))
                              (enqueue open_q (make-instance 'sample-node :cost full_cost :hcost (eu_calc (car node_successor) goal_location) :gcost (1+ (slot-value node_current 'gcost)) :coord (car node_successor) :actions moves))                      
                              (setf node_number (1+ node_number))
                            )
                            ;;;else if the child is in open and has a smaller cost, remove old node and add new one
                            ((and (is-in-queue open_q (car node_successor)) (>= (slot-value (get-node-from-location open_q (car node_successor)) 'cost) (1+ (slot-value node_current 'cost)))) 
                              (remove-from-queue open_q (get-node-from-location open_q (car node_successor)))
                              (setq moves actions)
                              (push (cadr node_successor) moves)
                              (setq full_cost (+ (eu_calc (car node_successor) goal_location) (1+ (slot-value node_current 'gcost))))
                              (enqueue open_q (make-instance 'sample-node :cost full_cost :hcost (eu_calc (car node_successor) goal_location) :gcost (1+ (slot-value node_current 'gcost)) :coord (car node_successor) :actions moves))   
                              (setf node_number (1+ node_number))                    
                            )                        
                        )
                    )
                )
          )
      )
      ;;;return nil list if no path exist
      actions
  )
)



(defmethod agent-program ((self A*2-agent) percept)
  (with-slots (name action_stack is_done location) self

    ;;;check if we have calculated path
    (cond
      ((not is_done) 
        (setf action_stack (A*2-cost self))
        (setf action_stack (reverse action_stack))
        (setq tot_cost (length action_stack)))
    )
    (setf is_done 1)

    ;;;do pop'ed action off action_stack until at goal, then do :nop
    (setf action (pop action_stack))

    (if (not action)
        (setf action :nop))


    ; (print '(***Node Number***))
    ; (print node_number)
    ; (print '(********))

    ; (print '(***Total Cost***))
    ; (print tot_cost)
    ; (print '(********))



    action

  )
)