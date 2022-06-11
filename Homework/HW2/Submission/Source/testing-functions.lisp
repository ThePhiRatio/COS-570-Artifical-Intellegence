;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This document defines all
;;; agent tests based on
;;; homework specification
;;;
;;; Author: Nicholas Soucy
;;;
;;; COS 570 F2021
;;;

(defun test_reflex ()

    (setq steps_list '())

    (dotimes (i 20)
        ;;;World 1: No obsticles
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                    :num-obstacles 0
                                    ))
        (setq r2 (make-instance 'reflex-agent))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'steps)) steps_list)
    )
    
    (setq steps_list2 '())
    (dotimes (i 20)
        ;;;World 2: Obstacles, but no false corners
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                :num-obstacles 0
                                :obstacle-locations '((2 2) (4 4) (6 6) (8 8) (10 10) (12 12) (14 14) (16 16) (18 18) (20 20) (22 22) (24 24) 
                                (2 24) (4 22) (6 20) (8 18) (10 16) (12 14) (14 12) (16 10) (18 8) (20 6) (22 4) (24 2)
                                (2 13) (4 13) (6 13) (8 13) (18 13) (20 13) (22 13) (24 13))
                                ))
        (setq r2 (make-instance 'reflex-agent))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'steps)) steps_list2)
    )

    (setq steps_list3 '())
    (dotimes (i 20)
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                :num-obstacles 45
                                :obstacle-locations '((2 4) (3 4) (4 4) (2 3) (2 2))
                                ))
        (setq r2 (make-instance 'reflex-agent))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'steps)) steps_list3)
    )

    (print '(***Steps list 1***))
    (print steps_list)
    (print '(*********))
    (print '(***Steps list 2***))
    (print steps_list2)
    (print '(*********))
    (print '(***Steps list 3***))
    (print steps_list3)
    (print '(*********))
)

(defun test_model ()
    (setq steps_list '())

    (dotimes (i 20)
        ;;;World 1: No obsticles
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                    :num-obstacles 0
                                    ))
        (setq r2 (make-instance 'model-agent))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'steps)) steps_list)
    )
    
    (setq steps_list2 '())
    (dotimes (i 20)
        ;;;World 2: Obstacles, but no false corners
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                :num-obstacles 0
                                :obstacle-locations '((2 2) (4 4) (6 6) (8 8) (10 10) (12 12) (14 14) (16 16) (18 18) (20 20) (22 22) (24 24) 
                                (2 24) (4 22) (6 20) (8 18) (10 16) (12 14) (14 12) (16 10) (18 8) (20 6) (22 4) (24 2)
                                (2 13) (4 13) (6 13) (8 13) (18 13) (20 13) (22 13) (24 13))
                                ))
        (setq r2 (make-instance 'model-agent))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'steps)) steps_list2)
    )

    (setq steps_list3 '())
    (dotimes (i 20)
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                :num-obstacles 45
                                :obstacle-locations '((2 4) (3 4) (4 4) (2 3) (2 2))
                                ))
        (setq r2 (make-instance 'model-agent))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'steps)) steps_list3)
    )

    (print '(***Steps list 1***))
    (print steps_list)
    (print '(*********))
    (print '(***Steps list 2***))
    (print steps_list2)
    (print '(*********))
    (print '(***Steps list 3***))
    (print steps_list3)
    (print '(*********))
)

(defun test_hill ()
    (setq steps_list '())

    (dotimes (i 20)
        ;;;World 1: No obsticles
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                    :num-obstacles 0
                                    ))
        (setq r2 (make-instance 'reflex-hill-agent))
        (add-robot sim :robot r2 )
        (run sim :for 1000)
        (push (print (slot-value r2 'steps)) steps_list)
    )
    
    (setq steps_list2 '())
    (dotimes (i 20)
        ;;;World 2: Obstacles, but no false corners
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                :num-obstacles 0
                                :obstacle-locations '((2 2) (4 4) (6 6) (8 8) (10 10) (12 12) (14 14) (16 16) (18 18) (20 20) (22 22) (24 24) 
                                (2 24) (4 22) (6 20) (8 18) (10 16) (12 14) (14 12) (16 10) (18 8) (20 6) (22 4) (24 2)
                                (2 13) (4 13) (6 13) (8 13) (18 13) (20 13) (22 13) (24 13))
                                ))
        (setq r2 (make-instance 'reflex-hill-agent))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'steps)) steps_list2)
    )

    (setq steps_list3 '())
    (dotimes (i 20)
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                :num-obstacles 45
                                :obstacle-locations '((2 4) (3 4) (4 4) (2 3) (2 2))
                                ))
        (setq r2 (make-instance 'reflex-hill-agent))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'steps)) steps_list3)
    )

    (print '(***Steps list 1***))
    (print steps_list)
    (print '(*********))
    (print '(***Steps list 2***))
    (print steps_list2)
    (print '(*********))
    (print '(***Steps list 3***))
    (print steps_list3)
    (print '(*********))
)

(defun test_uniform ()
    (setq node_number1 '())

    (dotimes (i 20)
        ;;;World 1: No obsticles
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                    :num-obstacles 0
                                    ))
        (setq r2 (make-instance 'uniform-agent :goal '(25 25) :ob_loc (object-locations (slot-value sim 'world)) :world_size (slot-value (slot-value sim 'world) 'size)))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'node_number)) node_number1)
    )
    
    (setq node_number2 '())
    (dotimes (i 20)
        ;;;World 2: Obstacles, but no false corners
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                :num-obstacles 0
                                :obstacle-locations '((2 2) (4 4) (6 6) (8 8) (10 10) (12 12) (14 14) (16 16) (18 18) (20 20) (22 22) (24 24) 
                                (2 24) (4 22) (6 20) (8 18) (10 16) (12 14) (14 12) (16 10) (18 8) (20 6) (22 4) (24 2)
                                (2 13) (4 13) (6 13) (8 13) (18 13) (20 13) (22 13) (24 13))
                                ))
        (setq r2 (make-instance 'uniform-agent :goal '(25 25) :ob_loc (object-locations (slot-value sim 'world)) :world_size (slot-value (slot-value sim 'world) 'size)))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'node_number)) node_number2)
    )

    (setq node_number3 '())
    (dotimes (i 20)
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                :num-obstacles 45
                                :obstacle-locations '((2 4) (3 4) (4 4) (2 3) (2 2))
                                ))
        (setq r2 (make-instance 'uniform-agent :goal '(25 25) :ob_loc (object-locations (slot-value sim 'world)) :world_size (slot-value (slot-value sim 'world) 'size)))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'node_number)) node_number3)
    )

    (print '(***Steps list 1***))
    (print node_number1)
    (print '(*********))
    (print '(***Steps list 2***))
    (print node_number2)
    (print '(*********))
    (print '(***Steps list 3***))
    (print node_number3)
    (print '(*********))
)

(defun test_A*1 ()
    (setq node_number1 '())

    (dotimes (i 20)
        ;;;World 1: No obsticles
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                    :num-obstacles 0
                                    ))
        (setq r2 (make-instance 'A*1-agent :goal '(25 25) :ob_loc (object-locations (slot-value sim 'world)) :world_size (slot-value (slot-value sim 'world) 'size)))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'node_number)) node_number1)
    )
    
    (setq node_number2 '())
    (dotimes (i 20)
        ;;;World 2: Obstacles, but no false corners
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                :num-obstacles 0
                                :obstacle-locations '((2 2) (4 4) (6 6) (8 8) (10 10) (12 12) (14 14) (16 16) (18 18) (20 20) (22 22) (24 24) 
                                (2 24) (4 22) (6 20) (8 18) (10 16) (12 14) (14 12) (16 10) (18 8) (20 6) (22 4) (24 2)
                                (2 13) (4 13) (6 13) (8 13) (18 13) (20 13) (22 13) (24 13))
                                ))
        (setq r2 (make-instance 'A*1-agent :goal '(25 25) :ob_loc (object-locations (slot-value sim 'world)) :world_size (slot-value (slot-value sim 'world) 'size)))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'node_number)) node_number2)
    )

    (setq node_number3 '())
    (dotimes (i 20)
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                :num-obstacles 45
                                :obstacle-locations '((2 4) (3 4) (4 4) (2 3) (2 2))
                                ))
        (setq r2 (make-instance 'A*1-agent :goal '(25 25) :ob_loc (object-locations (slot-value sim 'world)) :world_size (slot-value (slot-value sim 'world) 'size)))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'node_number)) node_number3)
    )

    (print '(***Steps list 1***))
    (print node_number1)
    (print '(*********))
    (print '(***Steps list 2***))
    (print node_number2)
    (print '(*********))
    (print '(***Steps list 3***))
    (print node_number3)
    (print '(*********))
)

(defun test_A*2 ()
    (setq node_number1 '())

    (dotimes (i 20)
        ;;;World 1: No obsticles
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                    :num-obstacles 0
                                    ))
        (setq r2 (make-instance 'A*2-agent :goal '(25 25) :ob_loc (object-locations (slot-value sim 'world)) :world_size (slot-value (slot-value sim 'world) 'size)))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'node_number)) node_number1)
    )
    
    (setq node_number2 '())
    (dotimes (i 20)
        ;;;World 2: Obstacles, but no false corners
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                :num-obstacles 0
                                :obstacle-locations '((2 2) (4 4) (6 6) (8 8) (10 10) (12 12) (14 14) (16 16) (18 18) (20 20) (22 22) (24 24) 
                                (2 24) (4 22) (6 20) (8 18) (10 16) (12 14) (14 12) (16 10) (18 8) (20 6) (22 4) (24 2)
                                (2 13) (4 13) (6 13) (8 13) (18 13) (20 13) (22 13) (24 13))
                                ))
        (setq r2 (make-instance 'A*2-agent :goal '(25 25) :ob_loc (object-locations (slot-value sim 'world)) :world_size (slot-value (slot-value sim 'world) 'size)))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'node_number)) node_number2)
    )

    (setq node_number3 '())
    (dotimes (i 20)
        (create-simulator)
        (setq sim (create-simulator :size '(25 25)
                                :num-obstacles 45
                                :obstacle-locations '((2 4) (3 4) (4 4) (2 3) (2 2))
                                ))
        (setq r2 (make-instance 'A*2-agent :goal '(25 25) :ob_loc (object-locations (slot-value sim 'world)) :world_size (slot-value (slot-value sim 'world) 'size)))
        (remove-object sim '(25 25))
        (add-robot sim :robot r2)
        (run sim :for 1000)
        (push (print (slot-value r2 'node_number)) node_number3)
    )

    (print '(***Steps list 1***))
    (print node_number1)
    (print '(*********))
    (print '(***Steps list 2***))
    (print node_number2)
    (print '(*********))
    (print '(***Steps list 3***))
    (print node_number3)
    (print '(*********))
)