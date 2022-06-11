;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This document shows all agents
;;; running based on the 
;;; homework specifications
;;;
;;; Author: Nicholas Soucy
;;;
;;; COS 570 F2021
;;;

;;;Imports
(load "simulator.lisp")
(shadowing-import '(sim:name sim:simulator))
(use-package 'simulator)

(load "messages.lisp")
(shadowing-import 'msg:msg)
(use-package 'message)

(load "simple-pqueue.lisp")

(load "robot-logic.lisp")

(load "towers_hanoi.lisp")

(load "testing-functions.lisp")


(defun run-robots ()

    (print '(***Reflex Agent***))
    ;;;reflex agent
    (create-simulator)
    (setq sim (create-simulator :size '(10 10)
                                :num-obstacles 0
                                :obstacle-locations '((2 2) (4 4) (6 6) (8 8) (2 8) (4 6) (6 4) (8 2))
                                ))
    (world-sketch sim)
    (setq r2 (make-instance 'reflex-agent))
    (add-robot sim :robot r2)
    (run sim :for 50 :sketch-each t)

    (print '(***Model Agent***))
    ;;;model agent
    (create-simulator)
    (setq sim (create-simulator :size '(10 10)
                                :num-obstacles 0
                                :obstacle-locations '((2 2) (4 4) (6 6) (8 8) (2 8) (4 6) (6 4) (8 2))
                                ))
    (world-sketch sim)
    (setq r3 (make-instance 'model-agent))
    (add-robot sim :robot r3 :location '(3 1))
    (run sim :for 50 :sketch-each t)

    (print '(***Hill Agent***))
    ;;;hill agent
    (create-simulator)
    (setq sim (create-simulator :size '(10 10)
                                :num-obstacles 0
                                :obstacle-locations '((2 2) (4 4) (6 6) (8 8) (2 8) (4 6) (6 4) (8 2))
                                ))
    (world-sketch sim)
    (setq r4 (make-instance 'reflex-hill-agent :goal '(10 10)))
    (add-robot sim :robot r4) 
    (run sim :for 50 :sketch-each t)

    (print '(***Uniform Cost Agent***))
    ;;;uniform cost agent
    (create-simulator)
    (setq sim (create-simulator :size '(10 10)
                                :num-obstacles 0
                                :obstacle-locations '((2 2) (4 4) (6 6) (8 8) (2 8) (4 6) (6 4) (8 2))
                                ))
    (world-sketch sim)
    (setq r5 (make-instance 'uniform-agent :goal '(10 10) :ob_loc (object-locations (slot-value sim 'world)) :world_size (slot-value (slot-value sim 'world) 'size)))
    (add-robot sim :robot r5 :location '(2 1) :random-location nil) 
    (run sim :for 50 :sketch-each t)

    (print '(***A* Manhattan Agent***))
    ;;;A*1 agent - Manhattan
    (create-simulator)
    (setq sim (create-simulator :size '(10 10)
                                :num-obstacles 0
                                :obstacle-locations '((2 2) (4 4) (6 6) (8 8) (2 8) (4 6) (6 4) (8 2))
                                ))
    (world-sketch sim)
    (setq r6 (make-instance 'A*1-agent :goal '(10 10) :ob_loc (object-locations (slot-value sim 'world)) :world_size (slot-value (slot-value sim 'world) 'size)))
    (add-robot sim :robot r6 :location '(2 1) :random-location nil)
    (run sim :for 50 :sketch-each t) 

    (print '(***A* Euclidean Agent***))
    ;;;A*2 agent - Euclidean
    (create-simulator)
    (setq sim (create-simulator :size '(10 10)
                                :num-obstacles 0
                                :obstacle-locations '((2 2) (4 4) (6 6) (8 8) (2 8) (4 6) (6 4) (8 2))
                                ))
    (world-sketch sim)
    (setq r7 (make-instance 'A*2-agent :goal '(10 10) :ob_loc (object-locations (slot-value sim 'world)) :world_size (slot-value (slot-value sim 'world) 'size)))
    (add-robot sim :robot r7 :location '(2 1) :random-location nil) 
    (run sim :for 50 :sketch-each t)
)

(defun run-other ()
    (print '(***Uniform Cost Agent***))
    ;;;Towers of Hanoi Uniform cost: 4 disk
    (towers-hanoi-unifom-cost 4)

    (print '(***A* Agent***))
    ;;;Towers of Hanoi A*: 4 disk
    (towers-hanoi-A*-cost 4)
)
