;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;AI COS:540 Assignment 1
;;;
;;;Author: Nicholas Soucy
;;;
;;;Purpose: File holds all functions needed for HW1
;;;
;;;For: Dr. Roy Turner

;;;For question 0?
(defun hello_world ()
"This is just to say Hello World! It is very important!"
  (format t "Hello World"))


;;;Q1
(defun first2 (l)
  "This function takes in a list 'l' and outputs a list where the elements are the first two elements in 'l'"
  (list (first l) (second l)))

;;;Q2
(defun add1 (a)
"This function adds the two integers 'a' and '1'"
  (1+ a))

;;;Q3
(defun listadd1 (l)
  "This function recursively adds 1 to each element in 'l'"
  (unless (null l)
    (cons (1+ (car l)) (listadd1 (cdr l)))))

(defun listadd2 (l)
  "This function adds 1 to each element in 'l' using mapcar"
  (mapcar #'1+ l))

(defun listadd3 (l)
  "This function adds 1 to each element in 'l' using loop constructs"
  (loop for i in l
      collect (1+ i)))

;;;Q4
(defun flatten (l)
  "This function recursively flattens a list even ones that include embedded list"
  (if (null l)    ;;;BC: check to see if we are at end of list, null is a great function!
      nil
      (if (atom (first l))     ;;;check to see if it is an element and not a list
          (cons (first l) (flatten (rest l)))  ;;;then take the first element and cons them with the flatten of the rest
          
          (append (flatten (first l)) (flatten (rest l))))))  ;;;else, append the flatten of the first list with the rest of the list


;;;Q5
(defun last2 (l)
  "This function takes a list 'l' and outputs a list where the elements are the last two elements in 'l'"
  (if (not (nth 2 l)) ;;;if we are at the end of the list
      l               ;;; return the list
      
      (last2 (cdr l)))) ;;;if not, recursively call the cdr

;;;Q6
(defun my-reverse (l)
  "This function takes a list 'l' and returns a reversed list"
  (if (null l) ;;;BC: check to see if we are at end of list
      nil
      
      (append (my-reverse (cdr l)) (list (car l))))) ;;;traverse down the list, when at the end, start appending

;;;Q7
(defun printlist (l)
  "This function used format to print the elements in a list"
  (loop for i in l
        do (format t "~@(~:R~) element: ~D ~%" (+ (position i l) 1) i)))

;;;Q8
(defun copyfile (foo bar)
  "This function takes the file 'foo' and copies all its contents to 'bar'"
  (with-open-file (in foo :direction :input)
    (with-open-file (out bar :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop with line
          until (eql :eof (setq line (read-line in nil :eof)))
          do (write-line line out)))))

;;;Q9
(defclass animal ()
  ((num-legs :initform 4
             :initarg :num_legs
             :accessor num_legs)))

(defclass bird (animal)
  ((num-legs :initform 2)))

(defclass cat (animal)
  ())

(defmethod speak ((self cat))
  (print "meow"))

(defmethod speak ((self bird))
  (print "tweet"))

;;;Q10
(let ((tmp))  ;;;wrapper tmp for scope
  (defun next (&optional l)
    (if l
        (setq tmp (cons () l)))   ;;;if there was a new list passed, save it
    (pop tmp)))   ;;;if no list, just pop the first element off
