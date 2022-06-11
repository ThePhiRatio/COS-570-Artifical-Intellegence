;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This document serves as the 
;;; control stucture for the 
;;; Resolution Theorum Prover (RTP)
;;; Based on rtp.lisp by 
;;; Dr. Roy Turner
;;;
;;; Author: Nicholas Soucy
;;;
;;; COS 570 F2021
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Imports
(load "unify.lisp")

(load "messages")
(shadowing-import 'msg:msg)
(use-package 'msg)
(all-messages)

;;;CNF garden world axiom set
(defvar garden_world
    ;;;Carrots are vegetables
    '(((vegetable carrots))
    ;;;John likes carrots
    ((likes John carrots))
    ;;;Mary likes carrots
    ((likes Mary carrots))
    ;;;half of In order to grow something, you must own a garden
    ((not (grows ?x6 ?y5)) (garden (sk2 ?x6 ?y5)))
    ;;;To eat something, you have to own it
    ((not (eats ?x3 ?y2)) (owns ?x3 ?y2))
    ;;;When you grow something, you own it
    ((not (grows ?x4 ?y3)) (owns ?x4 ?y3))
    ;;;half of In order to grow something, you must own a garden
    ((not (grows ?x5 ?y4)) (owns ?x5 (sk1 ?x5 ?y4)))
    ;;;John grows vegetables he likes
    ((not (likes John ?x1)) (not (vegetable ?x1)) (grows John ?x1))
    ;;;When you like a vegetable, you grow it
    ((not (likes ?x2 ?y1)) (not (vegetable ?y1)) (grows ?x2 ?y1)))
)

;;;CUSTOM CNF geese axiom set: very funny stuff here
(defvar geese_world
    ;;;Greg is nice
    '(((nice Greg))
    ;;;Tim is a goose
    ((goose Tim))
    ;;;Greg is running
    ((running Greg))
    ;;;all goose are mean
    ((not (goose ?x1)) (mean ?x1))
    ;;;when something is mean, it will bite you
    ((not (mean ?x2)) (bites ?x2 ?y1))
    ;;;when you are nice, you aren't mean
    ((not (nice ?x4)) (not (mean ?x4)))
    ;;;When something bites you, you run from it
    ((not (bites ?x3 ?y2)) (running ?y2)))
)

;;;some standard computable predicates 
(defvar *computable-predicates*
    '(((gt . >))
     
    ((lt . <)))
)

;;;Function: Prove
;;;Arguments:
;;;  - theorem: a list of literal(s) to prove
;;;  - axioms: name of axiom-set; defined above
;;;  - bindings: list of bindings
;;;Returns: text and binding list
;;;Description: 
;;;   Wrapper function to get input from the user and 
;;;   call rtp function to determine if a contradiction was
;;;   reached and report to the user

(defun prove (theorem axioms &key bindings)
  """Wrapper class that calls rtp and returns results to user"""
    (multiple-value-bind  (contradiction binding-list) 
        ;;;call rtp on negated theorem
        (rtp (negate theorem) axioms :bindings bindings)
        ;;;if we get a nil back from rtp
        (if (not contradiction)
            ;;;say the theorem is proven and print bindings and axioms
            (progn
            (fmsg "Theorem can be proven, contradiction found.  Bindings = ~s." binding-list)
            (fmsg "Axiom Set Used = ~s" axioms))
            
            ;;;else say the theorem cannot be proven
            (fmsg "Cannot prove the theorem."))
        (values (not contradiction) binding-list))
)


;;;Function: rtp
;;;Arguments:
;;;  - clause: a list of literal(s) to prove
;;;  - axioms: name of axiom-set; defined above
;;;  - bindings: list of bindings
;;;Returns: text and binding list
;;;Description: 
;;;   Function serves as control structure for the resolution
;;;   theorem prover. This determines if there a contradiction
;;;   with the clause in the given axiom set. 

(defun rtp (clause axioms &key bindings)
    """rtp impliments a resolution theorem prover, returns nil if there is a contradition"""
    (vfmsg "Attempting to find contradiction with clause ~s." clause)
    (cond
    ;;;if the clause is nil, we found our contradiction, return bindings, or if
    ;;;the cluase is empty, return.
     ((null clause)
        (values nil bindings))
    ;;;else, if there is actually something to prove...
     (t
        ;;;check for each literal in the clause we are proving
        (loop for literal in clause 
            ;;;if that literal is a computable and not a true predicate....
            when (and (computable-predicate? literal)
                    (not (true-predicate? literal bindings)))
            ;;;recursevly call rtp on the new clause without the chosen literal
            do  (multiple-value-bind (success binding-list) 
                (rtp (instantiate (remove literal clause :test #'equal) bindings) 
                                            axioms :bindings bindings)
                ;;;if we got a contradiction, return nil, our clause was proven
                (if (not success)
                    (return-from rtp (values nil binding-list))))
            ;;;if we still have not had a contradiction...
            else do 
                ;;;look at each literal in each axiom...
                (loop for axiom in axioms do
                    (loop for aliteral in axiom do
                    (multiple-value-bind (success binding-list)
                        ;;;see if literals resolve
                        (resolves? literal aliteral bindings)
                        ;;;if it does resolve, recursevly call rtp with the new resolved literal
                        ;;;resolved literal as the new clause
                        (when success
                          (vfmsg "Resolved: ~s" clause)
                          (vfmsg "    with: ~s" axiom)
                          (multiple-value-setq (success binding-list)
                          (rtp
                          (instantiate (resolvent literal clause aliteral axiom  binding-list)
                                binding-list)
                          axioms :bindings binding-list))
                          ;;;when we resolve if we get a nil, we proved the statement, return values.
                          (when (null success)
                              (return-from rtp (values nil binding-list))))))))
            ;;;else we failed, return with :fail value.
            (values :fail bindings))))


;;;Function: compuable-predicate?
;;;Arguments:
;;;  - literal: a list
;;;Returns: T or nil
;;;Description: 
;;;   Function determines if a given literal is a computable
;;;   predicate by searching in the above defined 
;;;   *computable-predicates* list for a match.

(defun computable-predicate? (literal)
  """Checks to see if given literal is a computable prdicate
  that is stored in the *computable-predicates* variable"""
  ;;;if it starts with a not...
  (if (eql 'not (car literal))
    ;;;recall function with negated literal
    (computable-predicate? (negate-literal literal))
    ;;;else, look to see if it is in *computable-predicates*
    (cdr (assoc (car literal) *computable-predicates*))))



;;;Function: true-predicate?
;;;Arguments:
;;;  - literal: a list
;;;  - bindings: list of bindings
;;;Returns: T or nil
;;;Description: 
;;;   Function determines if a given literal is a true
;;;   predicate by seeing if it has an unbound variable,
;;;   or by evaluating the computable predicate via the
;;;   *computable-predicates* list.

(defun true-predicate? (literal &optional bindings)
  """Determine if the given literal is a true predicate"""
  (let (value)
    (setq literal (instantiate literal bindings))
    (setq value
      (cond
       ;;;if there is a unbound variable, return t
       ((unbound-var-in-literal? literal) t)
       ;;;if it starts with not, recall function with negated literal
       ((eql 'not (car literal))
	        (not (true-predicate? (cadr literal) bindings)))
       ;;;evaluate it based on what *computable-predicate* dictates
       (t 
	        (eval `(,(cdr (assoc (car literal) *computable-predicates*)) ,@(cdr literal))))))
    (dfmsg "[computable predicate ~s is ~a]"
	   literal (if  value "true" "false"))
    value))


;;;Function: unbound-var-in-litearl?
;;;Arguments:
;;;  - literal: a list
;;;Returns: T or nil
;;;Description: 
;;;   Function determines if a given literal has a 
;;;   variable in it that is unbound.

(defun unbound-var-in-literal? (lit)
  """check to see if given litearl has an unbound variable"""
  (cond
   ;;;if it literal is nil, return nil
   ((null lit) nil)

   ;;;if its a list, check to see if either element is unbound
   ;;;than the whole thing is unbound
   ((listp lit)
        (or (unbound-var-in-literal? (car lit))
	        (unbound-var-in-literal? (cdr lit))))
   
   ;;;if it is a variable, return t
   ((variable? lit) t)

   (t nil)))

;;;Function: resolves?
;;;Arguments:
;;;  - literal A: a list
;;;  - literal B: a list
;;;  - bindings: list of bindings
;;;Returns: T or nil
;;;Description: 
;;;   Function determines if Literal A and Literal B
;;;   resolve with each other by calling the Unify 
;;;   function written by Dr. Roy Turner in unify.lisp

(defun resolves? (lita litb &optional bindings)
  """checks if two literals resolve"""
  ;;;Use Dr. Turner's unify code to determine if they resolve
  (unify (negate-literal lita) litb bindings))

;;;Function: resolvent
;;;Arguments:
;;;  - clause-lit: a list
;;;  - clause: a list
;;;  - axiom-lit: a list
;;;  - axiom: a list
;;;  - bindings: list of bindings
;;;Returns: list
;;;Description: 
;;;   takes the two literals that resolve, clause-lit
;;;   and axiom-lit, removes them from clause and axiom 
;;;   respectively, then appends the results. This is 
;;;   implimenting the resolution rule in logic.

(defun resolvent (clause-lit clause axiom-lit axiom &optional bindings)
  """Returns a resolved literal"""
  (append (remove clause-lit clause :test #'equal)
	      (remove axiom-lit axiom  :test #'equal)))

;;;Function: negate
;;;Arguments:
;;;  - clause: a list
;;;Returns: list
;;;Description: 
;;;   takes a clause with any number of or'ed literals
;;;   and negates them via de morgan's law.

(defun negate (clause)
  """returns a negated clause"""
  (cond
   ;;;Check if clause is nil, then return nil
   ((null clause) nil)
   ;;;if clause length is greater than 1, then it is an applied or, therefore
   ;;;we apply de Morgan's law
   ((> (length clause) 1)
    (values (list (negate-literal (car clause)))
	    (mapcar #'(lambda (a) (negate (list a))) (cdr clause))))
   
   (t (list (negate-literal (car clause))))))

;;;Function: negate-literal
;;;Arguments:
;;;  - literal: a list
;;;Returns: list
;;;Description: 
;;;   takes a litearl and adds a not if it didn't
;;;   have one, or removes it if it did.

(defun negate-literal (lit)
  """either adds or removes a not from a literal"""
  (if (eql 'not (car lit))
    (cadr lit)
    (list 'not lit)))