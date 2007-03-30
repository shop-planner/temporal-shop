;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;---------------------------------------------------------------------------
;;; Copyright (c) 2005 Smart Information Flow Technologies, d/b/a SIFT, LLC
;;; All rights reserved.
;;;
;;; SIFT PROPRIETARY
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    Code for augmenting SHOP2 operators and method definitions with
;;;    temporal arguments.
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2005/09/23:rpg] Created, based on code used in PVACS.
;;;
;;;---------------------------------------------------------------------------
(in-package :shop2)

(defclass simple-temporal-domain (domain)
     ()
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(!!record-start !!record-end simple-temporal-domain at-time)))


;;;---------------------------------------------------------------------------
;;; Experimental code for controlling SHOP2 search in temporal domains
;;;---------------------------------------------------------------------------


(defun now ()
  (second (first (query-current-state 'time))))

(defun unschedulable (task-list unifier)
  (let ((now (now)))
    (loop for task in task-list
	  for inst-task = (apply-substitution task unifier)
	for preferred-time = (preferred-time inst-task)
	when (and (numberp preferred-time)
		  (< preferred-time now))
	return t
	finally (return nil))))

(defun preferred-time (task)
  (case (car task)
    ;; I'm not sure this arises, but just to be on the safe side...
    (:immediate (preferred-time (cdr task)))
    (:task (preferred-time (cdr task)))
    (otherwise (at-time *domain* (first task) task))))



(defgeneric at-time (domain symbol task)
  (:documentation "Dispatching on SYMBOL, return a time value for when TASK should be scheduled,
or NIL.  Typically this will be a function, defined on the list structure that is TASK, that
picks some nth element to return."))

(defmethod at-time ((domain simple-temporal-domain) symbol task)
  "Default method --- no time for TASK."
  (declare (ignore symbol task))
  NIL)

(defun spacer-task-p (task)
  (case (car task)
    ;; I'm not sure this arises, but just to be on the safe side...
    (:immediate (spacer-task-p (cdr task)))
    (:task (spacer-task-p (cdr task)))
    (otherwise (eq (first task) '!!spacer))))


;;;---------------------------------------------------------------------------
;;; 
;;;---------------------------------------------------------------------------
(defconstant +untimed-first+ nil)

(defun make-task-iterator-alist (task-list unifier
					   &optional (untimed-first +untimed-first+)
					   (remove-from-spacers t))
  "Create an ALIST suitable for the TASK-SORTER:  associates with each
task in task list either a time at which is should commence, or NIL."
  (let (alist
	(min-time nil)
	(has-spacers nil))
    (loop for task in task-list
	  for inst-task = (apply-substitution task unifier)
	  for spacer-task-p = (spacer-task-p inst-task)
	  for time = (preferred-time inst-task)
	  when spacer-task-p
	    do (setf has-spacers t)
	  when (variablep time)
	    do (setf time nil)
	  do (push (cons task time) alist)
	  when time 
	    unless (numberp time)
	      do (error "Times should be numbers  Time for ~S is ~A."
			task time)
	  end
	  and when (or (null min-time) (< time min-time))
		do (setf min-time time))
    (setf alist (nreverse alist))	; I'd like this to be stable
    (when (and has-spacers remove-from-spacers)
      ;; we remove all the untimed tasks if there's a spacer, to force
      ;; the spacer to happen first.  This is just a symmetry
      ;; reduction, and may actually enforce
      ;; correctness. [2005/11/13:rpg]
      (setf alist (delete nil alist :key #'cdr)))
    (when min-time
      (setf alist (delete-if #'(lambda (cell)
				 (let ((time (cdr cell)))
				   (and time (> time min-time))))
			     alist))
      (setf alist 
	    (stable-sort alist #'< :key #'(lambda (x)
					    (or (cdr x)
						(if untimed-first -1
						    MOST-POSITIVE-FIXNUM))))))
    alist))

;;;(defun make-task-iterator-thunk (alist)
;;;  "Return a thunk that's suitable to be the return value
;;;of TASK-ITERATOR, qv."
;;;  (if alist
;;;      #'(lambda ()
;;;	  (cons 
;;;	   (caar alist)
;;;	   (make-task-iterator-thunk (cdr alist))))
;;;    #'(lambda ()
;;;	nil)))

(defmethod task-sorter ((domain simple-temporal-domain)
			  task-list unifier)
  (let ((alist (make-task-iterator-alist task-list unifier)))
    (mapcar #'car alist)))
	      
;;;(defun user-choose-task (task-list unifier &optional (immediate nil) (leashed *leashed*))
;;;  "Function called to allow a user to choose the next task for expansion, instead 
;;;of SHOP2."
;;;  (let ((num-tasks (length task-list))
;;;	(now (now)))
;;;    (cond
;;;      ((unschedulable task-list unifier)
;;;       (let ((response
;;;	      (y-or-n-p "Task list unschedulable at time ~D:~%Abort planning?"
;;;			now)))
;;;	 (if response
;;;	     (throw 'user-done nil)
;;;	     nil)))
;;;      ((and (= num-tasks 1) leashed)
;;;       (let* ((bound-task (apply-substitution (first task-list) unifier))
;;;	      (input (y-or-n-p "Only one candidate ~:[~;immediate ~] task for expansion:~%~T~S time ~S~%Proceed?~%"
;;;			       immediate bound-task
;;;			       (preferred-time bound-task))))
;;;	 (if input
;;;	     (first task-list)
;;;	     (if (y-or-n-p "Abort planning?")
;;;		 (throw 'user-done nil)
;;;		 (user-choose-task task-list unifier immediate)))))
;;;      ((= num-tasks 1)		; not leashed
;;;       (let ((bound-task (apply-substitution (first task-list) unifier)))
;;;	 (format t "Only one candidate ~:[~;immediate ~] task for expansion:~%~T~T~S time ~S.~%~TProceeding.~%"
;;;		 immediate bound-task
;;;		 (preferred-time bound-task))
;;;	 (first task-list)))
;;;      (t
;;;       (format t "~&Choose a~:[ ~;n immediate ~] task for expansion at time ~D:~%"
;;;	       immediate now)
;;;       (loop for i from 0
;;;	     for task in task-list
;;;	     for bound-task = (apply-substitution task unifier)
;;;	     do (format t "~D - ~S~@[ TIME: ~D~]~%" i bound-task (preferred-time bound-task)))
;;;       (let ((input (read)))
;;;	 (if (and (fixnump input)
;;;		  (>= input 0)
;;;		  (< input num-tasks))
;;;	     (nth input task-list)
;;;	     (user-choose-task task-list unifier immediate leashed)))))))

;;;---------------------------------------------------------------------------
;;; Stuff imported from the pvacs temporal domain code.  This was a
;;; not terribly well thought out attempt to automagically generate
;;; temporal arguments.  It turned out in general to be easier NOT to
;;; use this, because I got something wrong in the design.  Needs to
;;; be revisited. [2007/03/29:rpg]
;;;---------------------------------------------------------------------------

(defparameter NEW-PARAMETERS
	      '(?this.start ?this.end) ;; ?this.start-after ?this.deadline)
  "New parameters to be added to every method's header.")

;;; code that allows us the shorthand to omit the start and end
;;; variables. [2007/03/29:rpg]
(defmethod parse-domain-item ((domain simple-temporal-domain) (type (eql :temporal-method)) method-definition)
  "This method adds special ?THIS.START, ?THIS.END, ?THIS.START-AFTER, and ?THIS.DEADLINE variables
at the end of the method definition before creating it.  Must be added to the header, and to
all the methods invoked inside, to avoid busting the unification.  This function just invokes the
slave function PVACS-AUGMENT-METHOD-DEF."
  ;; written this way to make it easier to test outside of a DEFDOMAIN.
  (let ((new-method-def (pvacs-augment-method-def method-definition domain))
	;; needed by helper functions [2004/02/16:rpg]
	(*domain* domain))
    (declare (special *domain*))
    (set-variable-property domain new-method-def)
    (parse-domain-item domain :method new-method-def)))

(defun pvacs-augment-method-def (method-definition domain)
    ;; note that latter on we need a way to refer to steps in the task networks.
  ;; we can do this with a naming construct...
  ;; form:  (:method h [n_i] C_i T_i ... [n_n] C_n T_n)
  ;; h - head; C_i - Precondition; T_i task list...
  (let* ((method-head (second method-definition))
	 (method-body (cddr method-definition))
	 (new-method-body (pvacs-augment-method-body method-body domain)))
    `(:method ,(append method-head NEW-PARAMETERS)
	      ,@new-method-body)))  

;;; Something's wrong in the following, defeating my deadline constraint-handling.
;;; [2004/01/15:rpg]
(defun pvacs-augment-method-body (method-body domain &optional acc)
  (if (null method-body) (reverse acc)	;should probably change to nreverse
      (let* ((has-name (when (symbolp (first method-body)) (first method-body)))
	     (condition (if has-name (second method-body) (first method-body)))
	     (task-list (if has-name (third method-body) (second method-body)))
	     (new-condition condition)
	     (new-task-list (pvacs-augment-task-list task-list domain)))
	(pvacs-augment-method-body (if has-name (cdddr method-body) (cddr method-body))
				   domain
				   (append
				    (if has-name `(,new-task-list ,new-condition ,has-name)
					`(,new-task-list ,new-condition))
				    acc)))))

(defun pvacs-augment-task-list (tasks domain)
  "Does a recursive descent of the task-list to create a new one, adding the 
new parameters for temporal reasoning."
  (let ((new-task-list (pvacs-add-temporal-parameters tasks domain)))
    `(:ordered (!!record-start ?this.start)
	       ,new-task-list
	       (:task :immediate !!record-end ?this.end))))


(defun pvacs-add-temporal-parameters (tasks domain)
  "Does a recursive descent of the task-list TASKS to create a new one, adding the 
new parameters for temporal reasoning."
  (flet ((primitivep (symbol)
	   ;; we need our own definition of this, because it won't be
	   ;; fbound yet...[2004/01/09:rpg]
	   (char= #\! (char (symbol-name symbol) 0))))
    (cond
      ((null tasks) nil)
      ((member (first tasks) '(:ordered :unordered))
       (cons (first tasks)
	     (mapcar #'(lambda (task) (pvacs-add-temporal-parameters task domain)) (rest tasks))))
      ((eq (first tasks) :task)
       (let* ((immediatep (eq (second tasks) :immediate))
	      (task-symbol 
	       (if immediatep (third tasks)
		   (second tasks)))
	      (terms (if immediatep
			 (cdddr tasks)
			 (cddr tasks))))
	 (cond ((primitivep task-symbol)
		tasks)
	       ((gethash task-symbol (temporal-methods domain) nil)
		`(:task ,@(when immediatep '(:immediate)) ,task-symbol ,@(append terms (make-new-param-list))))
	       (t tasks))))
      ((atom (first tasks))
       (cond ((primitivep (first tasks))
	      (cons :task tasks))
	     ((gethash (first tasks) (temporal-methods domain) nil)
	      `(:task ,@tasks ,@(make-new-param-list)))
	     (t (cons :task tasks))))
      (t
       (cons :ordered
	     (mapcar #'(lambda (task) (pvacs-add-temporal-parameters task domain)) tasks))))))




;;;---------------------------------------------------------------------------
;;; Tests for task iterator
;;;---------------------------------------------------------------------------
#|
(defparameter *null-times-task-list-for-test*
      (cdr				; dump :ordered
       (process-task-list
	'( 
	  (:TASK OVERWATCH-SORTIE 102 JULIET 75 VICTOR-AIR :UNBOUND 1800 2400
		 :UNBOUND ?START2 ?END2)
	  (:TASK OVERWATCH-SORTIE 201 ALPHA 75 DESTAGE-201 :UNBOUND 500 800 :UNBOUND
	   ?OS.START1696843 ?OS.END1696844)
	  (:TASK OVERWATCH ALPHA 75 :UNBOUND 800 1800 :UNBOUND ?O.START1696845
	   ?O.END1696846)))))
|#

;;; these deftests currently don't work because I flipped the order
;;; that the tasks come out, in favor of the UNTIMED tasks. [2004/08/23:rpg]

#|
(rt:deftest check-null-times-alist
    (:documentation "Make sure we get stable results with a
list of untimed tasks.")
    (let ((alist (make-task-iterator-alist *null-times-task-list-for-test* nil)))
      (equalp  (mapcar #'car alist)
	       *null-times-task-list-for-test*))
  t)

(rt:deftest check-timed-tasks-first-alist
    (let* ((new-task (process-task-list '(:task wait-until 201 50)))
	   (tasks (append *null-times-task-list-for-test* (list new-task)))
	   (alist (make-task-iterator-alist tasks nil)))
      (and
       (equalp new-task (car (first alist)))
       (equalp (mapcar #'car (cdr alist)) *null-times-task-list-for-test*))
       )
  T)

(rt:deftest check-timed-tasks-first-iterator
    (let* ((new-task (process-task-list '(:task wait-until 201 50)))
	   (tasks (append *null-times-task-list-for-test* (list new-task)))
	   (vlist nil)
	   (*current-state* (make-state '((time 0)))))
      (declare (special *current-state*))
      (labels ((iter (thunk)
		 (let* ((val (funcall thunk))
			(value (car val))
			(new-thunk (cdr val)))
		   (when value
			   (push value vlist)
			   (iter new-thunk)))))
	(iter (task-iterator tasks nil))
	(setf vlist (nreverse vlist))
	(and
	 (equalp new-task (first vlist))
	 (equalp (cdr vlist) *null-times-task-list-for-test*))
	))
  T)

(rt:deftest check-timed-tasks-sort
    (let ((*current-state* (make-state '((time 0))))
	  tasks vlist)
      (declare (special *current-state*))
      (loop for i in '(101 102 103 104)
	    for j in '(50 100 200 300)
	    do (push (process-task-list `(:task wait-until ,i ,j))
		     tasks))
      (labels ((iter (thunk)
		 (let* ((val (funcall thunk))
			(value (car val))
			(new-thunk (cdr val)))
		   (when value
		     (push value vlist)
		     (iter new-thunk)))))
	(iter (task-iterator tasks nil))
	(nreverse vlist)))
  ((:task wait-until 101 50)))

(rt:deftest check-timed-tasks-unsched
    (:documentation "Make sure a task iterator will just return nil 
when given an unschedulable list of tasks.")
    (let ((*current-state* (make-state '((time 60))))
	  tasks vlist)
      (declare (special *current-state*))
      (loop for i in '(101 102 103 104)
	    for j in '(50 100 200 300)
	    do (push (process-task-list `(:task wait-until ,i ,j))
		     tasks))
      (labels ((iter (thunk)
		 (let* ((val (funcall thunk))
			(value (car val))
			(new-thunk (cdr val)))
		   (when value
		     (push value vlist)
		     (iter new-thunk)))))
	(iter (task-iterator tasks nil))
	(nreverse vlist)))
  nil)


(rt:deftest check-timed-and-untimed-tasks
    (:documentation "Check to make sure that when we mix
timed and untimed tasks, we get the schedulable timed tasks 
and all the null tasks.")
  (equalp (cons '(:task wait-until 101 50)
		*null-times-task-list-for-test*)
	  (let ((*current-state* (make-state '((time 0))))
		(tasks *null-times-task-list-for-test*)
		vlist)
	    (declare (special *current-state*))
	    (loop for i in '(101 102 103 104)
		for j in '(50 100 200 300)
		do (push (process-task-list `(:task wait-until ,i ,j))
			 tasks))
	    (labels ((iter (thunk)
		       (let* ((val (funcall thunk))
			      (value (car val))
			      (new-thunk (cdr val)))
			 (when value
			   (push value vlist)
			   (iter new-thunk)))))
	      (iter (task-iterator tasks nil))
	      (nreverse vlist))))
  t)
|#