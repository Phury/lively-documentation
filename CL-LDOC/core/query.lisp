#|
QUERY:
General behaviour to build queries in the lively documentation system.
|#
(in-package :ldoc)

(defparameter *filters* (make-dictionary 'string-equal))


; FILTER
;******************************************************************************
(defun find-filter (keyword)
  (get-value *filters* keyword))

(defclass Filter(Ldoc-Object)
   ((prodfun
    :initarg :prodfun
    :reader get-fun)))


(defmethod generate ((filter Filter))
  (funcall (get-fun filter)))

(defmethod produce ((filter Filter) input)
  (funcall (get-fun filter) input))

(defun precedence-filter-list-loop (filter list)
  (push filter list)
  (if (equal (type-of filter) 'sieve)
      (return-from precedence-filter-list-loop
	(precedence-filter-list-loop (get-input filter) list))
      list))

(defmethod precedence-filter-list ((filter Filter))
  (precedence-filter-list-loop filter nil))

(defmethod produce-output ((filter Filter) &optional arguments)
  (let* ( (filterlist (precedence-filter-list filter))
	  (result (generate (pop filterlist))) )
    (loop for elt in filterlist
       do (setf result 
		(if (and (equal (type-of elt) 'sieve)
			 (need-arguments elt))
		    (produce-with-arguments elt result (pop arguments))
		    (produce elt result))))
    result))

(defclass Generator(Filter) ())

(defclass Sieve(Filter)
  ((input
    :initarg :input
    :initform nil
    :reader get-input)
   (arguments
    :initarg :arguments
    :initform nil
    :reader need-arguments)))

(defmethod produce-with-arguments ((sieve Sieve) list arguments)
  (funcall (get-fun sieve) list arguments))

(defun make-filter (keyword function &optional (parent nil) (need-arg nil))
  (let ( (filter (if parent
		     (make-instance 'sieve
				    :name keyword
				    :prodfun function
				    :arguments need-arg
				    :input (find-filter parent))
		     (make-instance 'generator
				    :name keyword 
				    :prodfun function))) )
    (add-entry *filters* keyword filter)))

; Rename to define-filter

(defmacro defquery (keyword parent arguments &body body)
  "Define a new Filter. The keyword must be unique and identifies the Filter. The parent is the keyword of the filter on which this filter is based for its output. Arguments are the variables necessary to generate a production function an that are used in the @body code of the defined generator function."
  (let* ( (fun `(lambda ,arguments ,@body))
	  (args (if (< 1 (length arguments)) T nil)) )
     `(make-filter ,keyword ,fun ,parent ,args)))

#|
(defun define-query (class)
 	(let ( (filter (make-instance class)) )
	 (add-entry *filters* (get-name filter) filter)))
|#
; Query
;******************************************************************************
(defclass Query()
  ((command
    :initarg :command
    :reader command)
   (result
    :initarg :result
    :initform nil
    :reader result)
   (parameters
    :initarg :parameters
    :initform nil
    :reader parameters)))

(defmethod execute-query ((query query))
  "Execute the command of the query and set it in the result slot of the Query."
  (with-slots (command result) query 
    (setf result (handler-case (produce-output 
				(find-filter (car command)) 
				(cdr command))
		   (error () nil)))
  query))

(defmethod command-string ((query query))
  "Return the command of the query as a string representation."
  (let* ( (select (command query))
	  (str (pop select)) )
    (loop for elt in select
       do (setf str (format nil "~a/~a" str elt)))
    str))

(defmethod as-list ((query query))
  "Return the result of the query as-list."
  (as-list (result query)))

(defmethod as-list-with-meta ((query query))
  (as-list-with-meta (result query)))

(defun query (name &rest args)
  "Make a new Query"
  (make-instance 'query :command (append (list name) args)))

(defun find-from-uri (name arglist)
  (execute-query (apply 'query name arglist)))






