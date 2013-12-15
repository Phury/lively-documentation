(in-package :ldoc)

(define-condition ldoc-error (error)
  ((text 
    :initarg :text 
    :reader text)))

; String
;******************************************************************************
(defmethod string-split ((string string) &optional (separator #\/) l)
  "split a string into a list of string where each string is delimited by a <separator> char. The first word may not have a first separator and the last word may not hav a last separator.
	For example var1/var2/var3 will give (var1 var2 var3)."
  (let ( (i 0) )
    (loop for char across string
       do (if (char-equal char separator) 
	      (return-from string-split 
		(string-split (subseq string (+ i 1))
			      separator
			      (append l (list (subseq string 0 i)))))
	      (incf i)))
    (if (string-equal string "")
	l
	(append l (list string)))))

(defmethod string-empty ((string string))
  (if (= 0 (length (string-trim '(#\Space #\n #\t) string))) T))

(defmethod str ((obj T))
  (format nil "~a" obj))

(defun uri-to-oid (uri)
  "Concert an uri in object-identifier"
  (append (list (second uri))
	  (list (first uri))
	  (cdr (cdr uri))))
; List
;******************************************************************************
(defmethod add ((cons cons) elt)
  (setf (cdr (last cons)) (cons elt nil))
  cons)

(defmethod add ((null null) elt)
  (cons elt nil))

(defun list-intersection (list test)
  (if (>= (length list) 2)
      (list-intersection 
       (push (intersection (pop list) (pop list) :test test) list)
       test)
      (first list)))


; Dictionary
;******************************************************************************
(defclass Entry()
  ((key
    :initarg :key
    :reader key)
   (value
    :initarg :value
    :accessor value)))

(defclass Dictionary()
  ((entries
    :initarg :entries
    :initform nil
    :reader entries)
   (test
    :initarg :test
    :initform 'equal)))

(defmethod get-entry ((dic Dictionary) key)
  (with-slots (entries test) dic
    (loop for entry in entries
       when (funcall test (key entry) key)
       do (return entry))))

(defmethod get-value ((dic Dictionary) key)
  (let ( (entry (get-entry dic key)) )
    (if entry (value entry))))

(defmethod remove-entry ((dic Dictionary) key)
  (with-slots (entries) dic
    (setf entries (remove (get-entry dic key) entries))))

(defmethod add-entry ((dic Dictionary) key value)
  (with-slots (entries) dic
    (when (get-entry dic key)
      (remove-entry dic key))
    (setf entries (add entries (make-instance 'entry 
					      :key key 
					      :value value)))))

(defmethod key-set ((dic Dictionary))
  (with-slots (entries) dic
    (loop for entry in entries
       collect (key entry))))

(defmethod value-set ((dic Dictionary))
  (with-slots (entries) dic
    (loop for entry in entries
       collect (value entry))))

(defmethod as-list ((dic Dictionary))
  (let ( (res nil) )
    (loop for key in (key-set dic)
       do (setf res (add res (list :key key :value (get-value dic key)))))
    res))

(defmethod has-key ((dic Dictionary) key)
  (when (get-value dic key) T))

(defmethod empty ((dic Dictionary))
  (with-slots (entries) dic
    (if (> (length entries) 0) T)))

(defun make-dictionary(&optional test)
  "Make a new dictionary. The optional test is the function used to retrieve information in the dictionnary."
  (if test
      (make-instance 'Dictionary :test test)
      (make-instance 'Dictionary)))


; Tree
;******************************************************************************
(defclass Node()
  ((value
    :initarg :value
    :accessor value)
   (parent
    :initarg :parent
    :initform nil
    :accessor parent)
   (children
    :initarg children
    :initform nil
    :accessor children)))

(defmethod as-list ((node Node))
  (with-slots (value parent children) node
    (list :name value
	  :parent (when parent
		    (value parent))
	  :children (loop for child in children
		       collect (as-list child)))))

(defmethod is-root-node((node Node))
  (when (not (parent node)) 
    T))

(defmethod is-leaf-node((node Node))
  (when (not (children node))
    T))

(defmethod add-node((node Node) (parent Node))
  "Add a node to the tree"
  (with-slots (children) parent
    (setf children (add children node))
    (if (not (parent node))
	(setf (parent node) parent)))
  node)

(defun make-node (value &optional (parent nil))
  (make-instance 'Node 
		 :value value
		 :parent parent))


(defmethod get-ancestors-loop((node Node) list)
  (setf list (add list node))
  (if (parent node)
      (get-ancestors-loop (parent node) list))
  list)

(defmethod get-ancestors((node Node))
  (get-ancestors-loop node nil))

(defclass Tree()
  ((root
    :initarg :root
    :initform nil
    :accessor root)))

(defmethod find-node-loop((node Node) value)
  (if (equal (value node) value)
      node
      (loop for child in (children node)
	 do (let ( (n (find-node-loop child value)) )
	      (if n (return-from find-node-loop n))))))

(defmethod find-node((tree Tree) value)
  (if (root tree)
      (find-node-loop (root tree) value)))

(defmethod add-node((tree Tree) (node Node))
  (if (not (root Tree))
      (setf (root Tree) node))
  t)

(defmethod node-list-loop((node Node) list)
  (setf list (add list node))
  (with-slots (children) node
    (if children
	(loop for child in (children node)
	   do (setf list (node-list-loop child list)))))
  list)

(defmethod node-list((tree Tree))
  (if (root tree)
      (node-list-loop (root tree) nil)))
 

(defun make-tree ()
  (make-instance 'Tree))