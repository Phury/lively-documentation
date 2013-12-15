(in-package :ldoc)

(defclass Tutorial-Object(Ldoc-Object-Identifier) 
  ((sections 
    :initform nil
    :accessor sections)))

(defmethod initialize-instance :after ((obj Tutorial-Object) &key)
  (with-slots (sections) obj
    (setf sections (get-metadata obj))
    (setf sections
	  (loop for section in sections
	     collect (let* ( (oid
			      (uri-to-oid
			       (string-split (meta section))))
			     (entity (result
				      (execute-query
				       (apply 'query 
					      :name oid)))) ) 
		       (append (as-list section) 
			       (list :entity
				     (list (as-list-with-meta entity)))))))))

(defmethod as-list ((obj Tutorial-Object))
  (list :name (get-name obj) 
	:type (get-type obj)
	:uri (oidstring obj)))

(defmethod as-list-with-meta ((obj Tutorial-Object))
  (with-slots (sections) obj
	     (append (as-list obj)
		     (list :sections (as-list sections)))))

(defmethod metadata-path ((obj Tutorial-Object))
  (format nil "ldoc/data/tutorials/~a" (oidstring obj)))

(defun make-tutorial-object (&rest identifiers)
  "Make a tutorial object from the identifiers receiven in parameter. For instance: 'TUTORIAL' 'CREATE-WIDGET'"
  (make-instance 'Tutorial-Object :identifiers (append (list "TUTORIAL")
						       identifiers)))