#|
MOPI : Meta Object Protocol Interface
This file contains general meta programming for the lively documentation system
|#
(in-package :ldoc)




;;; Define MOP behavior for SYMBOL
(defmethod is-in-package ((symbol symbol) package)
  (equal (symbol-package symbol) package))

(defmethod get-class ((symbol symbol))
  (handler-case (values (find-class symbol) "CLASS")
    (error () nil)))

(defmethod get-generic-function ((symbol symbol))
  (handler-case (values (symbol-function symbol) "GENERIC-FUNCTION")
      (error () nil)))

(defmethod get-macro ((symbol symbol))
  (let ( (macro (get-generic-function symbol)) )
    (if (and macro (macro-function symbol))
	(values macro "MACRO"))))

(defmethod get-method ((symbol symbol))
  (let ( (meth (get-generic-function symbol)) )
    (if (and meth (equal (type-of meth) 'standard-generic-function))
	(values meth "METHOD"))))
   
(defmethod get-function ((symbol symbol))
  (let ( (fun (get-generic-function symbol)) )
    (if (and fun (not (get-macro symbol)) (not (get-method symbol)))
	(values fun "FUNCTION"))))

(defmethod get-variable ((symbol symbol))
  (if (boundp symbol)
      (values symbol "VARIABLE")
      (values symbol "SYMBOL")))


(defmethod get-object ((symbol symbol))
  (let ( (getters (list 'get-class 'get-macro 'get-method 
			'get-function 'get-variable)) )
    (loop for fun in getters
       when (funcall fun symbol)
       do (return (funcall fun symbol)))))


(defmethod get-symbol ((package package))
  nil)

(defmethod get-package ((package package))
  package)


;;; Redefine behavior for GET-NAME 

(defmethod get-name ((package package))
  (str (package-name package)))
	
(defmethod get-name ((symbol symbol))
  (str (symbol-name symbol)))

(defmethod get-name ((slot standard-slot-definition))
  (str (slot-definition-name slot)))

(defmethod get-name ((method standard-method))
  (str (generic-function-name (method-generic-function method))))



;;; Redefine behavior for AS-LIST
(defmethod as-list ((null null))
  nil)

(defmethod as-list-with-meta ((null null))
  nil)

(defmethod as-list ((cons cons))
  (let ( (res nil) )
    (loop for elt in cons
       do (handler-case (setf res (add res (as-list elt)))
	    (error ()
	      (setf res (append res (list elt))))))
    res))

(defmethod as-list-with-meta ((cons cons))
  (let ( (res nil) )
    (loop for elt in cons
       do (handler-case (setf res (add res (as-list-with-meta elt)))
	    (error ()
	      (setf res (append res (list elt))))))
    res))



;;; Define LDOC-TYPE for some objects
(defmethod get-type ((obj T))
  (type-of obj))

(defmethod get-type ((package package))
  "PACKAGE")

(defmethod get-type ((class standard-class))
  "CLASS")

(defmethod get-type ((fun function))
  "FUNCTION")

(defmethod get-type ((symbol symbol))
  (if (and symbol (boundp symbol))
      "VARIABLE"
      "SYMBOL"))

(defmethod get-type ((method standard-method))
  "METHOD")

(defmethod get-type ((slot slot-definition))
  "SLOT")

; OBJECT
;******************************************************************************
(defclass Ldoc-Object()
  ((name
    :initarg :name
    :initform nil
    :reader get-name))
  (:documentation "Base entity for the LDOC package"))

(defmethod find-by-name ((cons cons) (obj Ldoc-Object))
  (loop for elt in cons
     when (string-equal (get-name elt) (get-name obj))
     do (return elt)))

(defun name-equal(a b)
  (string-equal (get-name a) (get-name b)))

; OBJECT IDENTIFIER
;******************************************************************************
(defclass Ldoc-Object-Identifier (Ldoc-Object)
  ((identifiers
    :initarg :identifiers
    :initform nil
    :reader oid))
  (:documentation "Object that defines ldoc object identifiers for the Lively documentation system"))

(defmethod get-type ((obj Ldoc-Object-Identifier))
  (first (oid obj)))

(defmethod get-name ((obj Ldoc-Object-Identifier))
  (car (last (oid obj))))

(defmethod oidstring ((obj Ldoc-Object-Identifier))
  (let* ( (oid (oid obj))
	  (str (pop oid)) )
    (loop for elt in oid
       do (setf str (format nil "~a/~a" str elt)))
    str))

(defmethod metadata-path ((obj Ldoc-Object-Identifier))
  (format nil "ldoc/data/documentation/~a" (oidstring obj)))

(defun make-id-object (&rest identifiers)
  "Make an identifiable object from the identifiers receiven in parameter. For instance: 'LDOC' 'CLASS' 'FILTER'"
  (make-instance 'Ldoc-Object-Identifier :identifiers identifiers))

(defun make-global-id-object (&rest identifiers)
  "Make an identifiable object from the identifiers receiven in parameter. The identifiable object concerns multiple other identifiable objects"
  ; Make something like hash of ordoned identifiers for unique id
  ; global on the references of all concerned identifiers
  (make-instance 
   'Ldoc-Object-Identifier 
   :identifiers (string-split (global-metadata-path identifiers))))

#|
; Entry Handler
;******************************************************************************
(defclass MetaEntry-Handler ()
  ((tag
    :initform nil)
   (meta
    :initform (error "Must supply a meta object for which handler operates")
    :initarg :meta))
  (:documentation "Define a handler that handles operations to do on specific metadata found for an object"))

(defmethod accept-entry ((handler MetaEntry-Handler) entry)
  "Do the treatement of the entry"
  (error "Method must be overriden for effective tag handling"))

(defmethod handle-entry ((handler MetaEntry-Handler) entry)
  "Check if this handler is defined for the entry and call accept-entry if it is correct"
  (with-slots (tag) handler
    (when (equal (name entry) tag)
      (accept-entry handler entry))))

; Unknown tag
;************
(defclass Default-Handler(MetaEntry-Handler) ()
  (:documentation "Handle a tag that is not defined by adding the information in the metadata object"))

(defmethod accept-entry ((handler Default-Handler) entry)
  "Add the entry into the documentation information of the metadata object"
  (with-slots (meta) handler
    (with-slots (doc) meta
      (setf doc (add doc entry)))))

; Todo tag
;*********
(defclass Tag-Todo-Handler(Default-Handler) ()
  (:documentation "Handle a TODO tag. Tag the object as TODO"))

(defmethod accept-entry ((handler Tag-Todo-Handler) entry)
  "Add a TODO tag in the metadata object"
  (with-slots (meta) handler
    (with-slots (tags) meta
      (setf tags (add tags (value entry)))))
  (call-next-method))
|#


; METADATA
;******************************************************************************
(defclass Ldoc-Metadata (Ldoc-Object-Identifier)
  ((docstring
    :initarg :docstring
    :initform nil
    :reader docstring)
   (doc
    :initform nil)
   (tags
    :initform nil
    :reader tags))
  (:documentation "A metadata object holds information relative to an ur-identifiable object. This information can be the docstring, tags or any other metadata needed to be added to an object in the lively documentation system"))

(defmethod initialize-instance :after ((meta Ldoc-Metadata) &key)
  (with-slots (doc docstring tags) meta
    (let ( (srcdoc (get-metadata meta)) )
      (if srcdoc
	  (loop for elt in srcdoc
	     do (handle-entry meta elt))
	  (setf tags (list "undocumented"))))
    (when (not (has-tag meta (get-type meta)))
      (setf tags (add tags (get-type meta))))))

(defmethod handle-entry((meta Ldoc-Metadata) entry)
  (cond ((string-equal (name entry) "tag")
	 (with-slots (tags) meta
	   (push (value entry) tags)))
	((string-equal (name entry) "docstring")
	 (with-slots (docstring) meta
	   (setf docstring (value entry))))
	((with-slots (doc) meta
	  (setf doc (add doc entry))))))

(defmethod has-tag ((meta Ldoc-Metadata) name)
  (with-slots (tags) meta
    (loop for tag in tags
       when (string-equal tag name)
       do (return T))))

(defmethod add-metadata ((meta Ldoc-Metadata) name type value)
  (handle-entry meta (make-meta-entry name type value)))

(defun make-metadata (identifiers &optional docstring)
  (make-instance 'Ldoc-Metadata 
		 :identifiers identifiers
		 :docstring docstring))

; WRAPPER
;******************************************************************************
(defclass Ldoc-Wrapper(Ldoc-Object-Identifier)
  ((wrapped
    :initarg :wrapped
    :initform nil
    :reader wrapped)
   (wrapped-type
    :initarg :wrapped-type
    :initform (error "Must supply a type for the wrapping")
    :reader wrapped-type)
   (symbol 
    :initarg :symbol
    :initform nil)
   (parent
    :initarg :parent
    :initform nil
    :reader get-parent)
   (metadata
    :initform nil
    :reader metadata))
  (:documentation "An object that Wraps LISP objects to add lively documentation features"))

(defmethod initialize-instance :after ((wrapper Ldoc-Wrapper) &key)
  (with-slots (wrapped wrapped-type identifiers metadata) wrapper
    (if (get-parent wrapper)
	(setf identifiers (append (list wrapped-type)
				  (cdr (oid (get-parent wrapper)))
				  (list (get-name wrapper))))
	(setf identifiers (list wrapped-type (get-name wrapper))))
    (let ( (docstring nil) )
      (when (and wrapped (not (or
			       (string-equal wrapped-type "SYMBOL")
			       (string-equal wrapped-type "VARIABLE"))))
	(setf docstring (documentation wrapped T)))
      (setf metadata (make-metadata identifiers docstring)))))

(defmethod get-type ((wrapper Ldoc-Wrapper))
  (wrapped-type wrapper))

(defmethod get-symbol ((wrapper Ldoc-Wrapper))
  (with-slots (symbol wrapped) wrapper
    (handler-case (get-symbol wrapped)
      (error ()
	symbol))))

(defmethod get-name ((wrapper Ldoc-Wrapper))
  (with-slots (wrapped symbol name) wrapper
    (handler-case (get-name wrapped)
      (error ()
	(handler-case (get-name symbol)
	  (error ()
	    name))))))

(defmethod get-docstring ((wrapper Ldoc-Wrapper))
  (with-slots (metadata) wrapper
    (docstring metadata)))

(defmethod get-metadata-entries ((wrapper Ldoc-Wrapper))
  (with-slots (metadata) wrapper
    (with-slots (doc) metadata
      doc)))

(defmethod get-tag-entries ((wrapper Ldoc-Wrapper))
  (with-slots (metadata) wrapper
    (with-slots (tags) metadata
      tags)))

(defmethod get-metadata-entry ((wrapper Ldoc-Wrapper) name)
  (with-slots (metadata) wrapper
    (with-slots (doc) metadata
      (loop for entry in doc
	 when (string-equal (name entry) name)
	 do (return entry)))))

(defmethod as-list ((wrapper Ldoc-Wrapper))
  "Return a LIST (:pkg :name :urn :type :ref) of the object T."
  (with-slots (parent) wrapper
    (let* ( (parentname (if parent (get-name parent))) )
      (list :parent parentname
	    :name (get-name wrapper)
	    :type (get-type wrapper)
	    :uri (oidstring wrapper)))))

(defmethod as-list-with-meta ((wrapper Ldoc-Wrapper))
  "Return a LIST (:pkg :name :urn :type :ref :docstring :documentation) of the object T."
  (with-slots (metadata) wrapper
    (with-slots (doc docstring tags) metadata
      (append (as-list wrapper)
	      (list :documentation (as-list doc)
		    :docstring docstring
		    :tags (loop for tag in tags
			     collect (list :tag tag)))))))
    
(defmethod is-class ((wrapper Ldoc-Wrapper))
  (if (get-class (get-symbol wrapper)) T))

(defmethod is-macro ((wrapper Ldoc-Wrapper))
  (if (get-macro (get-symbol wrapper)) T))

(defmethod is-method ((wrapper Ldoc-Wrapper))
  (if (get-method (get-symbol wrapper)) T))

(defmethod is-slot ((wrapper Ldoc-Wrapper))
  (if (get-slot (get-symbol wrapper)) T))

(defmethod is-function ((wrapper Ldoc-Wrapper))
  (if (get-function (get-symbol wrapper)) T))

(defmethod is-variable ((wrapper Ldoc-Wrapper))
  (if (get-variable (get-symbol wrapper)) T))


(defun make-symbol-entity-wrapper (symbol &optional (parent nil))
  "Make an entity wrapper from a symbol and a parent."
  (multiple-value-bind (ref type)
      (get-object symbol)
    (make-instance 'Ldoc-Wrapper 
		   :symbol symbol
		   :wrapped ref
		   :wrapped-type type
		   :parent parent)))

(defun make-object-entity-wrapper (ref &optional (parent nil))
  "Make an entity wrapper from a reference to an object."
  (make-instance 'Ldoc-Wrapper
		 :wrapped ref
		 :wrapped-type (get-type ref)
		 :parent parent))

	
  