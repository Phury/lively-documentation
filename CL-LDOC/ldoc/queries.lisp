#|
QUERIES:
Predefined queries for the lively documentation system
|#
(in-package :ldoc)

;; Define how to find packages
(defquery :packages () ()
  (let ( (output (loop for pkg in (list-all-packages)
		    collect (make-object-entity-wrapper pkg))) )
    (sort output (lambda(a b) (string-lessp (get-name a) (get-name b))))))


;; Define how to find a package
(defquery :package-name :packages (list name)
  (loop for elt in list
     when (string-equal name (get-name elt))
     do (return elt)))

;; Define how to find content of a package
(defquery :package-content :package-name (obj)
  (let ( (symbols nil)
	 (package (wrapped obj)) )
    (do-symbols (s package)
      (when (is-in-package s package)
	(push (make-symbol-entity-wrapper s obj) symbols)))
    (sort symbols (lambda(a b) (string-lessp (get-name a) (get-name b))))))

;; Define how to find an object of type desired from elements in a package
(defquery :type :package-content (list type)
  (loop for elt in list
     when (string-equal (get-type elt) type)
     collect elt))

;; Define how to find an object of types desired from elements in a package
(defquery :types :package-content (list types)
  (loop for elt in list
     when (loop for type in types
	   when (string-equal (get-type elt) type)
	   do (return t))
     collect elt))

;; Define how to find an object of type desired and wich name is given in param
(defquery :name :type (list name)
  (loop for elt in list
     when (equal (get-name elt) name)
     do (return elt)))

;; Define how to find slots of a class
(defquery :slots :name (obj)
  (if (is-class obj)
      (let ( (class (wrapped obj)) )
	(loop for slot in (compute-slots class)
	   collect (make-object-entity-wrapper slot obj)))))

;; Define how to find methods of a class
(defquery :methods :name (obj)
  (if (is-class obj)
      (let ( (class (wrapped obj)) )
	(loop for meth in (specializer-direct-methods class)
	   collect (make-object-entity-wrapper meth obj)))))

;; Define how to find a method given its name
(defquery :method :methods (list name)
  (loop for elt in list
     when (string-equal (get-name elt) name)
     do (return elt)))

(defquery :subclasses :name (obj)
  (when (is-class obj)
    (let ( (class (wrapped obj)) )
      (loop for subclass in (class-direct-subclasses class)
	 collect (make-symbol-entity-wrapper (class-name subclass)
					     (make-object-entity-wrapper 
					      (symbol-package 
					       (class-name subclass))))))))
(defquery :superclasses :name (obj)
  (when (is-class obj)
    (let ( (class (wrapped obj)) )
      (loop for superclass in (class-direct-superclasses class)
	 collect (make-symbol-entity-wrapper (class-name superclass)
					     (make-object-entity-wrapper 
					      (symbol-package 
					       (class-name superclass))))))))


;; Define how to find content of a package including methods and slots
(defquery :full-package-content :package-content (list)
  (let ( (all-content list) )
    (loop for elt in list
       when (is-class elt)
       do (let* ( (class (wrapped elt))
		  (slots (handler-case (compute-slots class)
			   (error () nil)))
		  (methods (handler-case  (specializer-direct-methods class)
			     (error () nil))) )
	    (setf slots (loop for slot in slots
			   collect (make-object-entity-wrapper slot elt)))
	    (setf methods (loop for meth in methods
			     collect (make-object-entity-wrapper meth elt)))
	    (setf all-content (append all-content slots methods))))
    (sort all-content (lambda(a b) (string-lessp (get-name a) (get-name b))))))

;; Define how to find tagged entity
(defquery :tagged :full-package-content (list tag)
  (loop for elt in list
     when (has-tag (metadata elt) tag)
     collect elt))

;; Count the occurences of tags
(defquery :tag-count :full-package-content (list)
  (let ( (counts (make-dictionary)) )
    (loop for elt in list
       do (loop for tag in (tags (metadata elt))
	     do (if (has-key counts tag)
		    (add-entry counts tag (+ (get-value counts tag) 1))
		    (add-entry counts tag 1))))
    counts))


;--------------------
; TUTORIAL
;--------------------
(defquery :tutorials ()()
  (let ( (tutorials 
	  (directory 
	   (make-pathname 
	    :name :wild :type :wild 
	    :defaults (make-url *server-static-dir*
				"/data/tutorials/TUTORIAL/")))) )
    (loop for files in tutorials
	 collect (make-tutorial-object (pathname-name files)))))

(defquery :tutorial :tutorials (list name)
  (loop for elt in list
     when (string-equal (get-name elt) name)
     do (return elt)))
