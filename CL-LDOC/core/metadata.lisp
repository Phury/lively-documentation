(in-package :ldoc)


; TAGS
;******************************************************************************
(defvar *tags* (make-tree))
(defvar *root-tag* "tag")

(add-node *tags* (make-node *root-tag*))

(defun add-tag (tag &optional (parent nil))
  (when (not parent)
    (setf parent *root-tag*))
  (add-node (make-node tag) (find-node *tags* parent)))

(defun find-tag (name)
  (as-list (find-node *tags* name)))

(defun get-meta-tags()
  (node-list *tags*))

(defun get-related-tags(tag)
  (when (find-node *tags* tag)
    (loop for node in (children (parent (find-node *tags* tag)))
       when (not (string-equal (value node) tag))
       collect (value node))))

; Define some tags
;*****************

(add-tag "type")
(add-tag "package" "type")
(add-tag "class" "type")
(add-tag "function" "type")
(add-tag "macro" "type")
(add-tag "variable" "type")
(add-tag "method" "type")
(add-tag "slot" "type")

(add-tag "section")
(add-tag "printing" "section")
(add-tag "accessing" "section")
(add-tag "converting" "section")
(add-tag "testing" "section")

(add-tag "structure")
(add-tag "private" "structure")
(add-tag "public" "structure")
(add-tag "protected" "structure")
(add-tag "abstract" "structure")

(add-tag "documentation")
(add-tag "undocumented" "documentation")
(add-tag "todo" "documentation")
(add-tag "test-run" "documentation")
(add-tag "example" "documentation")


; TYPES
;******************************************************************************
(defparameter *types* (list "text" "code"))

(defun add-type (name)
  (setf *types* (add *types* name)))

(defun get-meta-types() 
  *types*)

(defparameter *parsers* (make-dictionary 'string-equal))

(defclass Type-Parser (Ldoc-Object) ())

(defmethod parse((parser Type-Parser) input)
  (error() "Parse should be implemented by subclass"))

(defun make-type-parser (class)
  (let ( (ref (make-instance class)) )
    (add-entry *parsers* (get-name ref) ref)
    (add-type (get-name ref))))

(defun find-type-parser(name)
  "Return a parser which name is given in parameter or NIL if no parser with that name exist"
  (get-value *parsers* name))


(defun parse-type(type input)
  "Parse the input given in parameter according to the given type"
  (let ( (parser (find-type-parser type)) )
    (if parser
	(parse parser input)
	input)))


; Parse LISP
;***********
(defclass Lisp-Parser (Type-Parser)
  ((name :initform "lisp")))

(defmethod parse((parser Lisp-Parser) input)
  (format nil "<pre class=\"code\"><code class=\"lisp\">~a</code></pre>" input))

(make-type-parser 'Lisp-Parser)

; Parse Markdown
;***************
(defclass Markdown-Parser (Type-Parser)
  ((name :initform "markdown")
   (pathstr :initform (concatenate 'string 
				   *server-static-dir*
				   "extensions/syntax/markdown/markdown.pl"))))
   
(defmethod parse((parser Markdown-Parser) input)
  (let ( (tmpfile "/tmp/input.markdown") (result nil) )
    (with-open-file (out tmpfile 
			 :direction :output 
			 :if-does-not-exist :create
			 :if-exists :supersede)
      (write-string input out))
    (with-slots (pathstr) parser
      (let* ( (proc (sb-ext:run-program 
		     pathstr
		     (list tmpfile) 
		     :input nil 
		     :output :stream
		     :error :stream
		     :wait nil))
	      (output (sb-ext:process-output proc)) )
	(loop for line = (read-line output nil nil)
	   while line do (setf result (concatenate 'string result line)))))
    result))

(make-type-parser 'Markdown-Parser)


; META-ENTRY
;***********
(defun make-entry-name (name)
  (format nil "@~a" name))

(defun make-entry-type (type)
  (format nil "#~a" type))

(defclass DocumentationEntry ()
  ((name
    :initform nil
    :initarg :name
    :accessor name)
   (meta
    :initform nil
    :initarg :meta
    :accessor meta)
   (value
    :initform nil
    :initarg :value
    :accessor value))
  (:documentation "A DocumentationEntry holds the name, the type and the value of a documentation entry"))

(defmethod as-list((obj DocumentationEntry))
  (with-slots (name meta value) obj
    (list :name name :type meta :value value)))

(defmethod as-list-with-meta((obj DocumentationEntry))
  (as-list obj))

(defmethod write-meta((obj DocumentationEntry) output)
  (with-slots (name meta value) obj
    (write (list name meta value) :stream output)))

(defun make-documentation-entry(&rest args)
  (make-instance 'DocumentationEntry 
		 :name (first args)
		 :meta (second args)
		 :value (third args)))

; METADATA HANDLING
;******************************************************************************
(defun global-metadata-path (object)
  (format nil "global"))


(defun has-special-path (object)
  (char-equal (elt (get-name object) 0) #\*))

(defun get-file-metadata (object)
  (when (has-special-path object)
    (return-from get-file-metadata nil))
  (let* ( (pathstr (metadata-path object))
	  (in (open pathstr :if-does-not-exist nil))
	  (entries nil) )
    (when in 
      (loop for list = (read in nil) 
         while list do (setf entries 
			     (add entries 
				  (apply 'make-documentation-entry list))))
      (close in))
    entries))

(defun put-file-metadata (object metalist)
  (let* ( (out (open (metadata-path object) 
		     :direction :output 
		     :if-exists :supersede
		     :if-does-not-exist :create)) )
    (when out
      (loop for elt in metalist
	 do (write-meta elt out))
      (close out)
      T)))

(defmethod get-metadata ((obj Ldoc-Object-Identifier))
  (get-file-metadata obj))

(defmethod save-metadata ((obj Ldoc-Object-Identifier) metalist)
  (put-file-metadata obj metalist))




