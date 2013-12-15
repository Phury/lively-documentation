(in-package :ldoc)


(defparameter *debug* nil)
;---------------------------------------------------------------------
; ABSTRACT PAGE CLASSES
;---------------------------------------------------------------------

;--------------------
; DOCUMENTATION PAGES
;--------------------
(defclass Page-Documentation(Page)()
  (:documentation "Base class for the pages that show documentation"))

(defmethod make-list-widget((page Page-Documentation) title results)
  "Make a List-Widget and fill it correctly with the elements of the result list given in parameter"
  (let ( (widget (make-instance 'List-Widget :title title)) )
    (loop for elt in results
       do (add-item widget (as-list elt)))
    widget))

(defmethod make-table-widget((page Page-Documentation) title results)
  "Make a Table-Widget and fills it correctly with the elements of the result list given in parameter"
  (let ( (widget (make-instance 'Table-Widget :title title)) )
    (add-column widget "name")
    (add-column widget "docstring")
    (loop for elt in results
       do (add-row widget 
		   :name (get-name elt)
		   :value (get-docstring elt)))
    widget))

(defmethod make-entity-list((page Page-Documentation) keyword results)
  "Make multiple Entity-Widget objects where each Entity-Widget contains one element of the result list given in parameter. The result is a list of Entity-Widgets"
  (let ( (widgets nil) )
    (loop for elt in results
       do (let ( (widget (make-instance 'Entity-Widget)) )
	    (set-entity widget elt)
	    (push widget widgets)))
    widgets))

(defmethod make-column-list-widget((page Page-Documentation) columns title results)
  "Make multiples list widgets where each elements of the result are equally
distributed in each of the list widgets and return a list of those widgets"
  (let ( (widgets (loop for i from 1 to columns
		     collect (make-instance 'List-Widget :title title)))
	 (max (ceiling (/ (length results) columns))) )
    (loop for elt in results
       with i = 0
       with currList = 0
       when (>= i max)
       do (progn
	    (incf currList)
	    (setf i 0))
       do (progn
	    (add-item (elt widgets currList) (as-list elt))
	    (incf i)))
    widgets))

;--------------------
; LISTING
;--------------------
(defclass Page-Listing(Page-Documentation)
  ((entity-type 
    :initform (error "Entity type should be provided")
    :reader get-entity-type)
   (title 
    :initform nil
    :reader get-title))
  (:documentation "Default behavior for pages that show a listing if no argument is provided"))

(defmethod update-model ((page Page-Listing) uri)
  (if (>= 1 (length uri))
      (make-listing-view page uri)
      (make-entity-view page uri)))

(defmethod make-entity-view((page Page-Listing) uri)
  (let* ( (command (list (second uri) (first uri) (third uri)))
	  (package (result
		    (execute-query
		     (query :package-name (second uri)))))
	  (types (result
		  (execute-query
		   (apply 'query :type command))))
	  (entities (apply 'call-widgets
			   :entities
			   (make-entity-list page :entities types))) )
    (with-slots (template model) page
      (setf template #P"doc/entity.html")
      (setf model (append (list :title (get-entity-type page))
			  (as-list package)
			  (call-widget
			   (make-table-widget page (get-title page) types)
			   :entity-table)
			  entities)))))

(defmethod make-entity-view :after ((page Page-Listing) uri)
  (let* ( (types (list "CLASS" "FUNCTION" "MACRO" "VARIABLE"))
	  (content (result
		    (execute-query
		     (query :types (second uri) types)))) )
    (with-slots (model) page
	(setf model (append model 
			    (call-widget
			     (make-list-widget 
			      page "Package content" content)
			     :pkg-list))))))

(defmethod make-listing-view ((page Page-Listing) uri)
  (let* ( (pkg (get-name (get-default-package)))
	  (entities (result
		     (execute-query
		      (query :type pkg (get-entity-type page)))))
	  (widgets (make-column-list-widget page 4 "" entities))
	  (keywords (list :list1 :list2 
			  :list3 :list4)) )
    (with-slots (template model) page
      (setf template #P"doc/listing.html")
      (let ( (widget-list (loop for w in widgets
			     for i from 0 to (length widgets)
			     collect (call-widget w (elt keywords i)))) )
	(push (list :title (get-entity-type page)) widget-list)
	(setf model (apply 'append widget-list))))))



;---------------------------------------------------------------------
; CONCRETE PAGE CLASSES
;---------------------------------------------------------------------


;--------------------
; HOME
;--------------------
(defclass Page-Home(Page-Documentation)
  ((listable :initform T))
  (:documentation "Home page of the documentation system"))

(defmethod update-model ((page Page-Home) uri)
  (let* ( (package (get-name (get-default-package)))
	  (packages (result
		     (execute-query
		      (query :packages))))
	  (tagcount (result
		     (execute-query
		      (query :tag-count package))))
	  (tasks (result
		  (execute-query
		   (query :tagged package "TODO"))))
	  (classes (make-random-list 
		    page (result
			  (execute-query
			   (query :type package "CLASS"))) 7))
	  (functions (make-random-list 
		      page (result
			    (execute-query
			     (query :type package "FUNCTION"))) 7))
	  (macros (make-random-list 
		   page (result
			 (execute-query
			  (query :type package "MACRO"))) 7))
	  (variables (make-random-list 
		      page (result
			    (execute-query
			     (query :type package "VARIABLE"))) 7)) )
    (with-slots (model) page
      (setf model (append (list :name package)
			  (call-widget  
			   (make-cloud-widget page "Tag-Cloud" tagcount)
			   :tag-cloud)
			  (call-widget  
			   (make-todo-widget page "Tasks" tasks)
			   :tasks)
			  (call-widget
			   (make-list-widget 
			    page "Content of package" packages)
			   :pkg-list)
			  (call-widget
			   (make-list-widget page "Classes" classes)
			   :class-list)
			  (call-widget
			   (make-list-widget page "Functions" functions)
			   :function-list)
			  (call-widget
			   (make-list-widget page "Macros" macros)
			   :macro-list)
			  (call-widget
			   (make-list-widget page "Variables" variables)
			   :variable-list)
			  (call-widget  
			   (make-tree-widget page) :tree))))))


(defmethod make-random-list ((page Page-Home) result size)
  (loop for i from 1 to (min size (length result))
     collect (elt result (random (length result)))))

(defmethod make-tree-widget((page Page-Home))
  (let ( (widget (make-instance 'Tree-Widget)) )
    (loop for node in (get-meta-tags)
       do (if (parent node)
	      (add-tree-item widget (value node) (value (parent node)))
	      (add-tree-item widget (value node))))
    widget))

(defmethod make-cloud-widget((page Page-Home) title results)
  (let ( (widget (make-instance 'Tag-Cloud-Widget :title title)) )
    (loop for tag in (key-set results)
       do (add-tag-item widget tag (get-value results tag)))
    widget))

(defmethod make-todo-widget((page Page-Home) title results)
  (let ( (widget (make-instance 'Todo-Widget :title title)) )
    (loop for elt in results
       do (add-task-item widget 
		    (as-list elt) 
		    (value (get-metadata-entry elt "todo"))))
    widget))

;--------------------
; SYSTEM
;--------------------
(defclass Page-System(Page-Documentation)
  ((listable :initform T))
  (:documentation "Page that shows a listing of the packages of the system"))

(defmethod update-model ((page Page-System) uri)
  (let* ( (entities (result
		     (execute-query
		      (query :packages))))
	  (widgets (make-column-list-widget page 4 "" entities))
	  (keywords (list :list1 :list2 :list3 :list4)) )
    (with-slots (template model) page
      (setf template #P"doc/listing.html")
      (let ( (widget-list (loop for w in widgets
			     for i from 0 to (length widgets)
			     collect (call-widget w (elt keywords i)))) )
	(setf model (apply 'append widget-list))))))


;--------------------
; PACKAGE
;--------------------
(defclass Page-Package(Page-Documentation)
  ((listable :initform T))
  (:documentation "Page that shows a listing of the package content when the package name is given in parameter"))

(defmethod update-model ((page Page-Package) uri)
  (let* ( (package nil)
	  (types (list "CLASS" "FUNCTION" "MACRO" "VARIABLE"))
	  (keywords (list :class :function :macro :variable)) )
    (setf *test-post* uri)
    (if (> (length uri) 1)
	(setf package (second uri))
	(setf package (get-name(get-default-package))))
    (with-slots (model) page
      (setf model nil)
      (loop for type in types
	 do (let ( (result (result
			    (execute-query
			     (query :type package type)))) )
	      (setf model (append model
				  (call-widget 
				   (make-list-widget page type result)
				   (pop keywords)))))))))


;--------------------
;CLASS
;--------------------
(defclass Page-Class(Page-Listing)
  ((entity-type :initform "Class")
   (listable :initform T))
  (:documentation "Page that shows all the classes of the current package if no class name is given or the information about a class if the class name is given as parameter. This page use two disting templates."))

(defmethod make-entity-view ((page Page-Class) uri)
  (let* ( (command (list (second uri) (first uri) (third uri)))
	  (class (result
		  (execute-query
		   (apply 'query :name command))))
	  (slots (result
		  (execute-query
		   (apply 'query :slots command))))
	  (methods (result
		    (execute-query
		     (apply 'query :methods command))))
	  (superclasses (result
			 (execute-query
			  (apply 'query :superclasses command))))
	  (subclasses (result
		       (execute-query
			(apply 'query :subclasses command))))
	  (entities (append (apply 'call-widgets
				   :class
				   (make-entity-list page :class (list class)))
			    (apply 'call-widgets
				   :slots
				   (make-entity-list page :slots slots))
			    (apply 'call-widgets
				   :methods
				   (make-entity-list page :methods methods)))) )
    (with-slots (template model) page
      (setf template #P"doc/class.html")
      (setf model (append (as-list-with-meta class)
			  (call-widget
			   (make-table-widget page "Slots" slots)
			   :slot-table)
			  (call-widget
			   (make-table-widget page "Methods" methods)
			   :method-table)
			  (call-widget
			   (make-list-widget page "Superclasses" superclasses)
			   :superclasses)
			  (call-widget
			   (make-list-widget page "Subclasses" subclasses)
			   :subclasses)
			  entities)))))

;--------------------
; FUNCTION
;--------------------
(defclass Page-Function(Page-Listing)
  ((entity-type :initform "Function")
   (title :initform "Functions")
   (listable :initform T)))

;--------------------
; MACRO
;--------------------
(defclass Page-Macro(Page-Listing)
  ((entity-type :initform "Macro")
   (title :initform "Macros")
   (listable :initform T)))

;--------------------
; VARIABLE
;--------------------
(defclass Page-Variable(Page-Listing)
  ((entity-type :initform "Variable")
   (title :initform "Variables")
   (listable :initform T)))

;--------------------
; SEARCH
;--------------------
(defclass Page-Search(Page)())

(defmethod update-model ((page Page-Search) uri)
  (if (= 2 (length uri))
      (update-post-tags page uri)
      (update-get-tags page uri)))

(defmethod update-get-tags ((page Page-Search) uri)
  (let* ( (tag (car (last uri)))
	  (pkg (get-name (get-default-package)))
	  (result (as-list-with-meta
		   (execute-query
		    (query :tagged pkg tag)))) )
    (with-slots (model) page
      (setf model (list :tag-list (list (list :tag tag)) 
			:related (loop for tag in (get-related-tags tag)
				    collect (list :tag tag))
			:tagged result)))))

(defmethod update-post-tags ((page Page-Search) uri)
  (let* ( (tags (string-split (get-post-parameter "search") #\,))
	  (pkg (get-name (get-default-package)))
	  (result nil)
	  (related nil) )
    (loop for tag in tags
       do (progn
	    (setf result (add result (result
					 (execute-query
					  (query :tagged pkg tag)))))
	    (setf related (add related (get-related-tags tag)))))
    (setf result (list-intersection result 'name-equal))
    (setf related (list-intersection related 'string-equal))
    (with-slots (model) page
      (setf model (list :tag-list (loop for tag in tags
				     collect (list :tag tag))
			:related (loop for tag in related
				    collect (list :tag tag))
			:tagged (as-list-with-meta result))))))



;--------------------
; TUTORIAL
;--------------------
(defclass Page-Tutorial(Page-Documentation)
  ((listable :initform T)) 
  (:documentation "Page that allow users to see the tutorials"))


(defmethod update-model ((page Page-Tutorial) uri)
  (if (>= 1 (length uri))
      (make-listing-view page uri)
      (make-tutorial-view page uri)))

(defmethod make-listing-view ((page Page-Tutorial) uri)
  (let ( (tutorials (result (execute-query (query :tutorials)))) ) 
    (with-slots (model) page
      (setf model (call-widget
		   (make-list-widget page "Tutorials" tutorials)
		   :toc)))))

(defmethod make-tutorial-view ((page Page-Tutorial) uri)
  (let ( (tutorials (result (execute-query (query :tutorials))))
	 (tutorial (result (execute-query (query :tutorial (second uri))))) ) 
    (setf *debug* (as-list tutorial))
    (with-slots (model) page
      (setf model (append (list :title (get-name tutorial))
			  (call-widget
			   (make-list-widget page "Tutorials" tutorials)
			   :toc)
			  (call-widget
			   (make-list-widget 
			    page "Sections" (sections tutorial))
			   :section-list)
			  (add (list :sections)
			       (as-list (sections tutorial))))))))

;--------------------
; OPTIONS
;--------------------
(defclass Page-Admin(Page)
  ((listable :initform T))
  (:documentation "Page that shows the options of the user like adding tags or enable editing"))

(defmethod update-model ((page Page-Admin) uri)
  (with-slots (model) page
    (let ( (all-tags (loop for tag in (get-meta-tags)
			when (not (is-leaf-node tag))
			collect (as-list tag))) )
    (setf model (append (list :all-tags all-tags)
			(as-list (get-user-options)))))))


;--------------------
; Make all pages
;--------------------
(make-page 'Page-Home :home #P"doc/home.html")
(make-page 'Page-System :system #P"doc/listing.html")
(make-page 'Page-Package :package #P"doc/package.html")
(make-page 'Page-Class :class #P"doc/class.html")
(make-page 'Page-Function :function #P"doc/entity.html")
(make-page 'Page-Macro :macro #P"doc/entity.html")
(make-page 'Page-Variable :variable #P"doc/entity.html")
(make-page 'Page-Search :search #P"doc/search.html")
(make-page 'Page-Tutorial :tutorial #P"tuto/tutorial.html")
(make-page 'Page-Admin :admin #P"admin/admin.html")





;---------------------------------------------------------------------
; HANDLERS
;---------------------------------------------------------------------
  

;--------------------
;EDIT-ENTITY
;--------------------
(defclass Entity-Handler(Post-Handler)
  ((name :initform "entity-handler")))

(defmethod action((handler Entity-Handler))
  (redirect (format nil "?~a" (get-post-parameter "uri"))))

(defmethod parse-post-param((handler Entity-Handler) post)
  (let* ( (data nil) (uri nil) (doc nil) (obj nil) (ref nil) )
    (loop for entry in post
       do (progn
	    (when (string-equal (car entry) "uri")
	      (setf uri (string-split (cdr entry)))
	      (setf obj (apply #'make-id-object uri)))
	    (when (string-equal (car entry) "ref")
	      (setf ref (add ref (cdr entry))))
	    (when (or (string-equal (car entry) "name")
		      (string-equal (car entry) "type")
		      (string-equal (car entry) "value"))
	      (setf data (add data (cdr entry))))
	    (when (string-equal (car entry) "tag")
	      (setf data (list "tag" "text" (cdr entry))))
	    (when (= 3 (length data))
	      (setf doc (add doc (apply 'make-documentation-entry data)))
		    (setf data nil))))
    (when (and obj doc)
      (save-metadata obj doc))))

(make-handler 'Entity-Handler)


;--------------------
; AJAX-EDIT-ENTITY
;--------------------
(defclass Ajax-Entity-Handler(Entity-Handler)
  ((name :initform "ajax-entity-handler")))

(defmethod action((handler Ajax-Entity-Handler))
  (display (make-instance 'Notification-Widget 
			  :kind "success"
			  :message "entity modified successfully") ""))

(make-handler 'Ajax-Entity-Handler)


;--------------------
; RUN TEST-CASE
;--------------------
(defclass Ajax-Run-Handler(Post-Handler)
  ((name
    :initform "ajax-run-handler")
   (entries
    :initform nil)))

(defmethod action((handler Ajax-Run-Handler))
  (with-slots (entries) handler
    (let ( (msg "") )
      (loop for entry in entries
	 do (setf msg (format nil "~a~a~a ==> ~a" 
			      msg #\Newline (first entry) (second entry))))
      (display (make-instance 'Notification-Widget 
			    :kind "success"
			    :message msg) ""))))

(defmethod run-code ((handler Ajax-Run-Handler) codestr)
  "Run LISP code that is given in form of string an return the result of the evaluation of the code as a string or the error of the evaluation as a string if the evaluation fails"
  (with-input-from-string (s codestr)
    (loop for expr = (read s nil) 
       while expr collect (list expr
				(format nil "~a"
					(handler-case (eval expr)
					  (error (e) e)))))))

(defmethod parse-post-param((handler Ajax-Run-Handler) post)
  (with-slots (entries) handler
    (loop for entry in post
       do (progn
	    (when (string-equal (car entry) "code")
	      (setf entries (run-code handler (cdr entry))))))))

(make-handler 'Ajax-Run-Handler)

;--------------------
; OPTIONS
;--------------------
(defclass Option-Handler(Post-Handler)
  ((name :initform "option-handler"))
  (:documentation "Handles the user options modifications"))

(defmethod action((handler Option-Handler))
  (redirect "?admin"))

(defmethod parse-post-param((handler Option-Handler) post)
  (let ( (options (get-user-options)) )
    (reset-options options)
    (loop for entry in post
       when (string-equal (car entry) "option")
       do (set-option options (cdr entry) T))))

(make-handler 'Option-Handler)

;--------------------
; ADD-TAG
;--------------------
(defclass Tag-Handler(Post-Handler)
  ((name :initform "tag-handler"))
  (:documentation "Handles the action of adding a new tag in the system"))

(defmethod action((handler Tag-Handler))
  (redirect "?admin"))

(defmethod parse-post-param((handler Tag-Handler) post)
  (let ( (tag (get-post-parameter "tag"))
	 (parent (get-post-parameter "parent")) )
    (when (and tag parent)
      (add-tag tag parent))))

(make-handler 'Tag-Handler)

;--------------------
; ADD-TAG (ajax)
;--------------------
(defclass Ajax-Tag-Handler(Tag-Handler)
  ((name :initform "ajax-tag-handler"))
  (:documentation "Ajax version of the Tag-Handler that reply with a notification"))

(defmethod action((handler Ajax-Tag-Handler))
  (display (make-instance 'Notification-Widget 
			  :kind "success"
			  :message "tag added successfully") ""))

(make-handler 'Ajax-Tag-Handler)


;--------------------
; TUTORIAL-HANDLER
;--------------------
(defclass Tutorial-Handler(Post-Handler)
  ((name :initform "tutorial-handler")))

(defmethod action((handler Tutorial-Handler))
  (redirect "?admin"))

(defmethod parse-post-param((handler Tutorial-Handler) post)
  (let ( (data nil) (tuto nil) (sections nil) )
    (loop for entry in post
       do (progn
	    (when (string-equal (car entry) "tutorial-name")
	      (setf tuto (make-tutorial-object (cdr entry))))
	    (when (or (string-equal (car entry) "name")
		      (string-equal (car entry) "ref")
		      (string-equal (car entry) "comment"))
	      (setf data (add data (cdr entry))))
	    (when (= 3 (length data))
	      (setf sections (add sections 
				  (apply 'make-documentation-entry data)))
	      (setf data nil))))
    (when (and tuto sections)
      (save-metadata tuto sections))))


(make-handler 'Tutorial-Handler)
