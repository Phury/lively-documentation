(in-package :ldoc)


(defparameter *pages* (make-dictionary 'string-equal))
(defparameter *menu-pages* (make-dictionary 'string-equal))

(defun get-menu-pages()
  "Return a list of page names of all the pages that are listed in the menu"
  (key-set *menu-pages*))

(defun find-page (name)
  "Return a page from the documentation system which name is given in parameter if it exists"
  (get-value *pages* name))

; USER OPTIONS
;******************************************************************************
(defclass User-Options(Ldoc-Object)
  ((name :initform "user-options")
   (option-list :initform nil :accessor option-list))
  (:documentation "Define the options that the user can enable or disable in the documentation system"))

(defmethod add-option ((options User-Options) keyword value description)
  (with-slots (option-list) options
    (setf option-list (add option-list
			   (list :name keyword 
				 :value value
				 :description description)))))

(defmethod set-option ((options User-Options) keyword value)
  (loop for option in (option-list options)
     when (string-equal (getf option :name) keyword)
     do (return (setf (getf option :value) value))))

(defmethod reset-options ((options User-Options))
  (loop for option in (option-list options)
     do (setf (getf option :value) nil)))
			  
(defmethod set-options ((options User-Options) widget)
  "Add the options to the model of the widget"
  (with-slots (model) widget
    (loop for option in (option-list options)
       when (getf option :value)
       do (setf model (append (list (getf option :name) T) model)))))

(defmethod as-list ((options User-Options))
  (let ( (result nil) )
    (loop for option in (option-list options)
       do (setf result (add result option)))
    (list :options result)))

(defparameter *user-options* (make-instance 'User-Options))
(add-option *user-options* :edit nil "Enable edition")
(add-option *user-options* :code-run nil "Enable code running")
(add-option *user-options* :parse nil "Show entries parsed")


(defun get-user-options()
  "Return the user options for the documentation system"
  *user-options*)

; WIDGET
;******************************************************************************
(defclass Widget(Ldoc-Object)
  ((name
    :initform (error "Must supply widget name"))
   (template
    :initarg :template
    :initform (error "Must supply widget template"))
   (model
    :initform nil
    :reader get-model))
  (:documentation "Widget is the base class for all elements that are displayed as an HTML page on the client."))

(defmethod display ((widget Widget) uri)
  (with-slots (template model) widget
    (let ((html-template:*string-modifier* #'identity))
      (with-output-to-string (stream)
	(html-template:fill-and-print-template
	 template
	 model
	 :stream stream)))))

(defmethod update-widget ((widget Widget))
  "Method that update the representation of the widget. The representation is stored in instance variable model and should be updated with this method each time the widget changes")

(defmethod update-widget :after ((widget Widget))
  "Add some data to the representation of the Widget like User options"
  (set-options (get-user-options) widget))
  
(defmethod call-widget ((widget Widget) keyword)
  "Formats the model of the widget correctly for a TMPL_CALL in a web-template. Refer to the HTML-TEMPLATE documentation for more information."
  (with-slots (model template) widget
    (list keyword (list (append (list template) model)))))

(defmethod notify-uri-changed((w Widget) uri)
  "Notify the widget that a change happened. This method will typically implement the actions to do with the uri receiven from the web-page."
  (error "NOTIFY-URI-CHANGE: Must be implemented by subclass"))

(defun call-widgets (keyword &rest widgets)
  "Formats the model of all the widgets correctly for a TMPL_CALL in a web-template. Refer to the HTML-TEMPLATE documentation for more information."
  (let ( (result nil) )
    (loop for elt in widgets
       do (with-slots (template model) elt
	    (push (append (list template) model) result)))
    (append (list keyword) (list result))))

; PAGE-HANDLERS
;******************************************************************************
(defclass Page-Handler(Widget)
  ((template :initform nil))
  (:documentation "Handles actions coming from the user like posting or sending values etc."))

(defmethod action((handler Page-Handler))
  "Define the action that the page handler will perform"
  (error "ACTION: Must be implemented by subclass"))

(defclass Post-Handler(Page-Handler)
  () (:documentation "Handles the actions when the user post data."))

(defmethod notify-uri-changed ((handler Post-Handler) uri)
  (parse-post-param handler (get-post-parameters))
  (action handler))

(defmethod parse-post-param ((handler Post-Handler) post)
  "Handling of the post parameters receiven from a web-page. This function must be implemented by a subclass"
  (error "PARSE-POST-PARAM: Must be implemented by subclass"))

(defun make-handler (class &optional name)
  "Create a new handler and add it to the system"
  (let ( (handler (if name 
		      (make-instance class :name name)
		      (make-instance class)))
	(key nil) )
    (with-slots (name) handler
      (setf key name))
    (add-entry *pages* key handler)
    handler))

; MENU
;******************************************************************************
(defclass Menu-Widget(List-Widget)
  ((template
    :initform #P"widget/menu.html")
   (name
    :initform "menu")
   (title
    :initform "Menu"
    :initarg :title
    :accessor title)
   (items
    :initform nil
    :accessor items)))

(defmethod add-item ((widget Menu-Widget) item)
  (with-slots (items) widget
    (setf items (add items (list :name item))))
  (update-widget widget))

(defmethod set-active ((widget Menu-Widget) item)
  (with-slots (items) widget
    (setf items (loop for elt in items
		   collect (if (string-equal item (second elt))
			       (append elt (list :active "active"))
			       elt))))
  (update-widget widget))

(defmethod update-widget ((widget Menu-Widget))
  (with-slots (model title items active) widget
    (setf model (list :title title
		      :items items))))
  
; PAGES
;******************************************************************************
(defclass Page(Widget)
  ((name :initform (error "Must supply page name"))
   (menu :initform nil
	 :accessor menu)
   (listable :initform nil
	     :accessor listable))
  (:documentation "Base class to create pages that will be displayed to the client. Pages are defined with an html template and can use several widgets to be build. Pages uses a model to store their representation."))

(defmethod notify-uri-changed ((page Page) uri)
  (update-model page uri)
  (display page uri))

(defmethod update-model :after ((page Page) uri)
  (with-slots (model menu) page
    (setf menu (make-instance 'Menu-Widget))
    (loop for name in (get-menu-pages)
       do (add-item menu name))
    (set-active menu (get-name page))
    (setf model (append model (call-widget menu :menu)))))

(defmethod call-model ((page Page) template keyword)
  (with-slots (model) page
    (list keyword (list (append (list template) model)))))

(defun make-page (class name template)
  (let ( (page (make-instance class 
			      :name name
			      :template template)) )
    (add-entry *pages* name page)
    (when (listable page)
      (add-entry *menu-pages* (get-name page) page)) 
    page))

; SERVER PAGE HANDLER
;******************************************************************************
(defun call-uri-page (uristr)
  (let* ( (uri (string-split uristr))
	  (page (find-page (car uri))) )
    (when page
      (notify-uri-changed page uri))))

(defun page-changed()
  (if (get-query-string)
      (call-uri-page (get-query-string))
      (call-uri-page "home")))
