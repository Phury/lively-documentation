(in-package :ldoc)

; TABLE
;******************************************************************************
(defclass Table-Widget(Widget)
  ((template
   :initform #P"widget/table.html")
   (name
    :initform "table")
   (title
    :initform nil
    :initarg :title
    :accessor title)
   (header
    :initform nil
    :accessor header)
   (content
    :initform nil
    :accessor content)))

(defmethod add-column ((table Table-Widget) name)
  (with-slots (header) table
    (setf header (add header (list :name name))))
  (update-widget table))

(defmethod add-row ((table Table-Widget) &rest elements)
  (with-slots (content) table
    (setf content (add content elements)))
  (update-widget table))

(defmethod update-widget ((table Table-Widget))
  (with-slots (model title header content) table
    (when content
      (setf model (list :title title
			:header header
			:content content)))))


; LIST
;******************************************************************************
(defclass List-Widget(Widget)
  ((template
    :initform #P"widget/list.html")
   (name
    :initform "list")
   (title
    :initform nil
    :initarg :title
    :accessor title)
   (items
    :initform nil
    :accessor items)))

(defmethod add-item ((list List-Widget) item)
  (with-slots (items) list
    (setf items (add items item)))
  (update-widget list))

(defmethod update-widget ((list List-Widget))
  (with-slots (model title items) list
    (setf model (list :title title
		      :items items))))

; Notification
;******************************************************************************
(defclass Notification-Widget(Widget)
  ((template
    :initform #P"widget/notification.html")
   (name
    :initform "notification")
   (kind
    :initform (error "Must supply a notification kind")
    :initarg :kind
    :accessor kind)
   (message
    :initform (error "Must supply a notification message")
    :initarg :message
    :accessor message)))

(defmethod initialize-instance :after ((widget Notification-Widget) &key)
  (update-widget widget))

(defmethod update-widget ((widget Notification-Widget))
  (with-slots (model kind message) widget
    (setf model (list :kind kind :message message))))

; TASK
;******************************************************************************
(defclass Todo-Widget(Widget)
  ((template
    :initform #P"widget/tasks.html")
   (name
    :initform "task")
   (title
    :initform nil
    :initarg :title
    :accessor title)
   (items
    :initform nil
    :accessor items)))

(defmethod add-task-item ((widget Todo-Widget) item task)
  (with-slots (items) widget
    (setf items (add items (append item (list :task task)))))
  (update-widget widget))

(defmethod update-widget ((widget Todo-Widget))
  (with-slots (model title items) widget
    (setf model (list :title title
		      :items items))))

; TREE
;******************************************************************************


(defclass Tree-Node-Widget(Widget)
  ((template
    :initform #P"widget/tree.html")
   (name
    :initform nil
    :initarg :name)
   (children
    :initform nil)))

(defmethod add-child-node((widget Tree-Node-Widget) name)
  (with-slots (children) widget
    (let ( (wnode (make-instance 'Tree-Node-Widget :name name)) )
      (update-widget wnode)
      (setf children
	    (add children wnode))))
  (update-widget widget))
  
(defmethod add-node-to-children((widget Tree-Node-Widget) nodename parent)
  (with-slots (name children) widget
    (if (string-equal name parent)
	(add-child-node widget nodename)
	(loop for child in children
	   do (add-node-to-children child nodename parent)))))

(defmethod update-widget((widget Tree-Node-Widget))
  (with-slots (model name children) widget
    (if children
	(progn
	  (let ( (wchildren (loop for child in children
			       collect (call-widget child :node))) )
	    (setf model (list :name name :nodes wchildren))))
	(setf model (list :name name)))))

(defmethod call-widget ((widget Tree-Node-Widget) keyword)
  "Formats the model of the widget correctly for a TMPL_CALL in a web-template."
  (call-next-method))


(defclass Tree-Widget(Widget)
  ((template
    :initform #P"widget/tree.html")
   (name
    :initform "tree")
   (root
    :initform nil)))

; change to add-node-item
(defmethod add-tree-item((widget Tree-Widget) name &optional parent)
  (with-slots (root) widget
    (cond ((not root)
	   (setf root (make-instance 'Tree-Node-Widget :name name)))
	  ((not parent)
	   (add-child-node root name))
	  ((add-node-to-children root name parent)))
    (update-widget root))
  (update-widget widget))

(defmethod update-widget ((widget Tree-Widget))
  (with-slots (model root) widget
    (let ( (tmp-model (call-widget root :nodes)) )
      (setf tmp-model (first (second tmp-model)))
      (pop tmp-model)
      (setf model tmp-model))))

; TAG-CLOUD
;******************************************************************************
(defclass Tag-Cloud-Widget(Widget)
  ((template
    :initform #P"widget/cloud.html")
   (name
    :initform "cloud")
   (title
    :initform nil
    :initarg :title
    :accessor title)
   (tags
    :initform nil)))

(defmethod add-tag-item ((widget Tag-Cloud-Widget) tag count)
  (with-slots (tags) widget
    (setf tags (add tags (list :tag tag :count count))))
  (update-widget widget))

(defmethod update-widget ((widget Tag-Cloud-Widget))
  (with-slots (model title tags) widget
    (setf model (list :title title
		      :tags tags))))

; EDITOR
;******************************************************************************
(defclass Editor-Widget(Widget)
  ((template
    :initform #P"widget/editor.html")
   (name
    :initform "editor")
   (editable
    :initform nil)
   (types
    :initform nil)))

(defmethod initialize-instance :after ((editor Editor-Widget) &key)
  (with-slots (types) editor
    (setf types (loop for type in (get-meta-types)
		   collect (list :type type)))))

(defmethod set-editable ((editor Editor-Widget) name type value)
  (with-slots (editable) editor
    (let ( (parsed (parse-type type value)) )
      (if (string-equal parsed value)
	  (setf editable (list :name name
			       :type type
			       :value value))
	  (setf editable (list :name name
			       :type type
			       :value value
			       :parsed-value parsed)))))
  (update-widget editor))

(defmethod update-widget ((editor Editor-Widget))
  (with-slots (model editable types) editor
    (setf model (append editable
			(list :all-types types)))))
; TAG-EDITOR
;******************************************************************************
(defclass Tag-Editor-Widget(Widget)
  ((template
    :initform #P"widget/tag-editor.html")
   (name
    :initform "editor")
   (tags
    :initform nil)
   (all-tags
    :initform nil)))

(defmethod initialize-instance :after ((widget Tag-Editor-Widget) &key)
  (with-slots (all-tags) widget
    (setf all-tags (get-meta-tags))
    (setf all-tags (loop for tag in all-tags
		      when (not (or (is-root-node tag) (is-leaf-node tag) ))
		      collect (as-list tag))))
  (update-widget widget))

(defmethod add-item ((widget Tag-Editor-Widget) tag)
  (with-slots (tags) widget
    (setf tags (add tags (list :tag tag))))
  (update-widget widget))

(defmethod update-widget ((widget Tag-Editor-Widget))
  (with-slots (model tags all-tags) widget
    (setf model (list :tags tags
		      :all-tags all-tags))))

; ENTITY
;******************************************************************************
(defclass Entity-Widget(Widget)
  ((template
    :initform #P"widget/entity.html")
   (name
    :initform "entity")
   (element
    :initform nil
    :accessor element)
   (editors
    :initform nil)
   (tag-editor
    :initform nil)))

(defmethod set-entity((widget Entity-Widget) entity)
  (with-slots (element editors tag-editor) widget
    (setf element entity)
    (setf tag-editor (make-instance 'Tag-Editor-Widget))
    (loop for tag in (get-tag-entries entity)
       do (add-item tag-editor tag))
    (let ( (editor nil) )
      (loop for entry in (get-metadata-entries entity)
	 do (progn
	      (setf editor (make-instance 'Editor-Widget))
	      (set-editable editor (name entry) (meta entry) (value entry))
	      (setf editors (add editors editor))))))
  (update-widget widget))

(defmethod update-widget ((widget Entity-Widget))
  (with-slots (model element editors tag-editor) widget
    (let ( (documentation nil) )
      (loop for editor in editors
	 do (setf documentation 
		  (add documentation 
		       (call-widget editor :editor))))
    (setf model (append (as-list element)
			(list :documentation documentation)
			(call-widget tag-editor :tag-editor))))))

   