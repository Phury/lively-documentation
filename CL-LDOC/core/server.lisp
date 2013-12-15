(in-package :ldoc)

;;; configurable variables
(defparameter *server-static-dir* nil)
(defparameter *server-static-uri* nil)
(defparameter *server-media-dir* nil)
(defparameter *server-tpl-dir* nil)
(defparameter *server-url* nil)
(defparameter *server-port* nil)
(defparameter *documentation-dir* nil)
(defparameter *installed-app* nil) 
	
;;; Server variables
(defparameter *default-package* nil) 
(defparameter *server-thread* nil) 	

(defun get-default-package()
  *default-package*)

(defun make-url (root place)
  (make-pathname :defaults (format nil "~a~a" root place)))


(defun redirect(url)
  "redirect the handler to the url that will be sent to the web-browser."
  (hunchentoot:redirect url))

(defun get-query-string()
  "return the query-string of the url"
  (hunchentoot:query-string))

(defun get-post-parameters()
  "Return an alist of POST parameters. See hunchentoot:post-parameters for more information"
  (hunchentoot:post-parameters))

(defun get-post-parameter(key)
  "Return the value associated to the parameter key in the POST parameters"
  (hunchentoot:post-parameter key))


(defun add-dispatcher (name handler)
  (push (hunchentoot:create-regex-dispatcher 
	 (format nil "^/~a/$" (str name))
	 handler)
	hunchentoot:*dispatch-table*))

(defmacro def-url-handler (name &body body)
  `(defun name() ,@body)
  `(add-dispatcher ',name))

(defun load-and-eval-file(path)
  "Load and evaluate the content of a lisp written file whose path is given in parameter"
  (when (string-equal (pathname-type path) "lisp")
    ;(print path)
    (let* ( (in (open path :if-does-not-exist nil))
	   (entries nil) )
      (when in 
	(loop for config = (read in nil) 
	   while config do (eval config))
	(close in)))))

(defun load-config ()
  "Load the configuration file, initialize template path and server dispatch table"
  (load-and-eval-file "config.lisp")
  (setq html-template:*default-template-pathname* 
	(make-url *server-static-dir* *server-tpl-dir*))
  (setq hunchentoot:*dispatch-table*
	(list (hunchentoot:create-regex-dispatcher "^/$" 'page-changed)
	      (hunchentoot:create-folder-dispatcher-and-handler 
	       *server-media-dir*
	       (make-url *server-static-dir* *server-media-dir*)))))

(defun load-extensions ()
  "After configuration is initialized, load-extensions start loding extensions"
  (let ( (extensions (directory (make-pathname 
				 :name :wild :type :wild 
				 :defaults "extensions/"))) )
    (loop for dir in extensions
       do (let ( (plugin (make-url dir "/plugin.lisp")) )
	    (load-and-eval-file plugin)))))

(defun load-app ()
  "After extensions are initialized, load-app start loading the application folder"
  (let ( (app (directory (make-pathname 
			  :name :wild :type :wild 
			  :defaults (make-url *server-static-dir* "/")))) )
    (loop for file in app
       do (load-and-eval-file file))))

(defun configure ()
  "Autoconfigure the server variables and initialize all elements to start the documentation server"
  (load-config)
  (load-extensions)
  (load-app))
	
(defun run-server ()
  (when (not *default-package*)
    (setf *default-package* *package*))
  (setf *server-thread* 
	(sb-thread:make-thread 
	 (lambda () 
	   (hunchentoot:start-server :port *server-port*)))))

(defun stop-server ()
  (hunchentoot:stop-server *server-thread*))