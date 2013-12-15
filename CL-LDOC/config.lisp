; The root directory for the application
(setf *server-static-dir* "/Users/kisame/Documents/SINF22/Memoire/code/lisp/CL-LDOC/ldoc")

;(defparameter *server-js-dir* "/js/")
;(defparameter *server-css-dir* "/css/")

(setf *documentation-path* "/documentation/") ; Location of the generated documentation
(setf *server-static-uri* "/files/") ; Location of the static files served by the server
(setf *server-media-dir* "/media/") ; Location of the media files directory (like css and javascript)
(setf *server-tpl-dir* "/tpl/") ; Location of the templates directory
(setf *server-data-dir* "/data/") ; Location for the data files (like database files)
(setf *server-url* "") ; URL of the server, leave blanc for "localhost"
(setf *server-port* 8080) ; Port of the server
	
;(setf *documentation-dir* "/DOC/") ; Location of the documenation files in the application