(in-package :cl-user)
(asdf:oos 'asdf:load-op 'closer-mop)
(push #p"/Users/kisame/.ldoc" asdf:*central-registry*)

(defpackage :ldoc-asd (:use :cl :asdf))
(in-package :ldoc-asd)

(asdf:defsystem :ldoc
  :name "LDOC"
  :version "2009-03"
  :licence "GPLv3"
  :description "Lively documentation system"
  :depends-on (:hunchentoot :html-template :closer-mop)
  :serial t
  :components
  ((:module "core"
	    :serial t
	    :components
	    ((:file "packages")
	     (:file "util")
	     (:file "mop")
	     (:file "query")
	     (:file "view")
	     (:file "server")
	     (:file "metadata")))))

#+eval (asdf:operate 'asdf:load-op 'ldoc)