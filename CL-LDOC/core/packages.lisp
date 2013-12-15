(in-package :ldoc-asd)

(defpackage :ldoc
  	(:use :cl :closer-mop :sb-thread)
	;; mopi
	(:export
	 mopimetaobject
	 get-symbol
	 get-name
	 get-package
	 get-uri)
	;; query
	(:export
	 query
	 defquery)
	;; web
	(:export
	 start-doc-server
	 stop-doc-server))
	;; db
	;(:export
	; make-documentation-type
	; add-documentation
	; get-documentation))