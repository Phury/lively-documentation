;;; NOT USED, SHOULD BE IN NEXT VERSION


;; Define how to find all packages of the system
(defclass All-Packages(Generator)
  ((name :initform "all-packages"))
  (:documentation "produce a list of all packages of the system"))

(defmethod generate((gen All-Packages))
  (let ( (output (loop for pkg in (list-all-packages)
		    collect (make-wrapper nil pkg))) )
    (sort output (lambda(a b) (string-lessp (get-name a) (get-name b))))))

(define-query 'All-Packages)

;; Define how to find a package
(defclass Package-Sieve(Sieve)
  ((name :initform "package-name")
   (input :initform "all-packages"))
  (:documentation "filter a package list to return a single package with a given name"))

(defmethod produce((sieve Package-Sieve) input)
  (loop for elt in input
     when (string-equal name (get-name elt))
     do (return elt)))

(define-query 'Package-Sieve)

;; Define how to find content of a package
(defclass Package-Content-Sieve(Sieve)
  ((name :initform "package")
   (input :initform "package-name"))
  (:documentation "filter the content of a package to return all its elements"))

(defmethod produce((sieve Package-Content-Sieve) input)
  (let ( (symbols nil)
	 (package (wrapped input)) )
    (do-symbols (s package)
      (if (is-in-package s package)
	  (push (make-wrapper s (get-object s) input)
		symbols)))
    (sort symbols (lambda(a b) (string-lessp (get-name a) (get-name b))))))

(define-query 'Package-Content-Sieve)

;; Define how to find an object of type desired from elements in a package
(defclass Type-Sieve(Sieve)
  ((name :initform "type")
   (input :initform "package"))
  (:documentation "filter a list of elements to return all elements of a given type"))