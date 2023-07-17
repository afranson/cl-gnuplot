;;;; cl-gnuplot.asd

(asdf:defsystem #:cl-gnuplot
  :description "Gnuplot wrapper"
  :author "Andrew Franson, Ph.D."
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop)
  :components ((:file "package")
               (:file "cl-gnuplot")))

(asdf:defsystem #:cl-gnuplot/test
  :description "Gnuplot wrapper tests"
  :author "Andrew Franson, Ph.D."
  :license "GPLv3"
  :depends-on (#:cl-gnuplot
	       #:fiveam)
  :components ((:module "test"
		:serial t
		:components ((:file "package")
			     (:file "tests")))))
