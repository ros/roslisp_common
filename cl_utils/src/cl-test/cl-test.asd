;;;; -*- Mode: LISP -*-

(in-package :asdf)

(defsystem :cl-test :components ((:file "cl-test")) :depends-on ("cl-utils"))
	    