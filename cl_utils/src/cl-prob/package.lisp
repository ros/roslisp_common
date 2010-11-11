(defpackage :cl-probability
  (:nicknames :cl-prob)
  (:use :cl :cl-utils)
  (:export 

   ;; Generic ops
   :probability :condition-on-event :sample :expectation

   ;; Specific distribution types

   :normalize-alist!

   :sample-exponential))

   

   
