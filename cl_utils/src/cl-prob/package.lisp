(defpackage :cl-probability
  (:nicknames :cl-prob)
  (:use :cl :cl-utils :extended-reals)
  (:export 

   ;; Generic ops
   :probability :condition-on-event :sample :expectation

   ;; Events
   :interval

   ;; Specific distribution types
   :normalize-alist!
   :exponential :sample-exponential :exponential-cdf))

   

   
