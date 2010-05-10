(defpackage :extended-reals
  (:use :cl)
  (:export
   :extended-real-arithmetic-error 
   :extended-real :extended-float
   :infinity :-infinity
   :e+ :e- :e* :e/
   :emax :emin :e> :e< :e>= :e<= :argmin :argmax)
  (:documentation
   "Arithmetic on extended real numbers (reals with +- infinity)."))