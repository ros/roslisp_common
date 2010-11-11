(in-package :cl-prob)

(defgeneric probability (dist event)
  (:documentation "Return probability of event under a distribution.  How events are represented depends on the distribution type.  For example, for alist distributions, an event is a boolean function."))

(defgeneric condition-on-event (dist event &key return-type)
  (:documentation "Condition distribution on event.  Return-type might have meaning for particular distribution types, but can always be omitted."))

(defgeneric sample (dist)
  (:documentation "Generate a sample from the distribution, using the current value of *random-state*"))

(defgeneric expectation (dist rv)
  (:documentation "Expectation of a random variable wrt a distribution"))