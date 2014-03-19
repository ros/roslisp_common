(in-package :actionlib)

(defmacro make-action-goal-msg (client &body args)
  `(make-message (action-goal-type (action-type ,client))
                 ,@args))

(defun str-has-suffix (str suffix)
  (and (> (length str) (length suffix))
       (equal (subseq str (- (length str) (length suffix)))
              suffix)))

(defun make-action-topic (a suffix)
  (concatenate 'string a "/" suffix))

(defun make-action-type (a suffix)
  (assert (str-has-suffix a "Action")
          nil
          "The action type is invalid. Actions always have the suffix 'Action'")
  (concatenate 'string a suffix))

(defun action-msg-type (a suffix)
  (assert (str-has-suffix a "Action")
          nil
          "The action type is invalid. Actions always have the suffix 'Action'")
  (concatenate 'string
               (subseq a 0 (- (length a) (length "Action")))
               suffix))

(defun action-goal-type (a)
  (action-msg-type a "Goal"))
